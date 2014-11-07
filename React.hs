{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
import           Data.Functor.Identity
import           Data.Monoid
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.HashMap.Strict (HashMap)
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.DOM.Types (Element)
import           GHCJS.Foreign
import           Data.String (IsString (..))
import           Control.Monad
import           Data.Text (Text)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Vector (Vector)
import           Control.Applicative
import qualified Data.Vector as V
import           Data.Aeson
import           GHC.Generics (Generic)
import qualified Data.Text as T
import           Control.Concurrent.STM

-- | A virtual react element.
data ReactElement_
type ReactElement' = JSRef ReactElement_

-- | Not sure what's in here. Perhaps click coordinates and things like that.
data ReactEvent_
type ReactEvent = JSRef ReactEvent_

-- Used for children of a react element. Can be just a string, or list
-- of strings, ReactElement, or array of ReactElement, probably other
-- things too.
data ReactNode_
type ReactNode' = JSRef ReactNode_

instance IsString ReactNode' where
    fromString = (castRef :: JSString -> ReactNode') . fromString

-- | First argument is the virtual element, latter is a real DOM
-- element serving as the container.
foreign import javascript "React.render($1, $2)"
    js_React_render :: ReactElement' -> Element -> IO ()

-- Create a sort of sum type of various things that React will accept
-- as a "node".
--
-- Can be just a string, or list of strings, ReactElement, or array of
-- ReactElement, probably other things too.
class ToReactNode a where
    toReactNode :: a -> IO ReactNode'
instance ToReactNode JSString where
    toReactNode = return . castRef
instance ToReactNode Text where
    toReactNode = return . castRef . toJSString
instance ToReactNode ReactElement' where
    toReactNode = return . castRef
instance ToReactNode ReactNode' where
    toReactNode = return . castRef
instance ToReactNode a => ToReactNode [a] where
    toReactNode = fmap castRef . toJSRef <=< mapM toReactNode
instance ToReactNode a => ToReactNode (Vector a) where
    toReactNode = fmap castRef . toJSRef . V.toList <=< V.mapM toReactNode

-- | Create a virtual ReactElement which can be rendered efficiently.
foreign import javascript "React.createElement($1, $2, $3)"
    js_React_createElement
        :: JSString
        -> JSRef props
        -> ReactNode'
        -> IO ReactElement'

foreign import javascript "document.getElementById($1)"
    getElementById :: JSString -> IO Element

-- | Separated because different types of properties will have to be
-- handled specially just before converting to JS.
data ElemProps = ElemProps
    { epStyle :: Map Text Text
    , epEvents :: Map Text (ReactEvent -> IO ())
    , epOtherProps :: Map Text Text
    -- ^ Cannot be: style, key
    }
    deriving Generic

-- | Used only in the ‘DSL’ to create a pure tree of elements.
data ReactElement =
  ReactElement {elemName :: Text                   -- ^ Name.
               ,elemProps :: ElemProps             -- ^ Properties: style, events, generic.
               ,elemChildren :: (Vector ReactNode) -- ^ Children.
               }

-- | Also used for the DSL AFAICT, not produced directly, constructed
-- via smart constructors.
data ReactNode = RNElement ReactElement
               | RNText Text

-- | Used in the DSL to conveniently create a react element ready for
-- conversion to a JS ReactElement.
reactElement :: Text -> ElemProps -> Vector ReactNode -> ReactElement
reactElement name props children = ReactElement
    name
    props
    children

-- | DSL smart-constructor.
nodeElement :: ReactElement -> ReactNode
nodeElement = RNElement

-- | DSL smart-constructor. Makes a hash of the text ahead of time.
nodeText :: Text -> ReactNode
nodeText t = RNText t

-- | I believe this is used in 'toReactElem'.
instance ToReactNode ReactNode where
    toReactNode (RNElement re) = toReactNode =<< toReactElem re
    toReactNode (RNText t) = toReactNode t

-- | Convert our DSL 'ReactElement' to a JS element.
toReactElem :: ReactElement -> IO ReactElement'
toReactElem (ReactElement name props children) = join $ js_React_createElement
    <$> pure (toJSString name)
    <*> toPropsRef props
    <*> toReactNode children

-- | Create a JS object for the properties.
--
-- * Ensures it creates a \"key\" element for React to use.
-- * Style object is converted to a straight key->string object.
-- * For each of the event handlers it creates a foreign JS wrapper
--   and assigns it in the object.
--
-- Example:
--
-- {\"key\": 123123123,
--  onClick: <function>,
--  style: {textDecoration: \"underline\"}}
--
toPropsRef :: ElemProps -> IO (JSRef props)
toPropsRef (ElemProps style events m) = do
    o <- newObj
    forM_ (Map.toList m) $ \(k, v) -> setProp k (toJSString v) o
    unless (Map.null style) $ do
        style' <- toJSRef_aeson style
        setProp ("style" :: JSString) style' o
    forM_ (Map.toList events) $ \(name, f) -> do
        let name' = T.concat ["on", T.toUpper $ T.take 1 name, T.drop 1 name]
        f' <- syncCallback1 AlwaysRetain True f
        setProp name' f' o
    return o

reactRender :: Element -> ReactElement -> IO ()
reactRender dom re = do
    re' <- toReactElem re
    js_React_render re' dom

-- | This seems to only be used for rendering the state, one instance.
class ToReactElement a where
    toReact :: a -> React a ()

-- | The app state that is rendered to a view.
data MyState = MyState Int Text -- ^ A counter.
    deriving Eq

-- | Our main view.
instance ToReactElement MyState where
  toReact (MyState i txt) =
    do build "p"
             (do style [("textDecoration","underline")]
                 on "click"
                    (const (\var ->
                              atomically
                                (do MyState i txt <- readTVar var
                                    writeTVar var (MyState (i + 1) txt))))
                 "Total clicks: "
                 text (T.pack (show i))
                 ", current value: "
                 text txt)
       build "p" (build "span" "This span should never be modified.")
       build "p"
             (build "input"
                    (do style (if T.null txt
                                  then [("backgroundColor","red")]
                                  else [])
                        attrs [("defaultValue",txt)]
                        on "change"
                           (\e var ->
                              do newTxt <- getVal e
                                 atomically
                                   (modifyTVar
                                      var
                                      (\(MyState x _) ->
                                         MyState x newTxt)))))
       build "p"
             (build "button"
                    (do on "click"
                           (\_ _ ->
                              putStrLn ("Value: " <> T.unpack txt))
                        "Show the value"))

-- | Get event val.
getVal :: ReactEvent -> IO Text
getVal ev =
  do jstr <- getVal_ ev
     return (T.pack (fromJSString jstr))

foreign import javascript "$1.target.value"
    getVal_ :: ReactEvent -> IO JSString

-- | Loop forever. Block on state updates; whenever state changes we
-- do a re-render.
renderThread dom state0 =
  do var <- newTVarIO state0
     let loop stateOld =
           do reactRender dom
                          (unwrap (snd (runIdentity (runReactT "div" var (toReact stateOld)))))
              stateNew <- atomically $
                          do stateNew <- readTVar var
                             check (stateOld /= stateNew)
                             return stateNew
              loop stateNew
     loop state0
  where unwrap (RNElement e) = e

-- | Grab the container element used for rendering into and start the
-- rendering loop.
main :: IO ()
main = do
    container <- getElementById "container"
    renderThread container $ MyState 0 "original"

--------------------------------------------------------------------------------
-- DSL

-- | React transformer.
type ReactT state m = ReaderT (TVar state) (StateT ReactNode m)

-- | Pure react monad.
type React state = ReactT state Identity

instance (a ~ (),Monad m) => IsString (ReaderT (TVar state) (StateT ReactNode m) a) where
  fromString = text . T.pack

-- | Modify the element in the state.
--
-- In the case the current node is text, this is a no-op. Given
-- that the API doesn't have a way to `build' inside a Text,
-- that's okay. We could GADT the ReactNode type to remove this
-- case, but not worth it ATM.
modifyEl :: Monad m => (ReactElement -> ReactElement) -> ReactT state m ()
modifyEl f =
  modify (\rn ->
            case rn of
              RNElement e -> RNElement (f e)
              text -> text)

-- | Modify properties of the element.
modifyProps f =
  modifyEl (\e -> e {elemProps = f (elemProps e)})

-- | Run the react monad.
runReactT :: Text -> TVar state -> ReactT state m a -> m (a,ReactNode)
runReactT name var m = runStateT (runReaderT m var) init
  where init =
          (RNElement (ReactElement name
                                   (ElemProps mempty mempty mempty)
                                   mempty))

--------------------------------------------------------------------------------
-- Public DSL API

-- | Build an element.
build :: Monad m
      => Text -> ReactT state m a -> ReactT state m a
build name m =
  do var <- ask
     (a,child) <- ReaderT (const (StateT (\s ->
                                            do r <- runReactT name var m
                                               return (r,s))))
     modifyEl (\e ->
                 e {elemChildren = elemChildren e <> V.singleton child})
     return a

-- | Add some text to the current node's children.
text :: Monad m
          => Text -> ReactT state m ()
text t =
  modifyEl (\e -> e { elemChildren = elemChildren e <> V.singleton (RNText t)})

-- | Add style. Does not overwrite existing keys.
style :: Monad m => [(Text,Text)] -> ReactT state m ()
style vs =
  modifyProps
    (\ep ->
       ep {epStyle = epStyle ep `Map.union` Map.fromList vs})

-- | Add attributes. Does not overwrite existing keys.
attrs :: Monad m => [(Text,Text)] -> ReactT state m ()
attrs vs =
  modifyProps
    (\ep ->
       ep {epOtherProps = epOtherProps ep `Map.union` Map.fromList vs})

-- | Add event handler. Does not overwrite existing keys.
on :: Monad m => Text -> (ReactEvent -> TVar state -> IO ()) -> ReactT state m ()
on name f =
  do var <- ask
     modifyProps
       (\ep ->
          ep {epEvents =
                epEvents ep `Map.union`
                Map.fromList
                  [(name,\ev -> f ev var)]})
