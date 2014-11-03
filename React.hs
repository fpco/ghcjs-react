{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM.Types (Element)
import GHCJS.Foreign
import Data.String (IsString (..))
import Control.Monad
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import Control.Applicative
import qualified Data.Vector as V
import Data.Aeson
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.Text as T
import Control.Concurrent.STM

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

foreign import javascript "console.log(\"%o\",$1)"
    js_console_log :: JSRef a -> IO ()

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
    , epEvents :: Map Text (Int, ReactEvent -> IO ())
    , epOtherProps :: Map Text Text
    -- ^ Cannot be: style, key
    }
    deriving Generic
instance Hashable ElemProps where
    hashWithSalt salt (ElemProps x y z) = hashWithSalt salt
        ( Map.toList x
        , Map.toList $ Map.map fst y
        , Map.toList z
        )

-- | Used only in the ‘DSL’ to create a pure tree of elements.
data ReactElement =
  ReactElement Int                -- ^ Hash.
               Text               -- ^ Name.
               ElemProps          -- ^ Properties: style, events, generic.
               (Vector ReactNode) -- ^ Children.

-- | Used in the DSL to conveniently create a react element ready for
-- conversion to a JS ReactElement.
reactElement :: Text -> ElemProps -> Vector ReactNode -> ReactElement
reactElement name props children = ReactElement
    (hash (name, props, V.toList $ V.map rnHash children))
    name
    props
    children

-- | Also used for the DSL AFAICT, not produced directly, constructed
-- via smart constructors.
data ReactNode = RNElement ReactElement
               | RNText Int Text

-- | Used for hashing the children of an element in 'reactElement'.
rnHash :: ReactNode -> Int
rnHash (RNElement (ReactElement h _ _ _)) = h
rnHash (RNText h _ ) = h

-- | DSL smart-constructor.
nodeElement :: ReactElement -> ReactNode
nodeElement = RNElement

-- | DSL smart-constructor. Makes a hash of the text ahead of time.
nodeText :: Text -> ReactNode
nodeText t = RNText (hash t) t

-- | I believe this is used in 'toReactElem'.
instance ToReactNode ReactNode where
    toReactNode (RNElement re) = toReactNode =<< toReactElem re
    toReactNode (RNText _ t) = toReactNode t

-- | Convert our DSL 'ReactElement' to a JS element.
toReactElem :: ReactElement -> IO ReactElement'
toReactElem (ReactElement h name props children) = join $ js_React_createElement
    <$> pure (toJSString name)
    <*> toPropsRef h props
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
toPropsRef :: Int -> ElemProps -> IO (JSRef props)
toPropsRef key (ElemProps style events m) = do
    o <- newObj
    forM_ (Map.toList m) $ \(k, v) -> setProp k (toJSString v) o
    key' <- toJSRef key
    setProp ("key" :: JSString) key' o
    unless (Map.null style) $ do
        style' <- toJSRef_aeson style
        setProp ("style" :: JSString) style' o
    forM_ (Map.toList $ Map.map snd events) $ \(name, f) -> do
        let name' = T.concat ["on", T.toUpper $ T.take 1 name, T.drop 1 name]
        f' <- syncCallback1 AlwaysRetain True f
        setProp name' f' o
    js_console_log o
    return o

foreign import javascript "alert($1)"
    alert :: JSString -> IO ()

reactRender :: Element -> ReactElement -> IO ()
reactRender dom re = do
    re' <- toReactElem re
    js_React_render re' dom

-- | This seems to only be used for rendering the state, one instance.
class ToReactElement a where
    toReactElement :: TVar a -> a -> ReactElement

-- | The app state that is rendered to a view.
data MyState = MyState Int -- ^ A counter.
    deriving Eq

-- | Our main model renderer. Spits out:
--
-- @
-- <div id=\"container\">
-- <i data-reactid=\".0\">
-- <b style=\"text-decoration:underline;\" data-reactid=\".0.$-1372681592\">
-- <span data-reactid=\".0.$-1372681592.0\">Total clicks: 0</span>
-- </b>
-- <span data-reactid=\".0.$1558243823\">
-- <span data-reactid=\".0.$1558243823.0\">This span should never be modified.</span>
-- </span>
-- </i>
-- </div>
-- @
--
-- Where the data-reactid will be changed when elements are changed
-- (judged by their hash).
--
instance ToReactElement MyState where
    toReactElement var (MyState i) = reactElement "i" (ElemProps [] [] [])
        [ nodeElement $ reactElement "b"
            (ElemProps
                [("textDecoration", "underline")]
                (Map.singleton "click" (i, const $ atomically $ do
                    MyState i <- readTVar var
                    writeTVar var $ MyState $ i + 1))
                [])
            [ nodeText $ "Total clicks: " `T.append` T.pack (show i)
            ]
        , nodeElement $ reactElement "span"
            (ElemProps [] [] [])
            [ nodeText "This span should never be modified."
            ]
        ]

-- | Loop forever. Block on state updates; whenever state changes we
-- do a re-render.
renderThread dom state0 = do
    var <- newTVarIO state0
    let loop stateOld = do
            reactRender dom (toReactElement var stateOld)
            stateNew <- atomically $ do
                stateNew <- readTVar var
                check (stateOld /= stateNew)
                return stateNew
            loop stateNew
    loop state0

-- | Grab the container element used for rendering into and start the
-- rendering loop.
main :: IO ()
main = do
    container <- getElementById "container"
    renderThread container $ MyState 0
