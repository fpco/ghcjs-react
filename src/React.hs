{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module React where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Coerce
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as     M
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           GHC.Generics (Generic)
import           Unsafe.Coerce

#ifdef __GHCJS__
import           JavaScript.JQuery (JQuery)
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.DOM.Types (Element (..), Event (..))
import           GHCJS.Foreign
#endif

-- So I can type check this in GHCi.
#ifndef __GHCJS__
data Event
data JQuery
type JSObject a = JSRef (JSObject_ a)
data JSObject_ a
data JSRef a
data Element
castRef = undefined
toJSString = undefined
data JSString
instance IsString JSString where fromString = undefined
newObj = undefined
setProp = undefined
getProp = undefined
toJSRef_aeson = undefined
fromJSString = undefined
syncCallback1 = undefined
syncCallback2 = undefined
syncCallback = undefined
data U = AlwaysRetain
#endif

#ifndef __GHCJS__
class ToJSRef a where
  toJSRef :: a -> IO (JSRef a)
  toJSRef = undefined
  toJSRefListOf :: [a] -> IO (JSRef [a])
  toJSRefListOf = undefined
class FromJSRef a where
  fromJSRef :: JSRef a -> IO (Maybe a)
  fromJSRef = undefined
  fromJSRefListOf :: JSRef [a] -> IO (Maybe [a])
  fromJSRefListOf = undefined
instance ToJSRef [ReactNode']
instance FromJSRef JQuery
instance FromJSRef Int
instance FromJSRef Text
instance FromJSRef JSString
instance FromJSRef [Char]
instance ToJSRef Int
#endif

-- | A virtual react element.
data ReactElement_
type ReactElement' = JSRef ReactElement_

-- | A React component class.
data ReactClass_
type ReactClass' a = JSRef ReactClass_

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
#ifdef __GHCJS__
foreign import javascript "React.render($1, $2)"
    js_React_render :: ReactElement' -> Element -> IO ()
foreign import javascript "console.log('Properties: %o',$1)"
  console_log :: JSRef a -> IO ()
#else
js_React_render :: ReactElement' -> Element -> IO ()
js_React_render = undefined
console_log :: JSRef a -> IO ()
console_log = undefined
#endif

-- Create a sort of sum type of various things that React will accept
-- as a "node".
--
-- Can be just a string, or list of strings, ReactElement, or array of
-- ReactElement, probably other things too.
class ToReactNode state a where
    toReactNode :: App state m -> a -> IO ReactNode'
instance ToReactNode state JSString where
    toReactNode _ = return . castRef
instance ToReactNode state Text where
    toReactNode _ = return . castRef . toJSString
instance ToReactNode state ReactElement' where
    toReactNode _ = return . castRef
instance ToReactNode state ReactNode' where
    toReactNode _ = return . castRef
instance ToReactNode state a => ToReactNode state [a] where
    toReactNode app = fmap castRef . toJSRef <=< mapM (toReactNode app)
instance ToReactNode state a => ToReactNode state (Vector a) where
    toReactNode app = fmap castRef . toJSRef . V.toList <=< V.mapM (toReactNode app)

-- | Create a virtual ReactElement which can be rendered efficiently.
#ifdef __GHCJS__
foreign import javascript "$1.props.cursor"
  js_React_props_cursor :: JSRef this -> IO Int
foreign import javascript "React.createElement($1, $2, $3)"
    js_React_createElement
        :: JSString
        -> JSRef props
        -> ReactNode'
        -> IO ReactElement'
foreign import javascript "React.createElement($1, $2)"
    js_React_createElementFromClass
        :: ReactClass' state
        -> JSRef props
        -> JSRef (Lens state cursor)
        -> IO ReactElement'
foreign import javascript
    "React.createClass({ render: function(){ var x = {r:0}; $1(x); return x.r; }, componentDidMount: function(){ $2(jQuery(this.getDOMNode()),this); }, componentDidUpdate: function(){return $3(this)}, shouldComponentUpdate: function(){return $4(this)}, componentWillReceiveProps: function(news){return $5(this,news)} })"
    js_React_createClass
        :: JSFun (JSObject ReactElement' -> IO ())
        -> JSFun (JSRef JQuery -> JSRef props -> IO ())
        -> JSFun (JSRef props -> IO ())
        -> JSFun (JSRef props -> IO ())
        -> JSFun (JSRef props -> JSRef props -> IO ())
        -> IO (ReactClass' state)
foreign import javascript "React.DOM.a( {href:'http://venmo.com'}, 'Venmo!!!')"
  dom_a :: IO ReactElement'
#else
js_React_props_cursor :: JSRef this -> IO Int
js_React_props_cursor = undefined
js_React_createElementFromClass
        :: ReactClass' state
        -> JSRef props
        -> JSRef (Lens state cursor)
        -> IO ReactElement'
js_React_createElementFromClass = undefined
dom_a :: IO ReactElement'
dom_a = undefined
js_React_createElement
    :: JSString
    -> JSRef props
    -> ReactNode'
    -> IO ReactElement'
js_React_createElement = undefined
js_React_createClass
    :: (JSObject ReactElement' -> IO ())
    -> (JSRef JQuery -> JSRef props -> IO ())
    -> (JSRef props -> IO ())
    -> (JSRef props -> IO ())
    -> (JSRef props -> JSRef props -> IO ())
    -> IO (ReactClass' state)
js_React_createClass = undefined
#endif

-- | Separated because different types of properties will have to be
-- handled specially just before converting to JS.
data ElemProps = ElemProps
    { epStyle :: Map Text Text
    , epEvents :: Map Text (ReactEvent -> IO ())
    , epOtherProps :: Map Text Text
    -- ^ Cannot be: style, key
    }
    deriving (Generic,Show)

instance Show (ReactEvent -> IO ()) where
    show _ = "<ReactEvent -> IO ()>"

-- | Used only in the ‘DSL’ to create a pure tree of elements.
data ReactElement state =
  ReactElement {elemName :: Text                   -- ^ Name.
               ,elemProps :: ElemProps             -- ^ Properties: style, events, generic.
               ,elemChildren :: (Vector (ReactNode state)) -- ^ Children.
               }
  deriving (Show)

instance Show (ReactClass' a) where
    show _ = "<ReactClass'>"

-- | Used in the DSL for constructing an instance of a component class.
data ReactComponent state = forall cursor.
  ReactComponent {compName :: ReactClass' cursor
                 ,compProps :: ElemProps
                 ,compLens :: Lens state cursor}

deriving instance Show (ReactComponent state)

-- | Also used for the DSL AFAICT, not produced directly, constructed
-- via smart constructors.
data ReactNode state
  = RNElement (ReactElement state)
  | RNComponent (ReactComponent state)
  | RNText Text
  deriving (Show)

-- | I believe this is used in 'toReactElem'.
instance ToReactNode state (ReactNode state) where
    toReactNode app re@RNElement{} = toReactNode app =<< toReactElem app re
    toReactNode app (RNText t) = toReactNode app t
    toReactNode app c@RNComponent{} = toReactNode app =<< toReactElem app c

-- | Convert our DSL 'ReactElement' to a JS element.
toReactElem :: App state m -> ReactNode state -> IO ReactElement'
toReactElem app rn =
  case rn of
    RNElement (ReactElement name props children) ->
      join $
      js_React_createElement <$>
      pure (toJSString name) <*>
      toPropsRef app props Nothing <*>
      toReactNode app children
    RNComponent (ReactComponent cls props lens) ->
      do {-putStrLn ("RNComponent " ++ "<ReactClass'> " ++ show props)-}
         (join $
          js_React_createElementFromClass <$>
          pure cls <*>
          toPropsRef app props (Just lens) <*>
          toJSRef lens)
    RNText t -> error ":-("

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
toPropsRef :: App state m -> ElemProps -> Maybe (Lens state cursor) -> IO (JSRef props)
toPropsRef (App _ _ ints cursors) (ElemProps style events other) mlens = do
    o <- newObj
    forM_ (Map.toList other) $ \(k, v) ->
        setProp k (toJSString v) o
    unless (Map.null style) $ do
        style' <- toJSRef_aeson style
        setProp ("style" :: JSString) style' o
    forM_ (Map.toList events) $ \(name, f) -> do
        let name' = T.concat ["on", T.toUpper $ T.take 1 name, T.drop 1 name]
        f' <- syncCallback1 AlwaysRetain True f
        setProp name' f' o
    case mlens of
      Nothing -> return ()
      Just lens -> do
          cursorId <- genCursor ints cursors (Cursor lens)
          lensr <- toJSRef (cursorId :: Int)
          setProp ("cursor" :: JSString) lensr o
    return o

genCursor :: TVar Int
          -> TVar (HashMap Int Cursor)
          -> Cursor
          -> IO Int
genCursor ints cursors cursor =
  atomically
    (do i <- readTVar ints
        modifyTVar ints (+ 1)
        modifyTVar cursors (M.insert i cursor)
        return i)

reactRender :: App state m -> Element -> ReactNode state -> IO ()
reactRender app dom re =
  do re' <- toReactElem app re
     js_React_render re' dom

-- | Get event val.
getVal :: ReactEvent -> IO Text
getVal ev =
  do jstr <- getVal_ ev
     return (T.pack (fromJSString jstr))

#ifdef __GHCJS__
foreign import javascript "$1.target.value"
    getVal_ :: ReactEvent -> IO JSString
#else
getVal_ :: ReactEvent -> IO JSString
getVal_ = undefined
#endif

-- | Loop forever. Block on state updates; whenever state changes we
-- do a re-render.
renderThread :: (Show state,Eq state)
             => App state m
             -> (state -> ReactT state m ())
             -> Element
             -> IO ()
renderThread app@(App var run _ _) render dom =
  do let loop stateOld =
           do node <- run (runReactT "div" var (render stateOld))
              reactRender app
                          dom
                          (snd node)
              stateNew <- atomically $
                          do stateNew <- readTVar var
                             check (stateOld /= stateNew)
                             return stateNew
              {-putStrLn ("New state: " ++ show stateNew)-}
              loop stateNew
     state0 <- atomically (readTVar var)
     loop state0
  where unwrap (RNElement e) = e

--------------------------------------------------------------------------------
-- DSL

-- | React transformer.
type ReactT state m = ReaderT (TVar state) (StateT (ReactNode state) m)

-- | Pure react monad.
type React state = ReactT state Identity

instance (a ~ (),Monad m) => IsString (ReaderT (TVar state) (StateT (ReactNode state) m) a) where
  fromString = text . T.pack

-- | Modify the element in the state.
--
-- In the case the current node is text, this is a no-op. Given that
-- the API doesn't have a way to `build' inside a Text, that's
-- okay. We could GADT the ReactNode type to remove this case, but not
-- worth it ATM. Also there is now a Component type, but I don't know
-- how to build inside one of those either, so it's also a no-op (for
-- now).
modifyEl :: Monad m => (ReactElement state -> ReactElement state) -> ReactT state m ()
modifyEl f =
  modify (\rn ->
            case rn of
              RNElement e -> RNElement (f e)
              text -> text)

-- | Modify properties of the element.
modifyProps :: MonadState (ReactNode state) m => (ElemProps -> ElemProps) -> m ()
modifyProps f =
  modify (\s ->
            case s of
              RNElement e ->
                RNElement (e {elemProps =
                                f (elemProps e)})
              RNComponent c ->
                RNComponent
                  (c {compProps = f (compProps c)})
              RNText{} -> s)

-- | Run the react monad.
runReactT :: Text -> TVar state -> ReactT state m a -> m (a,ReactNode state)
runReactT name var m = runStateT (runReaderT m var) init
  where init =
          (RNElement (ReactElement name
                                   (ElemProps mempty mempty mempty)
                                   mempty))

-- | A component for some state transforming over some monad.
data Component state cursor (m :: * -> *) =
  Component (ReactClass' cursor)

-- | State cursor.
data Cursor =
  forall cursor state. Cursor (Lens cursor state)

-- | An application with some state transforming over some monad.
data App state m =
  App {appState :: TVar state                  -- ^ State used for rendering.
      ,appRun :: forall a. m a -> IO a         -- ^ Function for running in the particular monad.
      ,appIdSource :: TVar Int                 -- ^ Source of integers.
      ,appCursors :: TVar (HashMap Int Cursor) -- ^ Mapping from integer to state cursors.
      }

-- | Class used for creating components.
data Class state cursor m =
  Class {classApp :: App state m                                                              -- ^ Application.
        ,classRender :: ReactT state m ()                                                     -- ^ Rendering function.
        ,classDidMount :: (forall props. Lens state cursor -> JQuery -> JSRef props -> IO ()) -- ^ Did mount handler.
        ,classDidUpdate :: (forall props. Lens state cursor -> JSRef props -> IO ())          -- ^ Did update.
        ,classShouldUpdate :: (forall props. Lens state cursor -> JSRef props -> IO Bool)     -- ^ Should update?
        ,classReceivingProps :: (forall props. Lens state cursor -> JSRef props -> IO ())     -- ^ Receiving new props.
        }

-- | Create a component class.
createClass :: (Monad m)
            => Class state cursor m
            -> IO (Component state cursor m)
createClass cls =
  do renderFun <- syncCallback1
                    AlwaysRetain
                    True
                    (\elRef ->
                       do el <- appRun (classApp cls)
                                       (liftM snd
                                              (runReactT "div"
                                                         (appState (classApp cls))
                                                         (do attr "data-component" "true"
                                                             (classRender cls))))
                          el' <- toReactElem (classApp cls)
                                             el
                          setProp ("r" :: String) el' elRef)
     didMountFun <- syncCallback2
                      AlwaysRetain
                      True
                      (\jq ref ->
                         do el :: Maybe JQuery <- fromJSRef jq
                            cursor <- js_React_props_cursor ref
                            cs <- atomically (readTVar (appCursors (classApp cls)))
                            case M.lookup cursor cs of
                              Nothing ->
                                error ("Couldn't find cursor: " ++ show cursor)
                              Just (Cursor cursor) ->
                                case unsafeCoerce cursor of
                                  c ->
                                    classDidMount
                                      cls
                                      c
                                      (maybe (error "didMount: Couldn't get jquery element...")
                                             id
                                             el)
                                      ref
                            return ())
     didUpdateFun <- syncCallback1
                       AlwaysRetain
                       True
                       (\ref ->
                          do cursor <- js_React_props_cursor ref
                             cs <- atomically (readTVar (appCursors (classApp cls)))
                             case M.lookup cursor cs of
                               Nothing ->
                                 error ("Couldn't find cursor: " ++ show cursor)
                               Just (Cursor cursor) ->
                                 case unsafeCoerce cursor of
                                   c ->
                                     classDidUpdate cls c ref
                             return ())
     shouldUpdateFun <- syncCallback1
                          AlwaysRetain
                          True
                          (\ref ->
                             do cursor <- js_React_props_cursor ref
                                cs <- atomically (readTVar (appCursors (classApp cls)))
                                case M.lookup cursor cs of
                                  Nothing ->
                                    error ("Couldn't find cursor: " ++
                                           show cursor)
                                  Just (Cursor cursor) ->
                                    case unsafeCoerce cursor of
                                      c ->
                                        classShouldUpdate cls c ref
                                return ())
     willRecPropsFun <- syncCallback2
                          AlwaysRetain
                          True
                          (\ref newprops ->
                             do cursor <- js_React_props_cursor ref
                                cs <- atomically (readTVar (appCursors (classApp cls)))
                                case M.lookup cursor cs of
                                  Nothing ->
                                    error ("Couldn't find cursor: " ++
                                           show cursor)
                                  Just (Cursor cursor) ->
                                    case unsafeCoerce cursor of
                                      c ->
                                        classReceivingProps cls c newprops
                                return ())
     n <- js_React_createClass renderFun didMountFun didUpdateFun shouldUpdateFun willRecPropsFun
     return (Component n)

--------------------------------------------------------------------------------
-- Dumb functional references

-- | A functional reference.
data Ref state cursor get where
    Lens :: (state -> cursor) -> (cursor -> state -> state) -> Lens state cursor
    Prism :: (state -> Maybe cursor) -> (cursor -> state -> state) -> Prism state cursor

-- | A prism, i.e. getting is certain.
type Lens state cursor = Ref state cursor cursor

-- | A prism, i.e. getting is not certain.
type Prism state cursor = Ref state cursor (Maybe cursor)

-- | Put in the functional reference.
putR :: Ref state cursor get -> (cursor -> state -> state)
putR (Lens _ put) = put
putR (Prism _ put) = put

-- | Get from the functional reference.
getR :: Ref state cursor get -> (state -> get)
getR (Lens get _) = get
getR (Prism get _) = get

-- | Modify using a functional reference.
modifyR :: Ref state cursor get -> (cursor -> cursor) -> state -> state
modifyR r f state =
  case r of
    Lens get put ->
        put (f (get state)) state
    Prism get put ->
      case get state of
        Nothing -> state
        Just x -> put (f x) state

-- | Set the state at the given ref.
setAt :: Ref state cursor get -> cursor -> TVar state -> IO ()
setAt ref update var =
  atomically
    (modifyTVar var
                (putR ref update))

-- | Read a value from the state.
readAt :: Ref state cursor get -> TVar state -> IO get
readAt ref var =
  atomically
    (fmap (getR ref)
          (readTVar var))

-- | Modify the state at the given ref.
modifyAt :: Ref state cursor get -> (cursor -> cursor) -> TVar state -> IO ()
modifyAt ref update var =
  atomically
    (modifyTVar var
                (modifyR ref update))

-- | Set the state.
set :: state -> TVar state -> IO ()
set state var =
  atomically
    (modifyTVar var
                (const state))

--------------------------------------------------------------------------------
-- Public DSL API

instance ToJSRef (Lens state cursor) where
    toJSRef = unsafeCoerce

instance FromJSRef (Lens state cursor) where
    fromJSRef = return . Just . unsafeCoerce

instance Show (Lens state cursor) where
  show _ = "<Lens>"

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

-- | Build a component.
buildComponent :: Monad m
               => Component state cursor m -- ^ The component.
               -> Lens state cursor       -- ^ A cursor into the state for this instance.
               -> ReactT state m a         -- ^ Set attributes for the
                                           -- component. Ignores content (for
                                           -- now).
               -> ReactT state m a
buildComponent (Component cls) cursor m =
  do var <- ask
     (a,child) <- ReaderT (const (StateT (\s ->
                                            do r <- runReactT "tmp" var m
                                               return (r,s))))
     -- The above is just used for running attributes. ^
     modifyEl (\e ->
                 e {elemChildren =
                      elemChildren e <>
                      V.singleton
                        (RNComponent
                           (ReactComponent cls
                                           (getProps child)
                                           cursor))})
     return a
  where getProps (RNElement (ReactElement "tmp" props _)) = props
        getProps x = error ("getProps: unexpected case: " ++ show x)

-- | Add some text to the current node's children.
text :: Monad m
     => Text -> ReactT state m ()
text t =
  modifyEl (\e -> e {elemChildren = elemChildren e <> V.singleton (RNText t)})

-- | Add a style.
style :: Monad m => Text -> Text -> ReactT state m ()
style name val = styles [(name,val)]

-- | Add styles. Does not overwrite existing keys.
styles :: Monad m => [(Text,Text)] -> ReactT state m ()
styles vs =
  modifyProps
    (\ep ->
       ep {epStyle = epStyle ep `Map.union` Map.fromList vs})

-- | Add attributes. Does not overwrite existing keys.
attrs :: Monad m => [(Text,Text)] -> ReactT state m ()
attrs vs =
  modifyProps
    (\ep ->
       ep {epOtherProps = epOtherProps ep `Map.union` Map.fromList vs})

-- | Add attributes. Does not overwrite existing keys.
attr :: Monad m => Text -> Text -> ReactT state m ()
attr name prop =
  modifyProps
    (\ep ->
       ep {epOtherProps = epOtherProps ep `Map.union` Map.fromList [(name,prop)]})

-- | Add event handler. Does not overwrite existing keys.
onRaw :: Monad m => Text -> (ReactEvent -> TVar state -> IO ()) -> ReactT state m ()
onRaw name f =
  do var <- ask
     modifyProps
       (\ep ->
          ep {epEvents =
                epEvents ep `Map.union`
                Map.fromList
                  [(name,\ev -> f ev var)]})

-- FIXME Flesh out fully following: http://facebook.github.io/react/docs/events.html
newtype EventListener event = EventListener Text

class (Coercible ReactEvent event, Coercible event ReactEvent) => IsReactEvent event
instance IsReactEvent ReactEvent

onEvent :: (IsReactEvent event, Monad m)
   => EventListener event
   -> (event -> TVar state -> IO ())
   -> ReactT state m ()
onEvent (EventListener name) f = onRaw name $ \re var -> f (coerce re) var

#ifdef __GHCJS__

foreign import javascript "$2[$1]"
    getPropBool :: JSString -> ReactEvent -> IO Bool
foreign import javascript "$2[$1]"
    getPropInt :: JSString -> ReactEvent -> IO Int
foreign import javascript "$2[$1]"
    getPropElement :: JSString -> ReactEvent -> IO Element
foreign import javascript "$2[$1]"
    getPropEvent :: JSString -> ReactEvent -> IO Event
foreign import javascript unsafe "$1[\"preventDefault\"]()"
    js_preventDefault :: ReactEvent -> IO ()
foreign import javascript unsafe "$1[\"stopPropagation\"]()"
    js_stopPropagation :: ReactEvent -> IO ()
foreign import javascript unsafe "$1[\"getModifierState\"]($2)"
    js_getModifierState :: ReactEvent -> JSString -> IO Bool

#else

getPropBool :: JSString -> ReactEvent -> IO Bool
getPropBool = undefined
getPropInt :: JSString -> ReactEvent -> IO Int
getPropInt = undefined
getPropElement :: JSString -> ReactEvent -> IO Element
getPropElement = undefined
getPropEvent :: JSString -> ReactEvent -> IO Event
getPropEvent = undefined
js_preventDefault :: ReactEvent -> IO ()
js_preventDefault = undefined
js_stopPropagation :: ReactEvent -> IO ()
js_stopPropagation = undefined
js_getModifierState :: ReactEvent -> JSString -> IO Bool
js_getModifierState = undefined

#endif

bubbles :: IsReactEvent event => event -> IO Bool
bubbles = getPropBool "bubbles" . coerce

cancelable :: IsReactEvent event => event -> IO Bool
cancelable = getPropBool "cancelable" . coerce

currentTarget :: IsReactEvent event => event -> IO Element
currentTarget = getPropElement "currentTarget" . coerce

defaultPrevented :: IsReactEvent event => event -> IO Bool
defaultPrevented = getPropBool "defaultPrevented" . coerce

eventPhase :: IsReactEvent event => event -> IO Int
eventPhase = getPropInt "eventPhase" . coerce

isTrusted :: IsReactEvent event => event -> IO Bool
isTrusted = getPropBool "isTrusted" . coerce

nativeEvent :: IsReactEvent event => event -> IO Event
nativeEvent = getPropEvent "nativeEvent" . coerce

preventDefault :: IsReactEvent event => event -> IO ()
preventDefault = js_preventDefault . coerce

stopPropagation :: IsReactEvent event => event -> IO ()
stopPropagation = js_stopPropagation . coerce

target :: IsReactEvent event => event -> IO Element
target = getPropElement "target" . coerce

-- FIXME missing timeStamp

eventType :: IsReactEvent event => event -> IO Text
eventType e = fmap fromJSString $ getProp ("type" :: JSString) (coerce e :: ReactEvent)

newtype ClipboardEvent = ClipboardEvent ReactEvent
instance IsReactEvent ClipboardEvent

copy, cut, paste :: EventListener ClipboardEvent
copy = EventListener "copy"
cut = EventListener "cut"
paste = EventListener "paste"

onCopy :: Monad m => (ClipboardEvent -> TVar state -> IO ()) -> ReactT state m ()
onCopy = onEvent copy

onCut :: Monad m => (ClipboardEvent -> TVar state -> IO ()) -> ReactT state m ()
onCut = onEvent cut

onPaste :: Monad m => (ClipboardEvent -> TVar state -> IO ()) -> ReactT state m ()
onPaste = onEvent paste

class IsReactEvent event => IsClipboardEvent event
instance IsClipboardEvent ReactEvent
instance IsClipboardEvent ClipboardEvent

clipboardData :: IsClipboardEvent event => event -> IO (JSRef a) -- FIXME better data type
clipboardData e = getProp ("clipboardData" :: JSString) (coerce e :: ReactEvent)

newtype MouseEvent = MouseEvent ReactEvent
instance IsReactEvent MouseEvent

dblClick,click, mouseMove, mouseOut :: EventListener MouseEvent
click = EventListener "click"
dblClick = EventListener "dblClick"
mouseMove = EventListener "mouseMove"
mouseOut = EventListener "mouseOut"

onClick :: Monad m => (MouseEvent -> TVar state -> IO ()) -> ReactT state m ()
onClick = onEvent click

onDblClick :: Monad m => (MouseEvent -> TVar state -> IO ()) -> ReactT state m ()
onDblClick = onEvent dblClick

onMouseMove :: Monad m => (MouseEvent -> TVar state -> IO ()) -> ReactT state m ()
onMouseMove = onEvent mouseMove

onMouseOut :: Monad m => (MouseEvent -> TVar state -> IO ()) -> ReactT state m ()
onMouseOut = onEvent mouseOut

class IsReactEvent event => IsMouseEvent event
instance IsMouseEvent ReactEvent
instance IsMouseEvent MouseEvent

altKey :: IsMouseEvent event => event -> IO Bool
altKey = getPropBool "altKey" . coerce

button :: IsMouseEvent event => event -> IO Int
button = getPropInt "button" . coerce

buttons :: IsMouseEvent event => event -> IO Int
buttons = getPropInt "buttons" . coerce

clientX :: IsMouseEvent event => event -> IO Int
clientX = getPropInt "clientX" . coerce

clientY :: IsMouseEvent event => event -> IO Int
clientY = getPropInt "clientY" . coerce

ctrlKey :: IsMouseEvent event => event -> IO Bool
ctrlKey = getPropBool "ctrlKey" . coerce

getModifierState :: IsMouseEvent event => event -> Text -> IO Bool
getModifierState e t = js_getModifierState (coerce e) (toJSString t)

metaKey :: IsMouseEvent event => event -> IO Bool
metaKey = getPropBool "metaKey" . coerce

pageX :: IsMouseEvent event => event -> IO Int
pageX = getPropInt "pageX" . coerce

pageY :: IsMouseEvent event => event -> IO Int
pageY = getPropInt "pageY" . coerce

relatedTarget :: IsMouseEvent event => event -> IO Element
relatedTarget = getPropElement "relatedTarget" . coerce

screenX :: IsMouseEvent event => event -> IO Int
screenX = getPropInt "screenX" . coerce

screenY :: IsMouseEvent event => event -> IO Int
screenY = getPropInt "screenY" . coerce

shiftKey :: IsMouseEvent event => event -> IO Bool
shiftKey = getPropBool "shiftKey" . coerce
