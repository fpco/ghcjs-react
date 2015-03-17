{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |

module React.Monad where

import           Control.Applicative (Applicative((<*>), pure), (<$>))
import           Control.Concurrent.STM (TVar, check, atomically, readTVar, modifyTVar)
import           Control.Monad (liftM, join, unless, forM_, (<=<))
import           Control.Monad.Reader (ReaderT(..), MonadReader(ask))
import           Control.Monad.State.Strict (MonadState, StateT(..), modify)
import           Data.Coerce (coerce, Coercible)
import           Data.Functor.Identity (Identity)
import           Data.Map (Map)
import qualified Data.Map as M (insert, lookup)
import qualified Data.Map as Map (union, fromList, toList, null)
import           Data.Monoid (Monoid(mempty), (<>))
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T (toUpper, take, pack, drop, concat)
import           Data.Vector (Vector)
import qualified Data.Vector as V (toList, singleton, mapM)
import           GHC.Generics (Generic)
import           GHCJS.Compat
import           Unsafe.Coerce (unsafeCoerce)


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
      ,appCursors :: TVar (Map Int Cursor) -- ^ Mapping from integer to state cursors.
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
