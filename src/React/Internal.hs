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

-- | An Om-inspired API for React.js from Haskell.

module React.Internal where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Monad.State.Strict
import           Data.Coerce
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           GHC.Generics
import           GHCJS.Compat
import           React.Ref

#ifdef __GHCJS__
import JavaScript.JQuery (JQuery)
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.Event
import GHCJS.Foreign
#endif

-- | An application with some state transforming over some monad.
data App state m =
  App {appState :: TVar state                  -- ^ State used for rendering.
      ,appRun :: forall a. m a -> IO a         -- ^ Function for running in the particular monad.
      ,appIdSource :: TVar Int                 -- ^ Source of integers.
      ,appCursors :: TVar (HashMap Int Cursor) -- ^ Mapping from integer to state cursors.
      }

-- | State cursor.
data Cursor =
  forall cursor state. Cursor (Ref cursor state)

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
#ifndef __GHCJS__
instance ToJSRef ReactNode_ where
#endif

instance IsString ReactNode' where
    fromString = (castRef :: JSString -> ReactNode') . fromString

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
toPropsRef :: App state m -> ElemProps -> Maybe (Ref state cursor) -> IO (JSRef props)
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

-- | First argument is the virtual element, latter is a real DOM
-- element serving as the container.
#ifdef __GHCJS__
foreign import javascript "React.render($1, $2)"
    js_React_render :: ReactElement' -> Element -> IO ()
#else
js_React_render :: ReactElement' -> Element -> IO ()
js_React_render = undefined
#endif

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
        -> JSRef (Ref state cursor)
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
        -> JSRef (Ref state cursor)
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
                 ,compRef :: Ref state cursor}

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

-- FIXME Flesh out fully following: http://facebook.github.io/react/docs/events.html
newtype EventListener event = EventListener Text

class (Coercible ReactEvent event, Coercible event ReactEvent) => IsReactEvent event
instance IsReactEvent ReactEvent

-- | React transformer.
type ReactT state m = ReaderT (TVar state) (StateT (ReactNode state) m)

-- | Pure react monad.
type React state = ReactT state Identity

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

-- | Class used for creating components.
data Class state cursor m =
  Class {_classApp :: App state m
        -- ^ Application.
        ,_classRender :: ReactT state m ()
        -- ^ Rendering function.
        ,_classDidMount :: (forall props. Ref state cursor -> JQuery -> JSRef props -> IO ())
        -- ^ Did mount handler.
        ,_classDidUpdate :: (forall props. Ref state cursor -> JSRef props -> IO ())
        -- ^ Did update.
        ,_classShouldUpdate :: (forall props. Ref state cursor -> JSRef props -> IO Bool)
        -- ^ Should update?
        ,_classReceivingProps :: (forall props. Ref state cursor -> JSRef props -> IO ())
        -- ^ Receiving new props.
        }
