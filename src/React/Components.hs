{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- |

module React.Components where

import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.HashMap.Strict as M
import           Debug.Trace
import           GHCJS.Compat
import           React.Builder
import           React.Ref
import           React.Lens
import           React.Internal
import           Unsafe.Coerce

#ifdef __GHCJS__
import           JavaScript.JQuery (JQuery)
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.DOM.Types (Element (..), Event (..))
import           GHCJS.Foreign
import           GHCJS.DOM
import           GHCJS.DOM.Element
import           GHCJS.DOM.Event
#endif

-- | Make a new class spec.
newClass :: App state m -- ^ Application.
         -> (ReactT state m ()) -- ^ Rendering function.
         -> (forall props. Ref state cursor -> JQuery -> JSRef props -> IO ()) -- ^ Did mount handler.
         -> (forall props. Ref state cursor -> JSRef props -> IO ()) -- ^ Did update.
         -> (forall props. Ref state cursor -> JSRef props -> IO Bool) -- ^ Should update?
         -> (forall props. Ref state cursor -> JSRef props -> IO ()) -- ^ Receiving new props.
         -> Class state cursor m
newClass app render didMount didUpdate shouldUpdate recProps =
  Class app
        render
        (\ref q p -> didMount ref q p)
        (\ref p -> didUpdate ref p)
        (\ref p -> shouldUpdate ref p)
        (\ref p -> recProps ref p)

-- | Get the app of the class.
classApp :: Class state cursor m -> App state m
classApp  = _classApp

-- | Create a component class.
createComponent :: (Monad m)
                => Class state cursor m
                -> IO (Component state cursor m)
createComponent cls =
  do renderFun <-
       syncCallback1
         AlwaysRetain
         True
         (\elRef ->
            do el <-
                 appRun (_classApp cls)
                        (liftM snd
                               (runReactT "div"
                                          (appState (_classApp cls))
                                          (do attr "data-component" "true"
                                              (_classRender cls))))
               el' <-
                 toReactElem (_classApp cls)
                             el
               setProp ("r" :: String) el' elRef)
     didMountFun <-
       syncCallback2
         AlwaysRetain
         True
         (\jq ref ->
            do el :: Maybe JQuery <- fromJSRef jq
               cursor <- js_React_props_cursor ref
               cs <-
                 atomically (readTVar (appCursors (_classApp cls)))
               case M.lookup cursor cs of
                 Nothing ->
                   error ("Couldn't find cursor: " ++ show cursor)
                 Just (Cursor cursor) ->
                   case trace "didMountFun.unsafeCoerce" (unsafeCoerce cursor) of
                     c ->
                       _classDidMount cls
                                      c
                                      (maybe (error "didMount: Couldn't get jquery element...") id el)
                                      ref
               return ())
     didUpdateFun <-
       syncCallback1
         AlwaysRetain
         True
         (\ref ->
            do cursor <- js_React_props_cursor ref
               cs <-
                 atomically (readTVar (appCursors (_classApp cls)))
               case M.lookup cursor cs of
                 Nothing ->
                   error ("Couldn't find cursor: " ++ show cursor)
                 Just (Cursor cursor) ->
                   case trace "didUpdateFun.unsafeCoerce" (unsafeCoerce cursor) of
                     c ->
                       _classDidUpdate cls c ref
               return ())
     shouldUpdateFun <-
       syncCallback1
         AlwaysRetain
         True
         (\ref ->
            do cursor <- js_React_props_cursor ref
               cs <-
                 atomically (readTVar (appCursors (_classApp cls)))
               case M.lookup cursor cs of
                 Nothing ->
                   error ("Couldn't find cursor: " ++ show cursor)
                 Just (Cursor cursor) ->
                   case trace "shouldUpdateFun.unsafeCoerce" (unsafeCoerce cursor) of
                     c ->
                       _classShouldUpdate cls c ref
               return ())
     willRecPropsFun <-
       syncCallback2
         AlwaysRetain
         True
         (\ref newprops ->
            do cursor <- js_React_props_cursor ref
               cs <-
                 atomically (readTVar (appCursors (_classApp cls)))
               case M.lookup cursor cs of
                 Nothing ->
                   error ("Couldn't find cursor: " ++ show cursor)
                 Just (Cursor cursor) ->
                   case trace "willRecPropsFun.unsafeCoerce" (unsafeCoerce cursor) of
                     c ->
                       _classReceivingProps cls c newprops
               return ())
     n <-
       js_React_createClass renderFun didMountFun didUpdateFun shouldUpdateFun willRecPropsFun
     return (Component n)
