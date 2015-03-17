{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- |

module React.Components where

import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.HashMap.Strict as M
import           GHCJS.Compat
import           React.Builder
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

-- | Create a component class.
createClass :: (Monad m)
            => Class state cursor m
            -> IO (Component state cursor m)
createClass cls =
  do renderFun <-
       syncCallback1
         AlwaysRetain
         True
         (\elRef ->
            do el <-
                 appRun (classApp cls)
                        (liftM snd
                               (runReactT "div"
                                          (appState (classApp cls))
                                          (do attr "data-component" "true"
                                              (classRender cls))))
               el' <-
                 toReactElem (classApp cls)
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
                 atomically (readTVar (appCursors (classApp cls)))
               case M.lookup cursor cs of
                 Nothing ->
                   error ("Couldn't find cursor: " ++ show cursor)
                 Just (Cursor cursor) ->
                   case unsafeCoerce cursor of
                     c ->
                       classDidMount cls
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
                 atomically (readTVar (appCursors (classApp cls)))
               case M.lookup cursor cs of
                 Nothing ->
                   error ("Couldn't find cursor: " ++ show cursor)
                 Just (Cursor cursor) ->
                   case unsafeCoerce cursor of
                     c ->
                       classDidUpdate cls c ref
               return ())
     shouldUpdateFun <-
       syncCallback1
         AlwaysRetain
         True
         (\ref ->
            do cursor <- js_React_props_cursor ref
               cs <-
                 atomically (readTVar (appCursors (classApp cls)))
               case M.lookup cursor cs of
                 Nothing ->
                   error ("Couldn't find cursor: " ++ show cursor)
                 Just (Cursor cursor) ->
                   case unsafeCoerce cursor of
                     c ->
                       classShouldUpdate cls c ref
               return ())
     willRecPropsFun <-
       syncCallback2
         AlwaysRetain
         True
         (\ref newprops ->
            do cursor <- js_React_props_cursor ref
               cs <-
                 atomically (readTVar (appCursors (classApp cls)))
               case M.lookup cursor cs of
                 Nothing ->
                   error ("Couldn't find cursor: " ++ show cursor)
                 Just (Cursor cursor) ->
                   case unsafeCoerce cursor of
                     c ->
                       classReceivingProps cls c newprops
               return ())
     n <-
       js_React_createClass renderFun didMountFun didUpdateFun shouldUpdateFun willRecPropsFun
     return (Component n)
