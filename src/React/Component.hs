{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- |

module React.Component where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import qualified Data.HashMap.Strict as M
import           Data.Typeable
import           Debug.Trace
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

-- | Make a new class spec.
newClass :: App state m                                                          -- ^ Application.
         -> (ReactT state m ())                                                  -- ^ Rendering function.
         -> (forall props. Traversal' state cursor -> JQuery -> JSRef props -> IO ()) -- ^ Did mount handler.
         -> (forall props. Traversal' state cursor -> JSRef props -> IO ())           -- ^ Did update.
         -> (forall props. Traversal' state cursor -> JSRef props -> IO Bool)         -- ^ Should update?
         -> (forall props. Traversal' state cursor -> JSRef props -> IO ())           -- ^ Receiving new props.
         -> Class state cursor m
newClass app render didMount didUpdate shouldUpdate recProps =
  Class app
        render
        (\t q p -> didMount t q p)
        (\t p -> didUpdate t p)
        (\t p -> shouldUpdate t p)
        (\t p -> recProps t p)

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
                                          (_classApp cls)
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
                 Just (Cursor cursorc :: Cursor) ->
                   do
                      _classDidMount cls
                                     (cursorToTraversal (unsafeCoerce cursorc))
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
                 Just (Cursor cursorc :: Cursor) ->
                   do
                      _classDidUpdate cls
                                      (cursorToTraversal (unsafeCoerce cursorc))
                                      ref
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
                 Just (Cursor cursorc :: Cursor) ->
                   do
                      _classShouldUpdate cls
                                         (cursorToTraversal (unsafeCoerce cursorc))
                                         ref
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
                 Just (Cursor cursorc :: Cursor) ->
                   do
                      _classReceivingProps cls
                                           (cursorToTraversal (unsafeCoerce cursorc))
                                           newprops
               return ())
     n <-
       js_React_createClass renderFun didMountFun didUpdateFun shouldUpdateFun willRecPropsFun
     return (Component n)
