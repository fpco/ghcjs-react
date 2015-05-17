{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Element builders.

module React.Builder where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map as Map
import           Data.Monoid
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           React.Internal
import           System.IO.Unsafe (unsafePerformIO)

#ifdef __GHCJS__
import           JavaScript.JQuery (JQuery)
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.DOM.Types (Element (..), Event (..))
import           GHCJS.Foreign
#endif

-- | Build an element.
build :: Monad m
      => Text -- ^ Name of the element.
      -> ReactT state m a -- ^ Inner content.
      -> ReactT state m a
build name m =
  do var <- ask
     (a,child) <- ReactT (ReaderT (const (StateT (\s ->
                                             do r <- runReactT name var m
                                                return (r,s)))))
     modifyEl (\e ->
                 e {elemChildren = elemChildren e <> V.singleton child})
     return a

-- | Build a component.
buildComponent :: (Monad m,MonadIO m)
               => Component s a m -- ^ The component.
               -> Traversal' s a  -- ^ A cursor into the state for this instance.
               -> ReactT s m x    -- ^ Set attributes for the
                                  -- component. Ignores content (for
                                  -- now).
               -> ReactT s m x
buildComponent (Component cls) cursor m =
  do app <- ask
     (a,child) <-
       ReactT (ReaderT (const (StateT (\s ->
                                         do r <-
                                              runReactT "tmp" app m
                                            return (r,s)))))
     -- The above is just used for running attributes. ^
     cursorId <- internalLiftIOReact (genCursor app (traversalToCursor cursor))
     modifyEl
       (\e ->
          e {elemChildren =
               elemChildren e <>
               V.singleton
                 (RNComponent
                    (ReactComponent cls
                                    (getProps child)
                                    cursorId))})
     return a
  where getProps (RNElement (ReactElement "tmp" props _)) = props
        getProps x =
          error ("getProps: unexpected case: " ++ show x)

-- | Add some text to the current node's children.
text :: Monad m
     => Text -- ^ Text content.
     -> ReactT state m ()
text t =
  modifyEl (\e -> e {elemChildren = elemChildren e <> V.singleton (RNText t)})

-- | Add a style.
style :: Monad m => Text
      -- ^ CSS property name in JavaScript format; @fontSize@, etc.
      -> Text -- ^ Value.
      -> ReactT state m ()
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
attr :: Monad m
     => Text -- ^ Name.
     -> Text -- ^ Value.
     -> ReactT state m ()
attr name prop =
  modifyProps
    (\ep ->
       ep {epOtherProps = Map.insert name prop (epOtherProps ep)})

refAttr :: Monad m
        => Text -- ^ Name.
        -> JSRef a -- ^ Value.
        -> ReactT state m ()
refAttr name val =
  modifyProps
    (\ep ->
       ep {epRefProps = Map.insert name (RefProp val) (epRefProps ep)})

dangerouslySetInnerHTML :: Monad m => JSString -> ReactT state m ()
dangerouslySetInnerHTML html = do
  refAttr "dangerouslySetInnerHTML" $ unsafePerformIO $ do
    obj <- newObj
    setProp ("__html" :: JSString) html obj
    return obj

instance (a ~ (),Monad m) => IsString (ReactT state m a) where
  fromString = text . T.pack
