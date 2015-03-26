{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Element builders.

module React.Builder where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map as Map
import           Data.Monoid
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           React.Internal
import           React.Ref
import           React.Lens

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
     (a,child) <- ReaderT (const (StateT (\s ->
                                            do r <- runReactT name var m
                                               return (r,s))))
     modifyEl (\e ->
                 e {elemChildren = elemChildren e <> V.singleton child})
     return a

-- | Build a component.
buildComponent :: Monad m
               => Component s a m -- ^ The component.
               -> Lens s s a a    -- ^ A cursor into the state for this instance.
               -> ReactT s m x    -- ^ Set attributes for the
                                  -- component. Ignores content (for
                                  -- now).
               -> ReactT s m x
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
                                           (lensRef cursor)))})
     return a
  where getProps (RNElement (ReactElement "tmp" props _)) = props
        getProps x = error ("getProps: unexpected case: " ++ show x)

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
       ep {epOtherProps = epOtherProps ep `Map.union` Map.fromList [(name,prop)]})

instance (a ~ (),Monad m) => IsString (ReaderT (TVar state) (StateT (ReactNode state) m) a) where
  fromString = text . T.pack
