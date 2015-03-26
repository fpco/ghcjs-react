{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- | Functional reference.

module React.Ref where

import Control.Concurrent.STM
import GHCJS.Compat
import Unsafe.Coerce
import React.Lens

#ifdef __GHCJS__
import GHCJS.Marshal
#endif

-- | Convert a classic functional reference to a CPS lens.
refLens :: Ref s a -> Lens s s a a
refLens (Ref get update) =
  \f s ->
    fmap (\v -> update v s)
         (f (get s))

-- | Convert a CPS lens to a classical functional reference.
lensRef :: Lens s s a a -> Ref s a
lensRef l =
  Ref (view l)
      (set l)

-- | A functional reference.
data Ref state cursor where
    Ref :: (state -> cursor) -> (cursor -> state -> state) -> Ref state cursor

instance ToJSRef (Ref state cursor) where
    toJSRef = unsafeCoerce

instance FromJSRef (Ref state cursor) where
    fromJSRef = return . Just . unsafeCoerce

instance Show (Ref state cursor) where
  show _ = "<Ref>"
