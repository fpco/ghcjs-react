{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- |

module React.Ref where

import Control.Concurrent.STM
import GHCJS.Compat
import Unsafe.Coerce

#ifdef __GHCJS__
import JavaScript.JQuery (JQuery)
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM
import GHCJS.Foreign
#endif

--------------------------------------------------------------------------------
-- Dumb functional references

-- | A functional reference.
data Ref state cursor where
    Ref :: (state -> cursor) -> (cursor -> state -> state) -> Ref state cursor

instance ToJSRef (Ref state cursor) where
    toJSRef = unsafeCoerce

instance FromJSRef (Ref state cursor) where
    fromJSRef = return . Just . unsafeCoerce

-- | Put in the functional reference.
putR :: Ref state cursor -> (cursor -> state -> state)
putR (Ref _ put) = put

-- | Get from the functional reference.
getR :: Ref state cursor -> (state -> cursor)
getR (Ref get _) = get

-- | Modify using a functional reference.
modifyR :: Ref state cursor -> (cursor -> cursor) -> state -> state
modifyR r f state =
  case r of
    Ref get put ->
        put (f (get state)) state

-- | Set the state at the given ref.
setAt :: Ref state cursor -> cursor -> TVar state -> IO ()
setAt ref update var =
  atomically
    (modifyTVar var
                (putR ref update))

-- | Read a value from the state.
readAt :: Ref state cursor -> TVar state -> IO cursor
readAt ref var =
  atomically
    (fmap (getR ref)
          (readTVar var))

-- | Modify the state at the given ref.
modifyAt :: Ref state cursor -> (cursor -> cursor) -> TVar state -> IO ()
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

instance Show (Ref state cursor) where
  show _ = "<Ref>"
