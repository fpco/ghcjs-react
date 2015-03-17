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
data Ref state cursor get where
    Lens :: (state -> cursor) -> (cursor -> state -> state) -> Lens state cursor
    Prism :: (state -> Maybe cursor) -> (cursor -> state -> state) -> Prism state cursor

-- | A prism, i.e. getting is certain.
type Lens state cursor = Ref state cursor cursor

-- | A prism, i.e. getting is not certain.
type Prism state cursor = Ref state cursor (Maybe cursor)

instance ToJSRef (Lens state cursor) where
    toJSRef = unsafeCoerce

instance FromJSRef (Lens state cursor) where
    fromJSRef = return . Just . unsafeCoerce

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

instance Show (Lens state cursor) where
  show _ = "<Lens>"
