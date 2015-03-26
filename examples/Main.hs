{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Example application.

module Main where

import Data.Maybe
import React
import React.Ace
import React.Builder
import React.Internal
import React.Ref

-- | Application state.
data State =
  State { stateAce :: Ace }
  deriving (Show,Eq)

-- | Main entry point.
main :: IO ()
main =
  do aceState <- defaultAce
     app <- makeApp (State aceState) id
     ace <- createAce app
     container <- getElementById "container"
     react app (render ace) container

-- | Our main view.
render :: Monad m
       => Component State Ace m -- ^ Ace component.
       -> State                 -- ^ State.
       -> ReactT State m ()
render ace state =
  do build "div" (text "Hello, world!?")
     buildComponent
       ace
       (\f s ->
          fmap (\a -> s {stateAce = a})
               (f (stateAce s)))
       (attr "code" "Hello, world!")
