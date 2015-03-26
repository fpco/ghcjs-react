{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
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
  State { stateAce :: Maybe Ace }
  deriving (Show,Eq)

-- | Main entry point.
main :: IO ()
main =
  do app <- makeApp (State Nothing) id
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
       (Lens (\(State ace) -> fromJust ace)
             (\ace state -> State (Just ace)))
       (attr "code" "Hello, world!")
