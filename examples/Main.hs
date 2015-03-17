{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Example application.

module Main where




import React
import React.Internal

import React.Ace

-- | Application state.
data State =
  State
  deriving (Show,Eq)

-- | Main entry point.
main :: IO ()
main =
  do app <- makeApp State id
     ace <- createAce app
     container <- getElementById "container"
     react app (render ace) container

-- | Our main view.
render :: Monad m
       => Component s Ace m -- ^ Ace component.
       -> s                 -- ^ State.
       -> ReactT s m ()
render ace state =
  build "div" (text "Hello, world!?")
