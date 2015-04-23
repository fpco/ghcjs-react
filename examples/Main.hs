{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Example application.

module Main where

import Data.Maybe
import React
import React.Ace as Ace
import React.Builder
import React.Internal
import React.Lucid
import Control.Lens
import Control.Lens.TH
import Control.Monad.IO.Class

-- | Application state.
data State
  = Start
  | MainState Main
  deriving (Show,Eq)

data Main =
  Main {_mainAce :: Ace}
  deriving (Show,Eq)

$(makePrisms ''State)
$(makeLenses ''Main)

-- | Main entry point.
main :: IO ()
main =
  do app <- getApp
     ace <- Ace.new app
     container <- getElementById "container"
     (react app (render ace) container)

-- | Make the application.
getApp :: IO (App State IO)
getApp = makeApp Start id

-- | Our main view.
render :: MonadIO m
       => Component State Ace m -> State -> ReactT State m ()
render ace state =
  do build "div" (text "Hello, world!")
     buildComponent
       ace
       (_MainState . mainAce)
       (do code_ "main = putStrLn \"Woot\""
           Ace.startline_ 0
           Ace.startcol_ 0
           Ace.endline_ 0
           Ace.endcol_ 0)
