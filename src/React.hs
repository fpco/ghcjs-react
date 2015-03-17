{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module React
    (-- * Basic application
     -- $react-pre
     react
     -- ** The app type
     -- $the-app-type
     ,App
     -- $make-app
     ,makeApp
     -- ** Render function
     -- $render
     ,ReactT
     -- ** The parent DOM element
     -- $parent
    ,Element
    ,getElementById
    -- * Building the DOM
    -- $building
    ,build
    ,text
    ,style
    ,attr
    ,styles
    ,attrs
    -- * Handling events
    -- $events
    ,onEvent
    ,onClick
    ,onDblClick
    ,onMouseMove
    ,onMouseOut
    )
    where

import Control.Concurrent.STM
import Data.Monoid
import GHCJS.Compat
import React.Builder
import React.Internal
import React.Events

#ifdef __GHCJS__
import JavaScript.JQuery (JQuery)
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM.Types (Element (..), Event (..))
import GHCJS.Foreign
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.Event
#endif

--------------------------------------------------------------------------------
-- $react-pre
--
-- This module is laid out as both a tutorial and documentation. You
-- should be able to read it like a tutorial from start to end.
--
-- The way to start a React application within an element is via the
-- 'react' function:

-- | Loops forever. Blocks on state updates; whenever state changes it
-- re-renders.
react :: (Show state,Eq state)
      => App state m
      -- ^ The application's state.
      -> (state -> ReactT state m ())
      -- ^ The view renderer.
      -> Element
      -- ^ The parent DOM element into which the application is
      -- placed.
      -> IO ()
react app@(App var run _ _) render dom =
  do let loop stateOld =
           do node <-
                run (runReactT "div" var (render stateOld))
              reactRender app
                          dom
                          (snd node)
              stateNew <-
                atomically $
                do stateNew <- readTVar var
                   check (stateOld /= stateNew)
                   return stateNew
              loop stateNew
     state0 <- atomically (readTVar var)
     loop state0
  where unwrap (RNElement e) = e
        reactRender :: App state m -> Element -> ReactNode state -> IO ()
        reactRender app dom re =
          do re' <- toReactElem app re
             js_React_render re' dom

--------------------------------------------------------------------------------
-- $the-app-type
--
-- The 'App' type is an opaque type which holds a mutable cell
-- containing the pure state value.

--------------------------------------------------------------------------------
-- $make-app
--
-- To make an 'App' value, you can use 'makeApp':

-- | Make the application with the given initial state and runner.
makeApp :: Monad m
        => state
        -- ^ The initial application state.
        -> (forall a. m a -> IO a)
        -- ^ The monad runner. By default you can use 'id'.
        -> IO (App state m)
        -- ^ A new application state.
makeApp state run =
  do var <- newTVarIO state
     ints <- newTVarIO 0
     cursors <- newTVarIO mempty
     let app =
           App var run ints cursors
     return app

--------------------------------------------------------------------------------
-- $render
--
-- The 'react' function also needs a rendering function. This should
-- receive the current state and run in the 'ReactT' monad.
--
-- For example:
--
-- @
-- \\state -> build "div" (text "Hello, World!")
-- @
--
-- This will create a @\<div\>Hello, World!\</div\>@ element.
--
-- You can find a complete list of HTML builder combinators in
-- "React.Builder".

--------------------------------------------------------------------------------
-- $parent
--
-- React needs a parent element. You can use this handy
-- 'getElementById' below to find it.

#ifdef __GHCJS__
foreign import javascript "document.getElementById($1)"
    getElementById :: JSString -> IO Element
#else
-- | Get the element by its @id=\"foo\"@ identifer..
getElementById :: JSString -- ^ Element identifier.
               -> IO Element
getElementById = undefined
#endif

--------------------------------------------------------------------------------
-- $building
--
-- There are several basic building functions that you can use to
-- render your reactive DOM tree.

--------------------------------------------------------------------------------
-- $events
--
-- There is one main function for binding event handlers which is
-- `onEvent` (see below). There are also a few basic helper functions
-- like 'onClick', 'onMouseOut', etc.
--
-- For example:
--
-- @
-- build \"button\" (do text \"Click me!\"
--                    onClick (\event state -> putStrLn "You clicked me!"))
-- @
--
-- There is a variety of event predicates and accessors which can be
-- found in "React.Events".
