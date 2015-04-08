{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | React binding to Ace.

module React.Ace
  (createAce
  ,Ace
  ,defaultAce
  ,aceSelectStartLine
  ,aceSelectStartCol
  ,aceSelectEndLine
  ,aceSelectEndCol
  ,aceClientX
  ,aceClientY
  ,onAceClick)
  where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import GHC.Exts
import GHCJS.Compat
import React
import React.Components
import React.Internal
import React.Ref

#ifdef __GHCJS__
import JavaScript.JQuery (JQuery)
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM.Types (Element (..), Event (..))
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM.Types (Element (..), Event (..))
import GHCJS.Foreign
import GHCJS.DOM
#endif

--------------------------------------------------------------------------------
-- Types

-- | Ace component's state.
data Ace = Ace (Maybe Editor')

-- | An Editor editor.
data Editor_
type Editor' = JSRef Editor_

-- | Default state for instances of ace.
defaultAce :: IO Ace
defaultAce =
  return (Ace Nothing)

--------------------------------------------------------------------------------
-- Component

-- | Create an Ace editor component.
createAce :: Monad m
          => App state m                -- ^ The app.
          -> IO (Component state Ace m) -- ^ Ace component.
createAce app =
  createComponent
    (newClass app
              (return ())
              (didMount app)
              (\_l _props -> return ())
              (\_ _ -> return False)
              (receivingProps app))

-- | Setup the ace editor.
didMount :: App a m -> Lens' a Ace -> JQuery -> JSRef this -> IO ()
didMount app r el this =
  do props <- getProp ("props" :: JSString) this
     onClickFun <- getProp ("onClick" :: JSString) props
     onDblClickFun <- getProp ("onDblClick" :: JSString) props
     editor <- makeEditor el
                          onClickFun
                          onDblClickFun
     atomically
       (modifyTVar (appState app)
                   (set r (Ace (Just editor))))

-- | New code attribute has been set, update the editor contents.
receivingProps :: App state m -> Lens' state Ace -> JSRef a -> IO ()
receivingProps app l props =
  do codeRef <- getProp ("code" :: JSString) props
     mcode <- fromJSRef codeRef
     case mcode of
       Nothing -> return ()
       Just (code :: JSString) ->
         do Ace meditor <- fmap (view l)
                                (atomically (readTVar (appState app)))
            case meditor of
              Nothing -> return ()
              Just editor ->
                do code' <- getValue editor
                   when (code /= code')
                        (setValue editor code)
                   --
                   stateStartLine <- getStartLine props
                   stateStartCol <- getStartCol props
                   stateEndLine <- getEndLine props
                   stateEndCol <- getEndCol props
                   --
                   range <- getSelectionRange editor
                   curStartLine <- getStartLine range
                   curStartCol <- getStartCol range
                   curEndLine <- getEndLine range
                   curEndCol <- getEndCol range
                   --
                   case (stateStartLine,stateStartCol,stateEndLine,stateEndCol) of
                     (Just sl,Just sc,Just el,Just ec) ->
                       when ((stateStartLine
                             ,stateStartCol
                             ,stateEndLine
                             ,stateEndCol) /=
                             (curStartLine
                             ,curStartCol
                             ,curEndLine
                             ,curEndCol))
                            (setRange editor (sl-1) (sc-1) (el-1) (ec-1))
                     _ -> return ()

--------------------------------------------------------------------------------
-- Selection range accessors

getStartLine :: JSRef a -> IO (Maybe Int)
getStartLine props =
  do r <- getProp ("start-line" :: JSString) props
     fromJSRef r

getEndLine :: JSRef a -> IO (Maybe Int)
getEndLine props =
  do r <- getProp ("end-line" :: JSString) props
     fromJSRef r

getStartCol :: JSRef a -> IO (Maybe Int)
getStartCol props =
  do r <- getProp ("start-col" :: JSString) props
     fromJSRef r

getEndCol :: JSRef a -> IO (Maybe Int)
getEndCol props =
  do r <- getProp ("end-col" :: JSString) props
     fromJSRef r

--------------------------------------------------------------------------------
-- Component events

newtype SelectEvent = SelectEvent ReactEvent
instance IsReactEvent SelectEvent

-- | When the selection changes.
onAceClick :: Monad m => (SelectEvent -> TVar state -> IO ()) -> ReactT state m ()
onAceClick = onEvent (EventListener "click")

-- | Extract the start line from the event.
aceSelectStartLine :: SelectEvent -> IO Int
aceSelectStartLine = getPropInt "startLine" . coerce

-- | Extract the start col from the event.
aceSelectStartCol :: SelectEvent -> IO Int
aceSelectStartCol = getPropInt "startCol" . coerce

-- | Extract the end line from the event.
aceSelectEndLine :: SelectEvent -> IO Int
aceSelectEndLine = getPropInt "endLine" . coerce

-- | Extract the end col from the event.
aceSelectEndCol :: SelectEvent -> IO Int
aceSelectEndCol = getPropInt "endCol" . coerce

aceClientX :: SelectEvent -> IO Int
aceClientX = getPropInt "clientX" . coerce

aceClientY :: SelectEvent -> IO Int
aceClientY = getPropInt "clientY" . coerce

--------------------------------------------------------------------------------
-- Foreign imports

#ifdef __GHCJS__

foreign import javascript "makeEditor($1,$2,$3)"
    makeEditor :: JQuery
               -> JSFun (JSRef props -> IO ())
               -> JSFun (JSRef props -> IO ())
               -> IO Editor'

foreign import javascript "($1).setValue($2,-1)"
  setValue :: Editor' -> JSString -> IO ()

foreign import javascript "getSelectionRange($1)"
  getSelectionRange :: Editor' -> IO (JSRef Int)

foreign import javascript "($1).selection.setSelectionRange(new AceRange($2,$3,$4,$5))"
  setRange :: Editor' -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript "($1).getValue()"
  getValue :: Editor' -> IO JSString

foreign import javascript "$1===$2"
  stringEq :: JSString -> JSString -> Bool

#else

getSelectionRange :: Editor' -> IO (JSRef Int)
getSelectionRange = undefined

setRange :: Editor' -> Int -> Int -> Int -> Int -> IO ()
setRange = undefined

makeEditor :: JQuery
           -> (JSRef props -> IO ())
           -> (JSRef props -> IO ())
           -> IO Editor'
makeEditor = undefined

setValue :: Editor' -> JSString -> IO ()
setValue = undefined

getValue :: Editor' -> IO JSString
getValue = undefined

stringEq :: JSString -> JSString -> Bool
stringEq = undefined

#endif

instance Eq JSString where
    (==) = stringEq

--------------------------------------------------------------------------------
-- Instances

instance Show Ace where
    show (Ace m) = "<Ace " ++ c ++ " <MVar ()>>"
      where c = case m of
                  Nothing -> "Nothing"
                  Just _ -> "Just <Editor>"

instance Eq Ace where
    Ace Nothing == Ace Nothing = True
    Ace (Just{}) == Ace (Just {}) = True
    _ == _ = False
