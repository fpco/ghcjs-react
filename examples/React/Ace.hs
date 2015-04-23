{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | React binding to Ace.

module React.Ace where

import           Control.Concurrent.STM
import           Control.Lens hiding (coerce)
import           Control.Monad
import qualified Data.Text as T
import           GHC.Exts
import           GHCJS.Compat
import           React hiding (onClick)
import           React.Internal

#ifdef __GHCJS__
import           JavaScript.JQuery (JQuery)
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.DOM.Types (Element (..), Event (..))
import           GHCJS.Foreign
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.DOM.Types (Element (..), Event (..))
import           GHCJS.Foreign
import           GHCJS.DOM
#endif

--------------------------------------------------------------------------------
-- Types

-- | Ace component's state.
data Ace = Ace (Maybe Editor')

-- | An Editor editor.
data Editor_
type Editor' = JSRef Editor_

-- | Default state for instances of ace.
getDef :: IO Ace
getDef = return (Ace Nothing)

--------------------------------------------------------------------------------
-- Component

-- | Create an Ace editor component.
new :: Monad m
    => App state m                -- ^ The app.
    -> IO (Component state Ace m) -- ^ Ace component.
new app =
  createComponent
    (newClass app
              (return ())
              (didMount app)
              (\_l _props -> return ())
              (\_ _ -> return False)
              (receivingProps app))

-- | Setup the ace editor.
didMount :: App a m -> Traversal' a Ace -> JQuery -> JSRef this -> IO ()
didMount app r el this =
  do putStrLn "Did mount!"
     props <- getProp ("props" :: JSString) this
     onClickFun <- getProp ("onClick" :: JSString) props
     onDblClickFun <- getProp ("onDblClick" :: JSString) props
     editor <- makeEditor el
                          onClickFun
                          onDblClickFun
     atomically
       (modifyTVar (appState app)
                   (set r (Ace (Just editor))))

-- | New code attribute has been set, update the editor contents.
receivingProps :: App state m -> Traversal' state Ace -> JSRef a -> IO ()
receivingProps app l props =
  do putStrLn "Receiving props!"
     codeRef <- getProp ("code" :: JSString) props
     mcode <- fromJSRef codeRef
     case mcode of
       Nothing -> return ()
       Just (code :: JSString) ->
         do meditor <- fmap (preview l)
                            (atomically (readTVar (appState app)))
            case meditor of
              Nothing -> return ()
              Just (Ace (Just editor)) ->
                do code' <- getValue editor
                   putStrLn ("Code " ++ fromJSString code)
                   putStrLn ("Code' " ++ fromJSString code')
                   when (code /= code')
                        (do putStrLn "Set the value!"
                            setValue editor code)
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
              _ -> return ()

--------------------------------------------------------------------------------
-- Properties

startline_ :: (Monad m) => Int -> ReactT state m ()
startline_ =
  attr "startline" .
  T.pack . show

startcol_ :: (Monad m) => Int -> ReactT state m ()
startcol_ =
  attr "startcol" .
  T.pack . show

endline_ :: (Monad m) => Int -> ReactT state m ()
endline_ =
  attr "endline" .
  T.pack . show

endcol_ :: (Monad m) => Int -> ReactT state m ()
endcol_ = attr "endcol". T.pack . show

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
onClick :: Monad m => (SelectEvent -> TVar state -> IO ()) -> ReactT state m ()
onClick = onEvent (EventListener "click")

-- | Extract the start line from the event.
selectStartLine :: SelectEvent -> IO Int
selectStartLine = getPropInt "startLine" . coerce

-- | Extract the start col from the event.
selectStartCol :: SelectEvent -> IO Int
selectStartCol = getPropInt "startCol" . coerce

-- | Extract the end line from the event.
selectEndLine :: SelectEvent -> IO Int
selectEndLine = getPropInt "endLine" . coerce

-- | Extract the end col from the event.
selectEndCol :: SelectEvent -> IO Int
selectEndCol = getPropInt "endCol" . coerce

clientX :: SelectEvent -> IO Int
clientX = getPropInt "clientX" . coerce

clientY :: SelectEvent -> IO Int
clientY = getPropInt "clientY" . coerce

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
