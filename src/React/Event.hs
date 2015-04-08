{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-- |

module React.Event where

import           Control.Concurrent.STM (TVar, check, atomically, readTVar, modifyTVar)
import           Control.Monad.Reader (ReaderT(..), MonadReader(ask))
import           Data.Coerce (coerce, Coercible)
import qualified Data.Map as Map (union, fromList, toList, null)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHCJS.Compat
import           React.Internal

#ifdef __GHCJS__
import           JavaScript.JQuery (JQuery)
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.DOM.Types (Element (..), Event (..))
import           GHCJS.Foreign
import           GHCJS.DOM
import           GHCJS.DOM.Element
import           GHCJS.DOM.Event
#endif

onEvent :: (IsReactEvent event, Monad m)
   => EventListener event
   -> (event -> TVar state -> IO ())
   -> ReactT state m ()
onEvent (EventListener name) f = onRaw name $ \re var -> f (coerce re) var

-- | Add event handler. Does not overwrite existing keys.
onRaw :: Monad m => Text -> (ReactEvent -> TVar state -> IO ()) -> ReactT state m ()
onRaw name f =
  do app <- ask
     modifyProps
       (\ep ->
          ep {epEvents =
                epEvents ep `Map.union`
                Map.fromList
                  [(name,\ev -> f ev (appState app))]})

bubbles :: IsReactEvent event => event -> IO Bool
bubbles = getPropBool "bubbles" . coerce

cancelable :: IsReactEvent event => event -> IO Bool
cancelable = getPropBool "cancelable" . coerce

currentTarget :: IsReactEvent event => event -> IO Element
currentTarget = getPropElement "currentTarget" . coerce

defaultPrevented :: IsReactEvent event => event -> IO Bool
defaultPrevented = getPropBool "defaultPrevented" . coerce

eventPhase :: IsReactEvent event => event -> IO Int
eventPhase = getPropInt "eventPhase" . coerce

isTrusted :: IsReactEvent event => event -> IO Bool
isTrusted = getPropBool "isTrusted" . coerce

nativeEvent :: IsReactEvent event => event -> IO Event
nativeEvent = getPropEvent "nativeEvent" . coerce

preventDefault :: IsReactEvent event => event -> IO ()
preventDefault = js_preventDefault . coerce

stopPropagation :: IsReactEvent event => event -> IO ()
stopPropagation = js_stopPropagation . coerce

target :: IsReactEvent event => event -> IO Element
target = getPropElement "target" . coerce

-- FIXME missing timeStamp

eventType :: IsReactEvent event => event -> IO Text
eventType e = fmap fromJSString $ getProp ("type" :: JSString) (coerce e :: ReactEvent)

newtype ClipboardEvent = ClipboardEvent ReactEvent
instance IsReactEvent ClipboardEvent

copy, cut, paste :: EventListener ClipboardEvent
copy = EventListener "copy"
cut = EventListener "cut"
paste = EventListener "paste"

onCopy :: Monad m => (ClipboardEvent -> TVar state -> IO ()) -> ReactT state m ()
onCopy = onEvent copy

onCut :: Monad m => (ClipboardEvent -> TVar state -> IO ()) -> ReactT state m ()
onCut = onEvent cut

onPaste :: Monad m => (ClipboardEvent -> TVar state -> IO ()) -> ReactT state m ()
onPaste = onEvent paste

class IsReactEvent event => IsClipboardEvent event
instance IsClipboardEvent ReactEvent
instance IsClipboardEvent ClipboardEvent

clipboardData :: IsClipboardEvent event => event -> IO (JSRef a) -- FIXME better data type
clipboardData e = getProp ("clipboardData" :: JSString) (coerce e :: ReactEvent)

newtype MouseEvent = MouseEvent ReactEvent
instance IsReactEvent MouseEvent

dblClick,click, mouseMove, mouseOut :: EventListener MouseEvent
click = EventListener "click"
dblClick = EventListener "dblClick"
mouseMove = EventListener "mouseMove"
mouseOut = EventListener "mouseOut"

onClick :: Monad m => (MouseEvent -> TVar state -> IO ()) -> ReactT state m ()
onClick = onEvent click

onDblClick :: Monad m => (MouseEvent -> TVar state -> IO ()) -> ReactT state m ()
onDblClick = onEvent dblClick

onMouseMove :: Monad m => (MouseEvent -> TVar state -> IO ()) -> ReactT state m ()
onMouseMove = onEvent mouseMove

onMouseOut :: Monad m => (MouseEvent -> TVar state -> IO ()) -> ReactT state m ()
onMouseOut = onEvent mouseOut

class IsReactEvent event => IsMouseEvent event
instance IsMouseEvent ReactEvent
instance IsMouseEvent MouseEvent

altKey :: IsMouseEvent event => event -> IO Bool
altKey = getPropBool "altKey" . coerce

button :: IsMouseEvent event => event -> IO Int
button = getPropInt "button" . coerce

buttons :: IsMouseEvent event => event -> IO Int
buttons = getPropInt "buttons" . coerce

clientX :: IsMouseEvent event => event -> IO Int
clientX = getPropInt "clientX" . coerce

clientY :: IsMouseEvent event => event -> IO Int
clientY = getPropInt "clientY" . coerce

ctrlKey :: IsMouseEvent event => event -> IO Bool
ctrlKey = getPropBool "ctrlKey" . coerce

getModifierState :: IsMouseEvent event => event -> Text -> IO Bool
getModifierState e t = js_getModifierState (coerce e) (toJSString t)

metaKey :: IsMouseEvent event => event -> IO Bool
metaKey = getPropBool "metaKey" . coerce

pageX :: IsMouseEvent event => event -> IO Int
pageX = getPropInt "pageX" . coerce

pageY :: IsMouseEvent event => event -> IO Int
pageY = getPropInt "pageY" . coerce

relatedTarget :: IsMouseEvent event => event -> IO Element
relatedTarget = getPropElement "relatedTarget" . coerce

screenX :: IsMouseEvent event => event -> IO Int
screenX = getPropInt "screenX" . coerce

screenY :: IsMouseEvent event => event -> IO Int
screenY = getPropInt "screenY" . coerce

shiftKey :: IsMouseEvent event => event -> IO Bool
shiftKey = getPropBool "shiftKey" . coerce

-- | Get event val.
getVal :: ReactEvent -> IO Text
getVal ev =
  do jstr <- getVal_ ev
     return (T.pack (fromJSString jstr))

#ifdef __GHCJS__
foreign import javascript "$1.target.value"
    getVal_ :: ReactEvent -> IO JSString
#else
getVal_ :: ReactEvent -> IO JSString
getVal_ = undefined
#endif
