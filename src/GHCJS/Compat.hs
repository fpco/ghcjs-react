{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Compatibility for both GHCi and GHCJS.

module GHCJS.Compat where

import Data.String
import Data.Text (Text)

#ifdef __GHCJS__
import JavaScript.JQuery (JQuery)
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM
import GHCJS.Foreign
#endif

-- So I can type check this in GHCi.
#ifndef __GHCJS__
data Event
data JQuery
type JSObject a = JSRef (JSObject_ a)
data JSObject_ a
data JSRef a
-- | A DOM element.
data Element
castRef = undefined
toJSString = undefined
data JSString
instance IsString JSString where fromString = undefined
newObj = undefined
setProp = undefined
getProp = undefined
toJSRef_aeson = undefined
fromJSString = undefined
syncCallback1 = undefined
syncCallback2 = undefined
syncCallback = undefined
data U = AlwaysRetain
#endif

#ifndef __GHCJS__
class ToJSRef a where
  toJSRef :: a -> IO (JSRef a)
  toJSRef = undefined
  toJSRefListOf :: [a] -> IO (JSRef [a])
  toJSRefListOf = undefined
class FromJSRef a where
  fromJSRef :: JSRef a -> IO (Maybe a)
  fromJSRef = undefined
  fromJSRefListOf :: JSRef [a] -> IO (Maybe [a])
  fromJSRefListOf = undefined
instance FromJSRef JQuery
instance FromJSRef Int
instance FromJSRef Text
instance FromJSRef JSString
instance FromJSRef x => FromJSRef [x]
instance ToJSRef x => ToJSRef [x]
instance ToJSRef x => ToJSRef (JSRef x)
instance ToJSRef Int
#endif
