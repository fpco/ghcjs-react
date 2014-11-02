{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM.Types (Element)
import GHCJS.Foreign
import Data.String (IsString (..))
import Control.Monad
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import Control.Applicative
import qualified Data.Vector as V
import Data.Aeson

data ReactElement_
type ReactElement' = JSRef ReactElement_

data ReactNode_
type ReactNode' = JSRef ReactNode_

instance IsString ReactNode' where
    fromString = (castRef :: JSString -> ReactNode') . fromString

foreign import javascript "React.render($1, $2)"
    js_React_render :: ReactElement' -> Element -> IO ()

class ToReactNode a where
    toReactNode :: a -> IO ReactNode'
instance ToReactNode JSString where
    toReactNode = return . castRef
instance ToReactNode Text where
    toReactNode = return . castRef . toJSString
instance ToReactNode ReactElement' where
    toReactNode = return . castRef
instance ToReactNode ReactNode' where
    toReactNode = return . castRef
instance ToReactNode a => ToReactNode [a] where
    toReactNode = fmap castRef . toJSRef <=< mapM toReactNode
instance ToReactNode a => ToReactNode (Vector a) where
    toReactNode = fmap castRef . toJSRef . V.toList <=< V.mapM toReactNode

-- FIXME foreign import javascript "React.createElement($1, $2, $3)"
foreign import javascript "React.createElement($1, null, $3)"
    js_React_createElement
        :: JSString
        -> JSRef props
        -> ReactNode'
        -> IO ReactElement'

foreign import javascript "document.getElementById($1)"
    getElementById :: JSString -> IO Element

data ReactElement = ReactElement Text (Map Text Text) (Vector ReactNode)
data ReactNode = RNElement ReactElement
               | RNText Text

instance ToReactNode ReactNode where
    toReactNode (RNElement re) = toReactNode =<< toReactElem re
    toReactNode (RNText t) = toReactNode t

toReactElem :: ReactElement -> IO ReactElement'
toReactElem (ReactElement name props children) = join $ js_React_createElement
    <$> pure (toJSString name)
    <*> toPropsRef props
    <*> toReactNode children

toPropsRef :: Map Text Text -> IO (JSRef (Map Text Text))
toPropsRef = toJSRef_aeson
{-
toPropsRef m = do
    o <- newObj
    forM_ (Map.toList m) $ \(k, v) -> setProp k (toJSString v) o
    return o
-}

reactRender :: Element -> ReactElement -> IO ()
reactRender dom re = do
    re' <- toReactElem re
    js_React_render re' dom

main :: IO ()
main = do
    container <- getElementById "container"
    reactRender container $ ReactElement "i" []
        [ RNElement $ ReactElement "b" [("style", "text-decoration:underline")]
            [ RNText "Hello World 3"
            ]
        ]
