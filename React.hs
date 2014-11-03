{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.Text as T
import Control.Concurrent.STM

data ReactElement_
type ReactElement' = JSRef ReactElement_

data ReactEvent_
type ReactEvent = JSRef ReactEvent_

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

foreign import javascript "React.createElement($1, $2, $3)"
    js_React_createElement
        :: JSString
        -> JSRef props
        -> ReactNode'
        -> IO ReactElement'

foreign import javascript "document.getElementById($1)"
    getElementById :: JSString -> IO Element

data ElemProps = ElemProps
    { epStyle :: Map Text Text
    , epEvents :: Map Text (Int, ReactEvent -> IO ())
    , epOtherProps :: Map Text Text
    -- ^ Cannot be: style, key
    }
    deriving Generic
instance Hashable ElemProps where
    hashWithSalt salt (ElemProps x y z) = hashWithSalt salt
        ( Map.toList x
        , Map.toList $ Map.map fst y
        , Map.toList z
        )

data ReactElement = ReactElement Int Text ElemProps (Vector ReactNode)

reactElement :: Text -> ElemProps -> Vector ReactNode -> ReactElement
reactElement name props children = ReactElement
    (hash (name, props, V.toList $ V.map rnHash children))
    name
    props
    children

data ReactNode = RNElement ReactElement
               | RNText Int Text

rnHash :: ReactNode -> Int
rnHash (RNElement (ReactElement h _ _ _)) = h
rnHash (RNText h _ ) = h

nodeElement :: ReactElement -> ReactNode
nodeElement = RNElement

nodeText :: Text -> ReactNode
nodeText t = RNText (hash t) t

instance ToReactNode ReactNode where
    toReactNode (RNElement re) = toReactNode =<< toReactElem re
    toReactNode (RNText _ t) = toReactNode t

toReactElem :: ReactElement -> IO ReactElement'
toReactElem (ReactElement h name props children) = join $ js_React_createElement
    <$> pure (toJSString name)
    <*> toPropsRef h props
    <*> toReactNode children

toPropsRef :: Int -> ElemProps -> IO (JSRef props)
toPropsRef key (ElemProps style events m) = do
    o <- newObj
    forM_ (Map.toList m) $ \(k, v) -> setProp k (toJSString v) o
    key' <- toJSRef key
    setProp ("key" :: JSString) key' o
    unless (Map.null style) $ do
        style' <- toJSRef_aeson style
        setProp ("style" :: JSString) style' o
    forM_ (Map.toList $ Map.map snd events) $ \(name, f) -> do
        let name' = T.concat ["on", T.toUpper $ T.take 1 name, T.drop 1 name]
        f' <- syncCallback1 AlwaysRetain True f
        setProp name' f' o
    return o

foreign import javascript "alert($1)"
    alert :: JSString -> IO ()

reactRender :: Element -> ReactElement -> IO ()
reactRender dom re = do
    re' <- toReactElem re
    js_React_render re' dom


class ToReactElement a where
    toReactElement :: TVar a -> a -> ReactElement

data MyState = MyState Int
    deriving Eq
instance ToReactElement MyState where
    toReactElement var (MyState i) = reactElement "i" (ElemProps [] [] [])
        [ nodeElement $ reactElement "b"
            (ElemProps
                [("textDecoration", "underline")]
                (Map.singleton "click" (i, const $ atomically $ do
                    MyState i <- readTVar var
                    writeTVar var $ MyState $ i + 1))
                [])
            [ nodeText $ "Total clicks: " `T.append` T.pack (show i)
            ]
        , nodeElement $ reactElement "span"
            (ElemProps [] [] [])
            [ nodeText "This span should never be modified."
            ]
        ]

renderThread dom state0 = do
    var <- newTVarIO state0
    let loop stateOld = do
            reactRender dom (toReactElement var stateOld)
            stateNew <- atomically $ do
                stateNew <- readTVar var
                check (stateOld /= stateNew)
                return stateNew
            loop stateNew
    loop state0

main :: IO ()
main = do
    container <- getElementById "container"
    renderThread container $ MyState 0
