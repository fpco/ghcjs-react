{-# LANGUAGE OverloadedStrings #-}

-- | Lucid-like elements and attributes.

module React.Lucid where

import Data.Text (Text)
import React.Builder
import React.Internal

div_ :: Monad m
     => ReactT state m a -> ReactT state m a
div_ = build "div"

img_ :: Monad m
     => ReactT state m a -> ReactT state m a
img_ = build "img"

p_ :: Monad m
   => ReactT state m a -> ReactT state m a
p_ = build "p"

h1_ :: Monad m
    => ReactT state m a -> ReactT state m a
h1_ = build "h1"

h2_ :: Monad m
    => ReactT state m a -> ReactT state m a
h2_ = build "h2"

h3_ :: Monad m
    => ReactT state m a -> ReactT state m a
h3_ = build "h3"

h4_ :: Monad m
    => ReactT state m a -> ReactT state m a
h4_ = build "h4"

h5_ :: Monad m
    => ReactT state m a -> ReactT state m a
h5_ = build "h5"

h6_ :: Monad m
    => ReactT state m a -> ReactT state m a
h6_ = build "h6"

ul_ :: Monad m
    => ReactT state m a -> ReactT state m a
ul_ = build "ul"

li_ :: Monad m
    => ReactT state m a -> ReactT state m a
li_ = build "li"

pre_ :: Monad m
     => ReactT state m a -> ReactT state m a
pre_ = build "pre"

span_ :: Monad m
      => ReactT state m a -> ReactT state m a
span_ = build "span"

strong_ :: Monad m
        => ReactT state m a -> ReactT state m a
strong_ = build "strong"

td_ :: Monad m
    => ReactT state m a -> ReactT state m a
td_ = build "td"

th_ :: Monad m
    => ReactT state m a -> ReactT state m a
th_ = build "th"

tr_ :: Monad m
    => ReactT state m a -> ReactT state m a
tr_ = build "tr"

table_ :: Monad m
       => ReactT state m a -> ReactT state m a
table_ = build "table"

tbody_ :: Monad m
       => ReactT state m a -> ReactT state m a
tbody_ = build "tbody"

thead_ :: Monad m
       => ReactT state m a -> ReactT state m a
thead_ = build "thead"


class_ :: Monad m
       => Text -> ReactT state m ()
class_ = attr "className"

id_ :: Monad m
    => Text -> ReactT state m ()
id_ = attr "id"

src_ :: Monad m
     => Text -> ReactT state m ()
src_ = attr "src"

title_ :: Monad m
       => Text -> ReactT state m ()
title_ = attr "title"

code_ :: Monad m
      => Text -> ReactT state m ()
code_ = attr "code"
