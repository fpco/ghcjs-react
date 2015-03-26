{-# LANGUAGE RankNTypes #-}

-- | Basic lens type and functions.
--
-- The lens package should be a drop-in replacement for this.
--

module React.Lens
  (-- * Lens type
   Lens
   -- * Functions
  ,view
  ,set
  ,over)
  where

import Control.Applicative

-- |
--
-- __Purpose__
--
-- A value of type @Lens s t a b@ provides the following:
--
-- * A reference into the structure @s@ to read and update the value @a@ inside it.
-- * The possibility to change the type of @s@ to @t@ and the type of @a@ to @b@.
--
-- __The @Functor@ constraint__
--
-- Operations may do something more interesting inside the `f`
-- functor. For the purpose of this module and package, all the
-- functions below ('view', 'over', 'set') use a no-op functor
-- and therefore the above type is equivalent to:
--
-- @type Lens s t a b = (a -> b) -> (s -> t)@
--
-- But it is left generic for forward compatibilty with the lens
-- package.
--
-- __Example__
--
-- @
-- λ> data Person = Person Char Int deriving Show
-- λ> let _age f (Person x a) = fmap (\\b -> Person x b) (f a)
-- λ> view _age (Person 'a' 10)
-- 10
-- λ> over _age (+1) (Person 'a' 10)
-- Person 'a' 11
-- λ> set _age 100 (Person 'a' 10)
-- Person 'a' 100
-- λ>
-- @
--
-- __Laws__
--
-- 1) /Get-Put/: You get back what you put in.
--
-- @view l (set l v s) ≡ v@
--
-- 2) /Put-Get/: Putting back what you got doesn't change anything.
--
-- @set l (view l s) s ≡ s@
--
-- 3) /Put-Put/: Setting is idempotent.
--
-- @set l v (set l v s) ≡ set l v s@
--
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- Internal id functor.
newtype Id a = Id { runId :: a }

-- | Could use @DeriveFunctor@ here but that's not portable.
instance Functor Id where fmap f = Id . f . runId

-- | Get the @a@ inside the @s@.
view :: Lens s t a b -> s -> a
view l = getConst . l Const

-- | Modify the @a@ inside the @s@, optionally changing the types to
-- @b@ and @t@.
over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runId . l (Id . f)

-- | Set the @a@ inside the @s@, optionally changing the types to @b@
-- and @t@.
set :: Lens s t a b -> b -> s -> t
set l a = runId . l (Id . const a)
