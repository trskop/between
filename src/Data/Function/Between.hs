-- |
-- Module:       $HEADER$
-- Description:  Function between and variations.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
module Data.Function.Between
    (

    -- * Usage

    -- ** Composability
    --
    -- $composability

    -- ** Mapping inner value of newtype
    --
    -- $mapNewtypeExample

    -- * Between function combinator
    --
    -- | Captures common pattern of @\ g -> (f . g . h)@ where @f@ and @h@ are
    -- fixed parameters.
      between

    -- ** Derived Combinators
    , between2l
    , between3l
    )
    where


-- | Defined as @\\ f g -> (f .) . (. g)@.
between :: (c -> d) -> (a -> b) -> (b -> c) -> a -> d
between f g = (f .) . (. g)
{-# INLINE between #-}

-- | Defined as @\\ f g -> (f \`between\` g) \`between\` g@.
--
-- > flip (between2l id) = Data.Function.on
between2l :: (c -> d) -> (a -> b) -> (b -> b -> c) -> a -> a -> d
between2l f g = (f `between` g) `between` g
{-# INLINE between2l #-}

-- | Defined as @\\ f g -> (f \`between2l\` g) \`between\` g@ which is equal to
-- @\\ f g -> ((f \`between\` g) \`between\` g) \`between\` g@.
between3l :: (c -> d) -> (a -> b) -> (b -> b -> b -> c) -> a -> a -> a -> d
between3l f g = (f `between2l` g) `between` g
{-# INLINE between3l #-}

-- $composability
--
-- > (f . h) `between` (i . g) = (f `between` g) . (h `between` i)
--
-- This shows us that is possible to define @f `between` g@ and
-- @h \`between\` i@ separately, for reusability, and then compose them.
--
-- > (f `between` funOnX) `between` funOnY
-- >     = \ g x y -> f (g (funOnX x) (funOnY y))
--
-- As you can se now @g@ is a function that takes two parameters. Now we can
-- define @f \`between\` funOnX@ separately, then when ever we need we can
-- extend it to higher arity function by appending @(\`between\` funOnY)@.
-- Special case when @funOnY = funOnX@ is very interesting in example function
-- @Data.Function.on@ can be defined using 'between' as:
--
-- > on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- > on f g = (id `between` g `between` g) f
--
-- We can also define function @on3@ that takes function with arity three as
-- its first argument:
--
-- > on3 :: (b -> b -> b -> d) -> (a -> b) -> a -> a -> a -> d
-- > on f g = (id `between` g `between` g `between` g) f
--
-- Another interesting situation is when @f@ and @g@ in @f \`between\` g@ form
-- an isomorphism. We then can construct a mapping function that takes function
-- operating on one type and transform it in to a function that operates on a
-- different type. As we shown before it is also possible to map functions with
-- higher arity then one.
--
-- Simplicity of how 'between' combinator can be used to define set of
-- functions by reusing previous definitions makes it also very suitable for
-- usage in TemplateHaskell.

-- $mapNewtypeExample
--
-- When we use @f \`between\` g@ where @f@ and @g@ form an isomorphism of two
-- types, and if @f@ is a constructor and @g@ a selector of newtype, then
-- @f \`between\` g@ is a mapping function that allows us to manipulate value
-- wrapped inside a newtype.
--
-- > newtype T t a = T {fromT :: a}
-- >
-- > mapT
-- >     :: (a -> b)
-- >     -> T t a -> T t' b
-- > mapT = T `between` fromT
--
-- Note that @mapT@ above is generalized version of 'fmap'.
--
-- Interestingly, we can use 'between' to define higher order mapping functions
-- by simple chaining:
--
-- > mapT2
-- >     :: (a -> b -> c)
-- >     -> T t1 a -> T t2 b -> T t3 c
-- > mapT2 = mapT `between` fromT
-- >
-- > mapT3
-- >     :: (a -> b -> c -> d)
-- >     -> T t1 a -> T t2 b -> T t3 c -> T t4 d
-- > mapT3 = mapT2 `between` fromT
--
-- Here is another example with a little more complex type wrapped inside a
-- newtype:
--
-- > newtype T e a = T {fromT :: Either e a}
-- >
-- > mapT
-- >     :: (Either e a -> Either e' b)
-- >     -> T e a -> T e' b
-- > mapT = T `between` fromT
-- >
-- > mapT2
-- >     :: (Either e1 a -> Either e2 b -> Either e3 c)
-- >     -> T e1 a -> T e2 b -> T e3 c
-- > mapT2 = mapT `between` fromT
--
-- This last example is typical for monad transformers:
--
-- > newtype ErrorT e m a = ErrorT {runErrorT :: m (Either e a)}
-- >
-- > mapErrorT
-- >     :: (m (Either e a) -> m' (Either e' b))
-- >     -> ErrorT e m a -> ErrorT e' m' b
-- > mapErrorT = ErrorT `between` runErrorT
-- >
-- > mapErrorT2
-- >     :: (m1 (Either e1 a) -> m2 (Either e2 b) -> m3 (Either e3 c))
-- >     -> ErrorT e1 m1 a -> ErrorT e2 m2 b -> ErrorT e3 m3 c
-- > mapErrorT2 = mapErrorT `between` runErrorT
