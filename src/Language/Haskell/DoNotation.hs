{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

------------------------------------------------------------------------------
-- | This module provides new implementations for '(>>=)', '(>>)', 'pure' and
-- 'return' so that they will work simultaneously with both regular and indexed
-- monads.
--
-- Intended usage:
--
-- @@
--   {-# LANGUAGE RebindableSyntax #-}
--
--   import Language.Haskell.DoNotation
--   import Prelude hiding (Monad (..), pure)
-- @@
module Language.Haskell.DoNotation
  ( BindSyntax (..)
  , PureSyntax (..)
  , P.Monad ()
  , IxMonad ()
  ) where

import           Control.Monad.Indexed
import           Data.Coerce
import           Data.Kind (Constraint)
import           Data.Kind (Type)
import qualified Prelude as P
import           Prelude hiding (Monad (..), pure)
import           System.IO (IOMode (..))


------------------------------------------------------------------------------
-- | Typeclass that provides 'pure' and 'return'.
class PureSyntax (x :: Type -> Type) where
  pure :: a -> x a
  pure = return

  return :: a -> x a
  return = pure


instance {-# INCOHERENT #-}
      P.Monad m => PureSyntax m where
  pure = P.pure

instance (IxMonad m, j ~ i) => PureSyntax (m i j) where
  pure = ireturn


------------------------------------------------------------------------------
-- | Typeclass that provides '(>>=)' and '(>>)'.
class BindSyntax (x :: Type -> Type)
                 (y :: Type -> Type)
                 (z :: Type -> Type)
      | x y -> z
      , x z -> y
      , y z -> x where
  (>>=) :: x a -> (a -> y b) -> z b

  (>>) :: x a -> y b -> z b
  a >> b = a >>= const b

instance  (P.Monad m, x ~ m) => BindSyntax m x m where
  (>>=) = (P.>>=)

instance {-# INCOHERENT #-}
      ( IxMonad m
      , x ~ m i j
      , y ~ m j k
      , z ~ m i k
      ) => BindSyntax x y z where
  (>>=) = (>>>=)

