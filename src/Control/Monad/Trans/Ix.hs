{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Control.Monad.Trans.Ix
  ( Ix (..)
  , liftIx
  , unsafeLiftIx
  ) where

import           Control.Monad (MonadPlus (..))
import           Control.Monad.Indexed
import           Data.Coerce (coerce)
import           Data.Kind (Type)
import qualified Prelude as P
import           Prelude hiding (Monad (..), pure)


------------------------------------------------------------------------------
-- | The free indexed monad generated from a monad 'm'. Users are not expected
-- to use 'Ix' directly, but to newtype over it, specializing the kinds of 'i'
-- and 'j' as necessary.
--
-- GeneralizedNewtypeDeriving can be used to get the instances of 'IxFunctor',
-- 'IxPointed', 'IxApplicative', 'IxMonad', 'IxMonadZero' and 'IxMonadPlus' for
-- free.
newtype Ix (m :: Type -> Type) i j a = Ix
  { runIx :: m a
  }
  deriving (Functor, Applicative, P.Monad)

instance Functor m => IxFunctor (Ix m) where
  imap = fmap

instance Applicative m => IxPointed (Ix m) where
  ireturn = P.pure

instance Applicative m => IxApplicative (Ix m) where
  iap
      :: forall i j k a b
       . Ix m i j (a -> b)
      -> Ix m j k a
      -> Ix m i k b
  iap = coerce $ (<*>) @m @a @b

instance P.Monad m => IxMonad (Ix m) where
  ibind
      :: forall i j k a b
       . (a -> Ix m j k b)
      -> Ix m i j a
      -> Ix m i k b
  ibind = coerce $ (=<<) @m @a @b

instance MonadPlus m => IxMonadZero (Ix m) where
  imzero
      :: forall i j a
       . Ix m i j a
  imzero = coerce $ mzero @m @a

instance MonadPlus m => IxMonadPlus (Ix m) where
  implus
      :: forall i j a
       . Ix m i j a
      -> Ix m i j a
      -> Ix m i j a
  implus = coerce $ mplus @m @a


------------------------------------------------------------------------------
-- | Lift an 'm' action into 'Ix m', maintaining the current index.
liftIx :: m a -> Ix m i i a
liftIx = coerce

------------------------------------------------------------------------------
-- | Lift an 'm' action into 'Ix m', changing the current index. 'unsafeLiftIx'
-- is obviously unsafe due to the fact that it can arbitrarily change the
-- index.
unsafeLiftIx :: m a -> Ix m i j a
unsafeLiftIx = coerce

