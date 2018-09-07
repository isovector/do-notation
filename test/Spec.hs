{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

import           Control.Monad (void, fail)
import           Control.Monad.Indexed
import           Control.Monad.Trans.Ix
import           Data.Coerce
import           Data.Kind (Type)
import           GHC.TypeLits (Nat, type (+))
import           Language.Haskell.DoNotation
import           Prelude hiding (Monad (..), pure)
import qualified System.IO as SIO
import           System.IO hiding (openFile, Handle)


------------------------------------------------------------------------------
-- | This module just exists to make sure 'iShouldCompile' actually compiles.
main :: IO ()
main = do
  pure ()


------------------------------------------------------------------------------
-- | Function that uses both monads and indexed monads in do blocks.
iShouldCompile :: IO ()
iShouldCompile = do
  putStrLn "hello"
  void $ runLinear $ do
    f <- openFile "not a real file" ReadMode
    closeFile f
    pure "goodbye"
  putStrLn $ do
    c <- ['a' .. 'z']
    pure c


------------------------------------------------------------------------------
-- | 'Linear' statically tracks the files you open and makes sure you close them
-- exactly one time.
newtype Linear (s :: Type)
               (i :: LinearState)
               (j :: LinearState) a = Linear
  { unsafeRunLinear :: Ix IO i j a
  }
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad)


------------------------------------------------------------------------------
-- | Run the underlying IO action if you've closed all the files you opened.
runLinear
    :: (forall s. Linear s ('LinearState 0 '[])
                           ('LinearState n '[]) a)
    -> IO a
runLinear = coerce


------------------------------------------------------------------------------
-- | Data kind for tracking our linear state as the index to 'Linear'.
data LinearState = LinearState
  { linearNextKey  :: Nat
  , linearOpenKeys :: [Nat]
  }


------------------------------------------------------------------------------
-- | A wrapped 'SIO.Handle' with a phantom parameter for the ST trick.
newtype Handle (s :: Type) key = Handle
  { unsafeGetHandle :: SIO.Handle
  }


------------------------------------------------------------------------------
-- | Open a file and track that you did it in the type system.
openFile
    :: FilePath
    -> IOMode
    -> Linear s ('LinearState next open)
                ('LinearState (next + 1) (next ': open))
                (Handle s next)
openFile = coerce SIO.openFile


------------------------------------------------------------------------------
-- | Close a file and prove it!
closeFile
    :: IsOpen key open ~ 'True
    => Handle s key
    -> Linear s ('LinearState next open)
                ('LinearState next (Close key open))
                ()
closeFile = coerce SIO.hClose


------------------------------------------------------------------------------
-- | Type family to detect if a file handle is already opened (ie. if it exists
-- in the open set.
type family IsOpen
      (key :: k)
      (ts :: [k]) :: Bool where
  IsOpen key '[]         = 'False
  IsOpen key (key ': ts) = 'True
  IsOpen key (_x  ': ts) = IsOpen key ts


------------------------------------------------------------------------------
-- | Type family to remove a file handle from the open set.
type family Close
      (key :: k)
      (ts :: [k]) :: [k] where
  Close key (key ': ts) = ts
  Close key (_x  ': ts) =
    _x ': Close key ts

