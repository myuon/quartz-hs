module Data.Vector.PushBack where

import Prelude hiding (length, read)
import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- push-back vector
data PBVector s a = PBVector (MutVar s (VM.MVector s a)) (MutVar s Int)

new :: PrimMonad m => Int -> m (PBVector (PrimState m) a)
new p = new' (p + 10)
  where new' p = PBVector <$> (newMutVar =<< VM.new p) <*> (newMutVar 0)

read :: PrimMonad m => PBVector (PrimState m) a -> Int -> m a
read (PBVector vref _) k = readMutVar vref >>= \vec -> VM.read vec k

length :: PrimMonad m => PBVector (PrimState m) a -> m Int
length (PBVector _ len) = readMutVar len

capacity :: PrimMonad m => PBVector (PrimState m) a -> m Int
capacity (PBVector vref _) = fmap VM.length $ readMutVar vref

write :: PrimMonad m => PBVector (PrimState m) a -> Int -> a -> m ()
write (PBVector vref _) i v = do
  vec <- readMutVar vref
  VM.write vec i v

push :: PrimMonad m => PBVector (PrimState m) a -> a -> m ()
push pvec@(PBVector vref uvec) v = do
  vec <- readMutVar vref
  len <- length pvec
  cap <- capacity pvec
  when (len == cap) $ do
    vec' <- VM.grow vec cap
    writeMutVar vref vec'

  write pvec len v
  modifyMutVar uvec (+ 1)

pop :: PrimMonad m => PBVector (PrimState m) a -> m (Maybe a)
pop (PBVector vref vlen) = do
  n <- readMutVar vlen
  if (n == 0)
    then return Nothing
    else do
      vec <- readMutVar vref
      writeMutVar vlen (n - 1)
      a <- VM.read vec (n - 1)
      return $ Just a

fromList :: PrimMonad m => [a] -> m (PBVector (PrimState m) a)
fromList xs = do
  vec  <- V.thaw $ V.fromList xs
  vec' <- VM.grow vec ((VM.length vec + 5) * 2)
  vref <- newMutVar vec'
  uref <- newMutVar $ VM.length vec
  return $ PBVector vref uref

toList :: PrimMonad m => PBVector (PrimState m) a -> m [a]
toList p = fmap V.toList $ toVector p

toVector :: PrimMonad m => PBVector (PrimState m) a -> m (V.Vector a)
toVector (PBVector t len) = do
  v <- readMutVar t
  l <- readMutVar len
  V.freeze $ VM.take l v
