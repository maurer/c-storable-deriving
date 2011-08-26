module Foreign.CStorable
(CStorable(..)
) where

import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Data.Word

class GCStorable a where
  gcPeek      :: Ptr (a x)-> IO (a x)
  gcPoke      :: Ptr (a x) -> a x -> IO ()
  gcAlignment :: a x -> Int
  gcSizeOf    :: a x -> Int

instance GCStorable U1 where
  gcPeek _      = return U1
  gcPoke _ _    = return ()
  gcAlignment _ = 0
  gcSizeOf _    = 0

padding :: (GCStorable a, GCStorable b) => a x -> b y -> Int
padding a b = let
  sizeA   = gcSizeOf a
  alignB  = gcAlignment b
  in ((alignB - sizeA) `mod` alignB)

offset :: (GCStorable a, GCStorable b) => a x -> b y -> Int
offset a b = padding a b + gcSizeOf a

instance (GCStorable a, GCStorable b) => GCStorable (a :*: b) where
  gcPeek p = do
    a <- gcPeek $ castPtr p
    b <- gcPeek $ castPtr p `plusPtr` offset a (undefined :: b x)
    return $ a :*: b
  gcPoke p (a :*: b) = do
    gcPoke (castPtr p) a
    gcPoke (castPtr (p `plusPtr` offset a b)) b
  gcAlignment _ = lcm (gcAlignment (undefined :: a x))
                      (gcAlignment (undefined :: b y))
  gcSizeOf _    = let
    a = undefined :: a x
    b = undefined :: b y
    in gcSizeOf a + gcSizeOf b + padding a b

instance (GCStorable a) => GCStorable (M1 i c a) where
  gcPeek p           = fmap M1 $ gcPeek (castPtr p)
  gcPoke p (M1 x)    = gcPoke (castPtr p) x
  gcAlignment (M1 x) = gcAlignment x
  gcSizeOf (M1 x)    = gcSizeOf x

instance (CStorable a) => GCStorable (K1 i a) where
  gcPeek p           = fmap K1 $ cPeek (castPtr p)
  gcPoke p (K1 x)    = cPoke (castPtr p) x
  gcAlignment (K1 x) = cAlignment x
  gcSizeOf (K1 x)    = cSizeOf x

class CStorable a where
  cPeek              :: Ptr a -> IO a
  default cPeek      :: (Generic a, GCStorable (Rep a)) => Ptr a -> IO a
  cPeek p            = fmap to $ gcPeek (castPtr p)

  cPoke              :: Ptr a -> a -> IO ()
  default cPoke      :: (Generic a, GCStorable (Rep a)) => Ptr a -> a -> IO ()
  cPoke p x          = gcPoke (castPtr p) $ from x

  cAlignment         :: a -> Int
  default cAlignment :: (Generic a, GCStorable (Rep a)) => a -> Int
  cAlignment         = gcAlignment . from

  cSizeOf            :: a -> Int
  default cSizeOf    :: (Generic a, GCStorable (Rep a)) => a -> Int
  cSizeOf            = gcAlignment . from

newtype StorableWrap a = Storable a
instance (Storable a) => CStorable (StorableWrap a) where
  cPeek p                 = fmap Storable $ peek (castPtr p)
  cPoke p (Storable x)    = poke (castPtr p) x
  cAlignment (Storable x) = alignment x
  cSizeOf (Storable x)    = sizeOf x
