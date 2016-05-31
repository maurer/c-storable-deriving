{-# LANGUAGE DefaultSignatures #-}
-- | This module provides the mechanical deriving
--   mechanism for `CStorable'.
module Foreign.CStorable.TypeClass where

import Foreign.Ptr
import Foreign.Storable
import GHC.Generics

-- | A wrapper class for the raw autoderivation functions,
--   representing what is necessary for the defaulted
--   `CStorable' methods.
--
-- No @(':+:')@ instance.
--
class GCStorable a where
  gcAlignment :: a x -> Int
  gcPeek      :: Int -> Ptr (a x)-> IO (a x)
  gcPoke      :: Int -> Ptr (a x) -> a x -> IO ()
  gcSizeOf    :: Int -> a x -> Int

  -- padding before the field to align from the given offset
  gcPadding   :: Int -> a x -> Int
  gcPadding off a = (gcAlignment a - off) `mod` gcAlignment a

instance GCStorable U1 where
  gcAlignment _ = 0
  gcPeek _ _    = return U1
  gcPoke _ _ _  = return ()
  gcSizeOf _ _  = 0
  gcPadding _ _ = 0

-- | Test
instance (GCStorable a, GCStorable b) => GCStorable (a :*: b) where
  gcAlignment _ = lcm (gcAlignment (undefined :: a x))
                      (gcAlignment (undefined :: b y))

  gcPeek off p = do
    a <- gcPeek off                    $ castPtr p
    b <- gcPeek (off + gcSizeOf off a) $ castPtr p
    return $ a :*: b

  gcPoke off p (a :*: b) = do
    gcPoke off                    (castPtr p) a
    gcPoke (off + gcSizeOf off a) (castPtr p) b

  gcSizeOf off _    = let
    a = undefined :: a x
    b = undefined :: b y
    off2 = off + gcSizeOf off a
    in gcSizeOf off a + gcSizeOf off2 b

instance (GCStorable a) => GCStorable (M1 i c a) where
  gcAlignment (M1 x)     = gcAlignment x
  gcPeek off p           = fmap M1 $ gcPeek off (castPtr p)
  gcPoke off p (M1 x)    = gcPoke off (castPtr p) x
  gcSizeOf off (M1 x)    = gcSizeOf off x
  gcPadding off (M1 x)   = gcPadding off x

instance (CStorable a) => GCStorable (K1 i a) where
  gcAlignment (K1 x)     = cAlignment x
  gcPeek off p           = fmap K1 $ cPeek (castPtr p `plusPtr` (off + gcPadding off (undefined :: K1 i a x)))
  gcPoke off p (K1 x)    = cPoke (castPtr p `plusPtr` (off + gcPadding off (undefined :: K1 i a x))) x
  gcSizeOf off (K1 x)    = gcPadding off (undefined :: K1 i a x) + cSizeOf x

-- | This typeclass is basically just a duplicate of `Storable'. It exists
-- because I can't easily modify `Storable', as it is part of base.
-- The difference (besides naming) is the use of @DefaultSignatures@, with
-- implementations provided by the (internal class) 'GCStorable'.
--
-- Example:
--
-- @
-- {-\# LANGUAGE <https://ocharles.org.uk/blog/posts/2014-12-16-derive-generic.html DeriveGeneric>, <http://dev.stephendiehl.com/hask/#deriveanyclass DeriveAnyClass> \#-}
--
-- import 'Foreign' (Storable(..))
-- import Foreign.CStorable (CStorable(..))
--
-- --| a two-dimensional point.
-- -- Compatible with both OSX's @CGPoint@ and Window's @POINT@.
-- data Point = Point
--  { x :: Double
--  , y :: Double
--  } deriving ('Generic','CStorable')
--
-- instance 'Storable' Point where
--  'peek'      = 'cPeek'
--  'poke'      = 'cPoke'
--  'alignment' = 'cAlignment'
--  'sizeOf'    = 'cSizeOf'
-- @
--
-- Or without @-XDeriveAnyClass@:
--
-- @
-- ...
--  } deriving (Generic)
-- instance CStorable Point
-- ..
-- @
--
-- the @Storable@ instance is equivalent to:
--
-- @
-- struct Point {
--     double x;
--     double y;
-- };
-- @
--
-- Only product types are supported (i.e. no @union@).
--
class CStorable a where
  cPeek              :: Ptr a -> IO a
  default cPeek      :: (Generic a, GCStorable (Rep a)) => Ptr a -> IO a
  cPeek p            = fmap to $ gcPeek 0 (castPtr p)

  cPoke              :: Ptr a -> a -> IO ()
  default cPoke      :: (Generic a, GCStorable (Rep a)) => Ptr a -> a -> IO ()
  cPoke p x          = gcPoke 0 (castPtr p) $ from x

  cAlignment         :: a -> Int
  default cAlignment :: (Generic a, GCStorable (Rep a)) => a -> Int
  cAlignment         = gcAlignment . from

  cSizeOf            :: a -> Int
  default cSizeOf    :: (Generic a, GCStorable (Rep a)) => a -> Int
  cSizeOf            = gcSizeOf 0 . from
