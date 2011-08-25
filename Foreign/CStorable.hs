module Foreign.CStorable
(CStorable(..)
) where

import Foreign.Ptr
import Foreign.Storable

class CStorable a where
  cPeek      :: Ptr a -> IO a
  cPoke      :: Ptr a -> a -> IO ()
  cAlignment :: a -> Int
  cSizeOf    :: a -> Int

instance (Storable a) => CStorable a where
  cPeek      = peek
  cPoke      = poke
  cAlignment = alignment
  cSizeOf    = sizeOf
