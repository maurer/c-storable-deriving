module Foreign.CStorable
(CStorable(..)
) where

import Foreign.CStorable.TypeClass
import Foreign.CStorable.BaseInstances
import Foreign.Storable
import Foreign.Ptr

newtype StorableWrap a = Storable a
instance (Storable a) => CStorable (StorableWrap a) where
  cPeek p                 = fmap Storable $ peek (castPtr p)
  cPoke p (Storable x)    = poke (castPtr p) x
  cAlignment (Storable x) = alignment x
  cSizeOf (Storable x)    = sizeOf x
