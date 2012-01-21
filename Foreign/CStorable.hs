-- | This primarily exports the CStorable typeclass, which may have its
--   methods automatically defaulted if it has a Generic instance.
--   Then, this instance can be transfered via the `Storable' constructor.
module Foreign.CStorable
(CStorable(..),
 StorableWrap(..)
) where

import Foreign.CStorable.TypeClass
import Foreign.CStorable.BaseInstances
import Foreign.Storable
import Foreign.Ptr

-- | Applying the `Storable' constructor to something which is Storable
--   gives it a corresponding CStorable instance.
newtype StorableWrap a = Storable a

-- | Translates a Storable instance to a CStorable instance
instance (Storable a) => CStorable (StorableWrap a) where
  cPeek p                 = fmap Storable $ peek (castPtr p)
  cPoke p (Storable x)    = poke (castPtr p) x
  cAlignment (Storable x) = alignment x
  cSizeOf (Storable x)    = sizeOf x
