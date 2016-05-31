# c-storable-deriving

This library is intended to make generating C-like storable instances from datatypes easy.

## Example

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics (Generic(..))
import Foreign (Storable(..))
import Foreign.CStorable (CStorable(..))

-- | a two-dimensional point.
-- Compatible with both OSX's @CGPoint@ and Window's @POINT@.
data Point = Point
 { x :: Double
 , y :: Double
 } deriving (Generic, CStorable)

instance Storable Point where
 peek      = cPeek
 poke      = cPoke
 alignment = cAlignment
 sizeOf    = cSizeOf
```

See the haddocks for further details: [Foreign.CStorable](https://hackage.haskell.org/package/c-storable-deriving/docs/Foreign-CStorable.html)
