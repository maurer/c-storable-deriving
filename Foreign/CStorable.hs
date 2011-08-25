module Foreign.CStorable
(CStorable(..)
) where

import Foreign.Ptr

class CStorable a where
  cPeek      :: Ptr a -> IO a
  cPoke      :: Ptr a -> a -> IO ()
  cAlignment :: a -> Int
  cSizeOf    :: a -> Int
