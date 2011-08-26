module Foreign.CStorable.BaseInstances where

import Data.Word
import Data.Int

import Foreign.CStorable.TypeClass
import Foreign.C.Types
import Foreign.Storable
import System.Posix.Types
import Foreign.Ptr

#define C(x) \
instance CStorable x where\
  cPeek      = peek;\
  cPoke      = poke;\
  cAlignment = alignment;\
  cSizeOf    = sizeOf\

C(Bool)
C(Char)
C(Double)
C(Float)
C(Int)
C(Int8)
C(Int16)
C(Int32)
C(Int64)
C(Word)
C(Word8)
C(Word16)
C(Word32)
C(Word64)
C(CUIntMax)
C(CIntMax)
C(CUIntPtr)
C(CIntPtr)
C(CTime)
C(CClock)
C(CSigAtomic)
C(CWchar)
C(CSize)
C(CPtrdiff)
C(CDouble)
C(CFloat)
C(CULLong)
C(CLLong)
C(CULong)
C(CLong)
C(CUInt)
C(CInt)
C(CUShort)
C(CShort)
C(CUChar)
C(CSChar)
C(CChar)
C(IntPtr)
C(WordPtr)
C(Fd)
C(CRLim)
C(CTcflag)
C(CSpeed)
C(CCc)
C(CUid)
C(CNlink)
C(CGid)
C(CSsize)
C(CPid)
C(COff)
C(CMode)
C(CIno)
C(CDev)
-- TODO Figure out how to pull these types in
{-
C(CTimeval)
C(Event)
C(Event)
C(PollFd)
C((StablePtr a))
-}
C((Ptr a))
C((FunPtr a))
