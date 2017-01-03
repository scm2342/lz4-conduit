{-# LANGUAGE CPP, EmptyDataDecls #-}
module Data.Conduit.LZ4 where

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.ByteString
import qualified Data.ByteString.Unsafe as U
import qualified Data.ByteString.Internal as I
import Foreign.C
import Foreign.Ptr
import Data.IORef
import Data.Word

#include <lz4.h>

data C_LZ4Stream
data C_LZ4Stream_Decode

foreign import ccall unsafe "lz4.h LZ4_createStream"
--LZ4_createStream() will allocate and initialize an `LZ4_stream_t` structure.
  c_createStream :: IO (Ptr C_LZ4Stream)

foreign import ccall unsafe "lz4.h LZ4_freeStream"
--LZ4_freeStream() releases its memory.
  c_freeStream :: Ptr C_LZ4Stream -> IO ()

foreign import ccall unsafe "lz4.h LZ4_compressBound"
--Provides the maximum size that LZ4 compression may output in a "worst case" scenario (input data not compressible)
  c_compressBound :: CInt -> IO CInt

foreign import ccall unsafe "lz4.h LZ4_compress_fast_continue"
{-Compress buffer content 'src', using data from previously compressed blocks as dictionary to improve compression ratio.
  Important : Previous data blocks are assumed to still be present and unmodified !
  'dst' buffer must be already allocated.
  If maxDstSize >= LZ4_compressBound(srcSize), compression is guaranteed to succeed, and runs faster.
  If not, and if compressed data cannot fit into 'dst' buffer size, compression stops, and function returns a zero.
  int LZ4_compress_fast_continue (LZ4_stream_t* streamPtr, const char* src, char* dst, int srcSize, int maxDstSize, int acceleration);-}
  c_compressFastContinue :: Ptr C_LZ4Stream -> CString -> Ptr Word8 -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "lz4.h LZ4_createStreamDecode"
  c_createStreamDecode :: IO (Ptr C_LZ4Stream_Decode)
foreign import ccall unsafe "lz4.h LZ4_freeStreamDecode"
  c_freeStreamDecode :: Ptr C_LZ4Stream_Decode -> IO ()
foreign import ccall unsafe "lz4.h LZ4_decompress_fast_continue"
  c_decompress_fast_continue :: Ptr C_LZ4Stream_Decode -> CString -> CString -> CInt -> CInt -> IO CInt

compress :: MonadResource m => Conduit ByteString m ByteString
compress = do
  ioRef <- liftIO $ newIORef empty
  bracketP
    c_createStream
    c_freeStream
    (\stream -> do
      let go = do
            val <- await
            case val of
              Just val' -> do
                res <- liftIO $ U.unsafeUseAsCStringLen val' $ \(cstring, len) -> do
                  let cintlen = fromIntegral len
                  outlen <- c_compressBound cintlen
                  res <- I.createAndTrim (fromIntegral outlen) $ \content -> fromIntegral `fmap` c_compressFastContinue stream cstring content cintlen outlen 0
                  modifyIORef ioRef $ const val' -- use modify to enforce read for now, want to keep old bs alive for unsafeuse
                  return res
                yield res
                go
              Nothing -> return ()
      go)
