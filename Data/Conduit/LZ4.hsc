{-# LANGUAGE CPP, EmptyDataDecls, ScopedTypeVariables #-}
module Data.Conduit.LZ4 where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as U
import qualified Data.ByteString.Internal as I
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Data.IORef
import Data.Word
import Data.Monoid
import Data.Binary.Get
import Data.Binary.Put

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
foreign import ccall unsafe "lz4.h LZ4_decompress_safe_continue"
  c_decompressSafeContinue :: Ptr C_LZ4Stream_Decode -> CString -> Ptr Word8 -> CInt -> CInt -> IO CInt

compress :: MonadResource m => Conduit BS.ByteString m BS.ByteString
compress = do
  ioRef <- liftIO $ newIORef BS.empty
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
                  res <- I.createAndTrim (fromIntegral outlen + 8) $ \content -> do
                    size <- c_compressFastContinue stream cstring (content `plusPtr` 8) cintlen outlen 0
                    writeWords (word32be $ fromIntegral len) content
                    writeWords (word32be $ fromIntegral size) (content `plusPtr` 4)
                    {-poke (castPtr content) (fromIntegral len :: Word32) -- 4 bytes = decompressed size-}
                    {-poke (castPtr $ content `plusPtr` 4) (fromIntegral size :: Word32) -- 4 bytes = compressed size-}
                    putStrLn $ "COMPRESSION: decompressed size=" ++ show len ++ ", compressed size=" ++ show size
                    return $ fromIntegral size + 8
                  modifyIORef ioRef $ const val' -- use modify to enforce read for now, want to keep old bs alive for unsafeuse
                  return res
                yield res
                go
              Nothing -> return ()
      go)

word32be :: Word32 -> [Word8]
word32be w = runGet (replicateM 4 getWord8) (runPut (putWord32be w))
writeWords :: [Word8] -> Ptr Word8 -> IO ()
writeWords ws cstring = forM_ (zip [0..] ws) $ \(i, w) -> poke (cstring `plusPtr` i) w

decompress :: MonadResource m => Conduit BS.ByteString m BS.ByteString
decompress = do
  ioRef <- liftIO $ newIORef BS.empty
  bracketP
    c_createStreamDecode
    c_freeStreamDecode
    (\stream -> do
      let go buf = do
            val <- await
            case val of
              Just val' -> work (buf <> BSL.fromStrict val')
              Nothing -> return ()
          work bs = case runGetOrFail getFrame bs of
                Left _ -> go bs
                Right (buf', _, (decompressedSize, compressedSize, frame :: BS.ByteString)) -> do
                  liftIO $ putStrLn $ "DECOMPRESSION: decompressed size=" ++ show decompressedSize ++ ", compressed size=" ++ show compressedSize
                  res <- liftIO $ U.unsafeUseAsCString frame $ \cstring -> do
                    res <- I.createAndTrim (fromIntegral decompressedSize) $ \content -> do
                      fromIntegral `fmap` c_decompressSafeContinue stream cstring content (fromIntegral compressedSize) (fromIntegral decompressedSize)
                    modifyIORef ioRef $ const frame -- use modify to enforce read for now, want to keep old bs alive for unsafeuse
                    return res
                  yield res
                  work buf'
      go BSL.empty)
  where
  getFrame = do
    decompressedSize <- getWord32be
    compressedSize <- getWord32be
    bs <- getByteString $ fromIntegral compressedSize
    return (decompressedSize, compressedSize, bs)

