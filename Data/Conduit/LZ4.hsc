{-# LANGUAGE CPP, EmptyDataDecls #-}
module Data.Conduit.LZ4(compress, decompress) where

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
import Foreign.Marshal
import Data.Word
import Data.Monoid
import Data.Binary.Get
import Data.Binary.Put

#include <lz4.h>

data C_LZ4Stream

foreign import ccall unsafe "lz4.h LZ4_createStream"
  c_createStream :: IO (Ptr C_LZ4Stream)

foreign import ccall unsafe "lz4.h LZ4_freeStream"
  c_freeStream :: Ptr C_LZ4Stream -> IO ()

foreign import ccall unsafe "lz4.h LZ4_compressBound"
  c_compressBound :: CInt -> IO CInt

foreign import ccall unsafe "lz4.h LZ4_compress_fast_continue"
  c_compressFastContinue :: Ptr C_LZ4Stream -> CString -> Ptr Word8 -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "lz4.h LZ4_saveDict"
  c_saveDict :: Ptr C_LZ4Stream -> CString -> CInt -> IO CInt

foreign import ccall unsafe "lz4.h LZ4_decompress_safe_usingDict"
  c_decompressSafeUsingDict :: CString -> Ptr Word8 -> CInt -> CInt -> Ptr Word8 -> CInt -> IO CInt

-- | Compress a Data.ByteString stream using lz4 stream compression
compress
  :: MonadResource m
  => Maybe Int -- ^ Acceleration value. The higher the faster and less effective!
  -> Conduit BS.ByteString m BS.ByteString
compress acceleration = do
  bracketP
    ((,) <$> c_createStream <*> (mallocBytes (64 * 1024) :: IO CString))
    (\(stream, buf) -> c_freeStream stream >> free buf)
    (\(stream, dictBuf) -> do
      let go = do
            val <- await
            case val of
              Just val' -> do
                res <- liftIO $ U.unsafeUseAsCStringLen val' $ \(cstring, len) -> do
                  let cintlen = fromIntegral len
                  outlen <- c_compressBound cintlen
                  res <- I.createAndTrim (fromIntegral outlen + 8) $ \content -> do
                    size <- c_compressFastContinue stream cstring (content `plusPtr` 8) cintlen outlen $ maybe 0 fromIntegral acceleration
                    _ <- c_saveDict stream dictBuf (64 * 1024)
                    writeWords (word32be $ fromIntegral len) content
                    writeWords (word32be $ fromIntegral size) (content `plusPtr` 4)
                    {-putStrLn $ "COMPRESSION: decompressed size=" ++ show len ++ ", compressed size=" ++ show size-}
                    return $ fromIntegral size + 8
                  return res
                yield res
                go
              Nothing -> return ()
      go)
  where
  word32be :: Word32 -> [Word8]
  word32be w = runGet (replicateM 4 getWord8) (runPut (putWord32be w))
  writeWords :: [Word8] -> Ptr Word8 -> IO ()
  writeWords ws cstring = forM_ (zip [0..] ws) $ \(i, w) -> poke (cstring `plusPtr` i) w

decompress
  :: MonadResource m
  => Conduit BS.ByteString m BS.ByteString
decompress = do
  bracketP
    (mallocBytes (64 * 1024) :: IO (Ptr Word8))
    free
    (\dictBuf -> do
      let go dictSize buf = do
            val <- await
            case val of
              Just val' -> work dictSize (buf <> BSL.fromStrict val')
              Nothing | BSL.null buf -> return ()
              Nothing -> fail "decompress buff not empty but stream empty!"
          work dictSize bs = case runGetOrFail getFrame bs of
                Left _ -> go dictSize bs
                Right (buf', _, (decompressedSize, compressedSize, frame)) -> do
                  {-liftIO $ putStrLn $ "DECOMPRESSION: decompressed size=" ++ show decompressedSize ++ ", compressed size=" ++ show compressedSize-}
                  (res, size) <- liftIO $ U.unsafeUseAsCString frame $ \cstring -> do
                    I.createAndTrim' (fromIntegral decompressedSize) $ \content -> do
                      size <- c_decompressSafeUsingDict cstring content (fromIntegral compressedSize) (fromIntegral decompressedSize) dictBuf dictSize
                      let sizeInt = fromIntegral size
                      copyBytes dictBuf content $ min sizeInt $ 64 * 1024
                      return (0, sizeInt, size)
                  yield res
                  work size buf'
      go 0 BSL.empty)
  where
  getFrame = do
    decompressedSize <- getWord32be
    compressedSize <- getWord32be
    bs <- getByteString $ fromIntegral compressedSize
    return (decompressedSize, compressedSize, bs)

