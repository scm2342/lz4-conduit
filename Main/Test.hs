{-# Language TemplateHaskell #-}
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.LZ4
import Test.QuickCheck
import Data.ByteString.Arbitrary
import System.Exit
import Control.Monad
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

prop_lz4_identity (NonNegative a) (Positive n') (Positive m') (ABS10M bs) = ioProperty $ do
  res <- runResourceT $ yield bs $$ rechunk n =$= compress (Just a) =$= rechunk m =$= decompress =$= CL.consume
  return (bs == BS.concat res)
  where
  n = max 64 n'
  m = max 64 m'

prop_rechunk_identity (Positive n) (ABS bs) = runConduitPure $ do
  res <- yield bs $$ rechunk n =$= CL.consume
  return (bs == BS.concat res)

rechunk size = go BSL.empty
  where
  go buf = do
    bs <- await
    case bs of
      Nothing | BSL.null buf -> return ()
      Nothing -> yield $ BSL.toStrict buf
      Just bs' -> work (buf <> BSL.fromStrict bs')
  work buf
    | BSL.length buf < size = go buf
    | otherwise = let (t, d) = BSL.splitAt size buf in do
      yield $ BSL.toStrict t
      work d

return []

main :: IO ()
main = do
  res <- $forAllProperties (quickCheckWithResult (stdArgs {maxSuccess = 10000}))
  when (not res) exitFailure
