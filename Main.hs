import Data.Conduit.LZ4
import Data.Conduit
import Data.Conduit.Binary
import Control.Monad.Trans.Resource
import Control.Monad.Trans
import Data.ByteString
import Data.Conduit.List
import System.Environment

main :: IO ()
main = do
  [fin, ftemp, fout] <- getArgs
  runResourceT $ sourceFile fin $$ compress =$= sinkFile ftemp
  runResourceT $ sourceFile ftemp $$ decompress =$= sinkFile fout
