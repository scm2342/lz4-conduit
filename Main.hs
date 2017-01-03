import Data.Conduit.LZ4
import Data.Conduit
import Data.Conduit.Binary
import Control.Monad.Trans.Resource
import Control.Monad.Trans
import Data.ByteString
import Data.Conduit.List

main :: IO ()
main = do
  runResourceT $ sourceFile "foo" $$ compress =$= sinkFile "foobar"
  runResourceT $ sourceFile "foobar" $$ decompress =$= sinkFile "bar"
  {-runResourceT $ sourceFile "foo" $$ Data.Conduit.List.mapM_ (liftIO . Data.ByteString.putStrLn) =$= compress =$= compress =$= sinkFile "bar"-}
