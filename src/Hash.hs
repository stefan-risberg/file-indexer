module Hash
( hash
) where

import           Data.Conduit
import           Data.Conduit.Binary (sourceFile)
import           Data.LargeWord (Word128)
import           Data.ByteString (ByteString)
import           Data.Binary (decode)
import           Data.ByteString.Lazy (fromStrict)

import           Crypto.Hash.MD5 ( init
                                 , update
                                 , finalize
                                 , Ctx)

import           Control.Monad.Trans.Resource
import           Prelude hiding (init)


createHash :: MonadResource m
           => Sink ByteString m Word128
createHash =
    let c' :: MonadResource m
           => Ctx
           -> Sink ByteString m Word128
        c' ctx = do
            await
            >>= maybe (return $! decode $ fromStrict $ finalize ctx)
                      (\b -> c' (update ctx b))
    in c' init

hash :: FilePath
     -> IO Word128
hash fp = runResourceT $ sourceFile fp $$ createHash
