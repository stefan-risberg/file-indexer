module Types.File
( File (..)
) where

import Data.Int (Int64)
import qualified Types.Permission as P
import Data.Time.Clock

import Prelude hiding (id)

data File = File { id         :: Int
                 , path       :: FilePath
                 , size       :: Int64
                 , accessTime :: UTCTime
                 , modTime    :: UTCTime
                 , user       :: P.Permission
                 , group      :: P.Permission
                 , other      :: P.Permission
                 }
