module Types
( Byte (..)
, Human (..)
) where

import Data.Int (Int64)

import Text.Printf

class Human f where
    toSize :: f -> String

data Byte = Byte Int64

instance Human Byte where
    toSize (Byte i) =
        let suffix = ("B" :: String)
            pref = ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"]

            pr :: Double
               -> [String]
               -> String
            pr d [] = printf "%.3f%s%s" d ("Yi" :: String) suffix
            pr d (s:sx) | abs d < 1024 = printf "%.1f%s%s" d s suffix
                        | otherwise = pr (d / 1024) sx
        in pr (fromIntegral i) pref
