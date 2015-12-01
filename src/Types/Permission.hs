{-# LANGUAGE MultiParamTypeClasses #-}
module Types.Permission
( Permission (..)
, wordToPermission
, permissionToWord
) where

import Data.Bits             ( testBit
                             , bit
                             , zeroBits
                             , (.|.)
                             )
import Data.Word             (Word8)
import Data.Convertible.Base
import Database.HDBC         (SqlValue ( SqlChar))
import Control.Monad         (liftM)

data Permission = Permission { read :: Bool
                             , write :: Bool
                             , execute :: Bool
                             }
                deriving (Show, Eq)

wordToPermission :: Word8
                 -> Permission
wordToPermission a =
    let r = testBit a 0
        w = testBit a 1
        x = testBit a 2
    in Permission r w x

permissionToWord :: Permission
                 -> Word8
permissionToWord (Permission r w x) =
    let r' = if r then bit 0 else zeroBits
        w' = if w then bit 1 else zeroBits
        x' = if x then bit 2 else zeroBits
    in r' .|. w' .|. x'

instance Convertible Permission SqlValue where
    safeConvert = liftM SqlChar
                . safeConvert
                . permissionToWord

instance Convertible SqlValue Permission where
    safeConvert (SqlChar c) = return . wordToPermission $ convert c
    safeConvert _ = Left (ConvertError "" "" "" "")
