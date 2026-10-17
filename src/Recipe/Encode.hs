module Recipe.Encode 
  (encodeQuery) where 

import Data.Bits ((.&.), shiftR)
import Data.ByteString (unpack)
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)

import Recipe.Prelude

-- ----------------------------------------------------------------------------
-- URL Encoding

encodeQuery :: Text -> String
encodeQuery = concatMap encodeByte . unpack . TextEncoding.encodeUtf8

encodeByte :: Word8 -> String
encodeByte byte
  | byte == 0x20 = "+"
  | isUnreserved byte = [toEnum (fromIntegral byte)]
  | otherwise = ['%', hexDigit (byte `shiftR` 4), hexDigit (byte .&. 0x0f)]

isUnreserved :: Word8 -> Bool
isUnreserved byte =
  (byte >= 0x41 && byte <= 0x5a)
    || (byte >= 0x61 && byte <= 0x7a)
    || (byte >= 0x30 && byte <= 0x39)
    || byte == 0x2d
    || byte == 0x2e
    || byte == 0x5f
    || byte == 0x7e

hexDigit :: Word8 -> Char
hexDigit nibble
  | nibble < 10 = toEnum (fromEnum '0' + fromIntegral nibble)
  | otherwise = toEnum (fromEnum 'A' + fromIntegral nibble - 10)