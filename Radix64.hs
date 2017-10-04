import Data.Map.Strict (Map, fromList, (!))
import Data.Word (Word8)
import Numeric (showIntAtBase, showHex)
import Data.Char (chr, intToDigit)
import Data.Bits
import Debug.Trace (trace)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BS (c2w, w2c)

base64AList :: [(Char, Word8)]
base64AList = combined
    where
      upperRange  = zip ['A'..'Z'] [00..25]
      lowerRange  = zip ['a'..'z'] [26..51]
      numberRange = zip ['0'..'9'] [52..61]
      miscRange   = [('+', 62), ('/', 63)]
      combined    = upperRange ++ lowerRange ++ numberRange ++ miscRange

fromBase64Map :: Map Char Word8
fromBase64Map = fromList base64AList

-- "Man" -> "TWFu"
-- "any carnal pleasure." -> "YW55IGNhcm5hbCBwbGVhc3VyZS4="
toBase64 :: B.ByteString -> C.ByteString
toBase64 input
  | input == B.empty = B.empty
  | otherwise        =
      if bLen == 3
        then (C.pack out) `C.append` (toBase64 bs)          
        else (C.pack out) `C.append` case bLen of
                                       0 -> C.empty
                                       1 -> C.pack ['=', '=']
                                       2 -> C.pack ['=']
    where
      (b, bs) = B.splitAt 3 input
      bList = B.unpack b
      bLen = length bList
      b0 = if bLen > 0 then bList !! 0 else 0
      b1 = if bLen > 1 then bList !! 1 else 0
      b2 = if bLen > 2 then bList !! 2 else 0
      o0  = (b0 .&. 0xFC) `shiftR` 2 -- 0xFC == 0b11111100
      o1a = (b0 `shiftL` 4) .&. 0x30 -- 0x30 == 0b00110000
      o1b = b1 `shiftR` 4
      o1  = o1a .|. o1b
      o2a = (b1 `shiftL` 2) .&. 0x3F -- 0x3F == 0b00111111
      o2b = b2 `shiftR` 6
      o2 = o2a .|. o2b
      o3 = b2 .&. 0x3F
      out = map (toBase64Map !) [o0, o1, o2, o3]

toBase64Map :: Map Word8 Char
toBase64Map = fromList $ map (\(x, y) -> (y, x)) base64AList

fromBase64 :: C.ByteString -> B.ByteString
fromBase64 input = stripSuffix $ fromBase64' input
    where
      output = fromBase64' input
      inputLen = C.length input
      outputLen = B.length output
      equalsCount
        | input    == C.empty  = 0
        | inputLen == 1 = if C.head input == '=' then 1 else 0
        | otherwise         =
            C.length $ C.filter (\x -> x == '=') $ C.drop (inputLen - 2) input
      stripSuffix = B.take (outputLen - equalsCount)

fromBase64' :: C.ByteString -> B.ByteString
fromBase64' input
  | input == C.empty = C.empty
  | otherwise        =
      (B.pack out) `B.append` (fromBase64' cs)
    where
      (c, cs) = C.splitAt 4 input
      cListRaw = C.unpack c
      cLenChecked = length cListRaw
      cList = map (fromBase64Map !) $ takeWhile (\x -> x /= '=') cListRaw
      cLen = length cList
      c0 = if cLen > 0 then cList !! 0 else 0
      c1 = if cLen > 1 then cList !! 1 else 0
      c2 = if cLen > 2 then cList !! 2 else 0
      c3 = if cLen > 3 then cList !! 3 else 0
      o0 = (c0 `shiftL` 2) .|. (c1 `shiftR` 4)
      o1 = (c1 `shiftL` 4) .|. ((c2 `shiftR` 2) .&. 0x0F) -- 0x0F == 0b00001111
      o2 = (c2 `shiftL` 6) .|. c3
      out = take cLen [o0, o1, o2]

-- @TODO Radix64 Checksum
