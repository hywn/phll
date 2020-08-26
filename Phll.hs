module Phll (png_rgba) where

import qualified Data.ByteString.Lazy   as B
import qualified Codec.Compression.Zlib as Z
import Data.Array -- Array
import Data.List  -- foldl'
import Data.Word  -- Word8, Word32
import Data.Bits  -- shiftR

-- fromIntegral takes some integral type and returns a 'general number' (Num)
-- and ig Num is able to magically transform into any numeric type (e.g. Word8) so thats nice

be32 :: Int -> [Word8]
be32 num = map (fromIntegral . shiftR num) [24, 16, 8, 0]

-- fromEnum turns Char into Int (unicode)

unicode :: String -> [Word8]
unicode str = map (fromIntegral . fromEnum) str

-- note that n doesn't need to be &0xffed because of types

-- wonder if i could flip the last foldl'
crc32lookup :: Array Word8 Word32
crc32lookup = listArray (0, 255) $ flip map [0..255] $
	\n -> foldl' (\nn _ ->
		if nn .&. 1 == 1
			then xor 0xedb88320 $ shiftR nn 1
			else shiftR nn 1
	) n [0..7]

crc32helper :: Word32 -> Word8 -> Word32
crc32helper crc byte = xor (shiftR crc 8) (crc32lookup ! n)
	where n = fromIntegral $ xor crc $ fromIntegral byte

crc32 :: [Word8] -> Word32
crc32 bytes = xor 0xffffffff $ foldl crc32helper 0xffffffff bytes

chunk :: String -> [Word8] -> [Word8]
chunk name bytes = (be32 $ length bytes) ++ all ++ (be32 $ fromIntegral $ crc32 all)
	where all = (unicode name) ++ bytes

hdr :: [Word8]
hdr = unicode "\x89PNG\r\n\x1a\n"

scanline :: [(Word8, Word8, Word8, Word8)] -> [Word8]
scanline row = [0] ++ concatMap (\(r, g, b, a) -> [r, g, b, a]) row

png_rgba :: [[(Word8, Word8, Word8, Word8)]] -> [Word8]
png_rgba pixels = hdr ++ ihdr ++ idat ++ iend
	where ihdr = chunk "IHDR" $ (be32 width) ++ (be32 height) ++ [8, 6, 0, 0, 0]
	      idat = chunk "IDAT" img_data
	      iend = chunk "IEND" []
	      width    = length pixels
	      height   = length $ head pixels
	      img_data = B.unpack $ Z.compress $ B.concat $ map B.pack $ map scanline pixels