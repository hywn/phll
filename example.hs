import qualified Data.ByteString.Lazy as B
import Phll

pixels = flip map [0..255] $
	\x -> flip map [0..255] $
		\y -> (255, x, y, 255)

main = B.writeFile "hello.png" $ B.pack $ Phll.png_rgba pixels