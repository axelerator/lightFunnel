module Fast_PPM (make_ppm, save_ppm) where
 
import Data.Word
import qualified Data.ByteString as BIN
import Colour
 
quant8 :: Double -> Word8
quant8 x = floor $ x * 0xFF
 
cquant8 :: Colour -> [Word8]
cquant8 (Colour r g b) = [quant8 r, quant8 g, quant8 b]
 
string_to_bin :: String -> BIN.ByteString
string_to_bin = BIN.pack . map (fromIntegral . fromEnum)
 
header :: [[Colour]] -> BIN.ByteString
header pss =
  let nx = length $ head pss
      ny = length        pss
  in  string_to_bin $ "P6\n" ++ show nx ++ " " ++ show ny ++ " 255\n"
 
body :: [[Colour]] -> BIN.ByteString
body pss = BIN.pack $ concatMap (cquant8 . cclip) $ concat pss
 
make_ppm :: [[Colour]] -> BIN.ByteString
make_ppm pss = BIN.append (header pss) (body pss)
 
save_ppm :: FilePath -> [[Colour]] -> IO ()
save_ppm f pss = BIN.writeFile f (make_ppm pss)
