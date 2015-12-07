import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.Digest.Pure.MD5
import Data.List (find)

main :: IO ()
main = do
    s <- B.readFile "04.data"
    let s' = B.init s
    putStrLn $ show $ getCoin s'

getCoin :: B.ByteString -> Integer
getCoin s = case find (testNum s) [0..] of
    Nothing -> error "waaat"
    Just i  -> fromIntegral i

testNum :: B.ByteString -> Integer -> Bool
testNum secret n = take 6 (show $ md5 s) == "000000"
    where s = secret `B.append` mkByteString n

mkByteString :: Integer -> B.ByteString
mkByteString n = B.pack $ map (+48) $ digits n

digits :: Integer -> [Word8]
digits x = if x < 10
           then [fromIntegral x]
           else digits (x `div` 10) ++ [fromIntegral (x `mod` 10)]
