import Data.List (sort)

main :: IO ()
main = do
    s <- readFile "02.data"
    print $ sum $ map wrap $ lines s
    print $ sum $ map ribbon $ lines s

wrap :: String -> Integer
wrap s = sum (map (*2) sides) + minimum sides
    where sides = [l * w, l * h, w * h]
          (l, w, h) = case break (== 'x') s of
                        (l', s') -> case break (== 'x') (tail s') of
                                        (w', h') -> (read l', read w', read $ tail h')

ribbon :: String -> Integer
ribbon s = volume + sum (map (*2) smallest)
    where smallest = take 2 $ sort [l, w, h]
          volume = l * w * h
          (l, w, h) = case break (== 'x') s of
                        (l', s') -> case break (== 'x') (tail s') of
                                        (w', h') -> (read l', read w', read $ tail h')
