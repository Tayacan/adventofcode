main :: IO ()
main = do
    s <- readFile "05.data"
    putStrLn $ show $ countNice $ lines s

countNice :: [String] -> Integer
countNice = length'. filter req
    where req s = hasRepeatingPair s &&
                  hasSep1 s

length' :: [a] -> Integer
length' = foldr (\_ acc -> acc + 1) 0

hasRepeatingPair :: String -> Bool
hasRepeatingPair s = any (uncurry pairRepeats) $ toTest ps
    where ps = mkPairs s
          toTest [] = []
          toTest (x:xs) = (x, xs) : toTest xs

hasSep1 :: String -> Bool
hasSep1 s = any (\(x,_,z) -> x == z) triples
    where triples = mkTriples s

mkTriples :: String -> [(Char, Char, Char)]
mkTriples []         = []
mkTriples [_]        = []
mkTriples [_,_]      = []
mkTriples (x:y:z:xs) = (x,y,z) : mkTriples (y:z:xs)

pairRepeats :: (Char, Char) -> [(Char, Char)] -> Bool
pairRepeats p []     = False
pairRepeats p (q:qs) = pairRepeats' p qs

pairRepeats' :: (Char, Char) -> [(Char, Char)] -> Bool
pairRepeats' p [] = False
pairRepeats' p (q:qs) = p == q || pairRepeats' p qs

hasThreeVowels :: String -> Bool
hasThreeVowels s = foldr countVowels 0 s > 2
    where countVowels c n = if c `elem` "aeiou"
                            then n + 1
                            else n

hasDouble :: String -> Bool
hasDouble s = any (\(x,y) -> x == y) $ mkPairs s

noBad :: String -> Bool
noBad s = all (\p -> not (p `elem` bad)) $ mkPairs s

bad :: [(Char, Char)]
bad = [('a', 'b')
      ,('c', 'd')
      ,('p', 'q')
      ,('x', 'y')
      ]

mkPairs :: String -> [(Char, Char)]
mkPairs []       = []
mkPairs [x]      = []
mkPairs (x:y:xs) = (x, y) : mkPairs (y:xs)
