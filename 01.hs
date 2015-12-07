main :: IO ()
main = do
    s <- readFile "01.data"
    let s' = filter (`elem` "()") s
    putStrLn $ show $ countFloors s'
    putStrLn $ show $ getBasementIndex s'

countFloors :: String -> Integer
countFloors = foldr count 0
    where count '(' n = n + 1
          count ')' n = n - 1
          count _   n = n

getBasementIndex :: String -> Integer
getBasementIndex = fst . foldl count (0, 0)
    where count (idx, fl) c
            | fl < 0 = (idx, fl)
            | otherwise = case c of
                '(' -> (idx + 1, fl + 1)
                ')' -> (idx + 1, fl - 1)
                _   -> (idx, fl)
