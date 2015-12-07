import qualified Data.Map as M

type Position = (Integer, Integer)
type HouseMap = M.Map Position Integer

main :: IO ()
main = do
    s <- readFile "03.data"
    let s' = filter (`elem` "<>^v") s
    putStrLn $ show $ houses s'
    putStrLn $ show $ housesWithRobot s'

housesWithRobot :: String -> Int
housesWithRobot s = M.size $ snd $ foldl doMoves (((0, 0), (0, 0)), M.empty) moves
    where doMoves :: ((Position, Position), HouseMap)
                  -> (Position -> Position, Position -> Position)
                  -> ((Position, Position), HouseMap)
          doMoves ((p1, p2), hs) (f1, f2) = let p1' = f1 p1
                                                p2' = f2 p2
                                            in ((p1', p2'), updateMap p2 (updateMap p1 hs))
          moves = mkPairs $ map move s

mkPairs :: [a -> a] -> [(a -> a, a -> a)]
mkPairs []         = []
mkPairs [f]        = [(f, id)]
mkPairs (f1:f2:fs) = (f1, f2) : mkPairs fs

houses :: String -> Int
houses s = M.size $ snd $ foldl doMoves ((0, 0), M.empty) moves
    where doMoves :: (Position, HouseMap)
                  -> (Position -> Position)
                  -> (Position, HouseMap)
          doMoves (p, hs) f = let p' = f p
                              in (p', updateMap p hs)
          moves = map move s

move :: Char -> Position -> Position
move '^' (x, y) = (x, y + 1)
move 'v' (x, y) = (x, y - 1)
move '<' (x, y) = (x - 1, y)
move '>' (x, y) = (x + 1, y)

updateMap :: Position -> HouseMap -> HouseMap
updateMap p = M.insertWith (+) p 1
