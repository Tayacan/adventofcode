import qualified Data.Map as M
import Data.List (stripPrefix, foldl')
import Control.Applicative ((<|>))

main :: IO ()
main = do
    s <- readFile "06.data"
    print $ sum $ M.elems $ foldl' doCommand initMap (map parseFullCommand $ lines s)

-- types
type Position = (Int, Int)
type LightMap = M.Map Position Integer

data Command = TurnOff | TurnOn | Toggle deriving (Show, Eq)
type Area = (Position, Position)
type FullCommand = (Command, Area)

-- command execution
doCommand :: LightMap -> FullCommand -> LightMap
doCommand lm (TurnOn , a) = foldr (\p m -> M.adjust (+1) p m) lm $ getIndices a
doCommand lm (TurnOff, a) = foldr (\p m -> M.adjust f    p m) lm $ getIndices a
    where f 0 = 0
          f n = n - 1
doCommand lm (Toggle , a) = foldr (\p m -> M.adjust (+2) p m)   lm $ getIndices a

getIndices :: Area -> [Position]
getIndices ((x0, y0), (x1, y1)) = [(x, y) | x <- [x0..x1], y <- [y0..y1]]

initMap :: LightMap
initMap = M.fromList $ [((x,y), 0) | x <- [0..999], y <- [0..999]]

-- command parsing
parseFullCommand :: String -> FullCommand
parseFullCommand s = case parseCommand s of
    Nothing      -> error "waaat"
    Just (c, s') -> (c, parseArea s')

parseCommand :: String -> Maybe (Command, String)
parseCommand s = (do
    s' <- stripPrefix "turn on " s
    return (TurnOn, s')) <|> (do
    s' <- stripPrefix "turn off " s
    return (TurnOff, s')) <|> (do
    s' <- stripPrefix "toggle " s
    return (Toggle, s'))

parseArea :: String -> Area
parseArea s = let [p0, _, p1] = words s
              in  (parsePos p0, parsePos p1)

parsePos :: String -> Position
parsePos s = let (x, y) = break (== ',') s
             in  (read x, read (tail y))
