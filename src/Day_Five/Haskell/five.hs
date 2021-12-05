import qualified Data.Text as T
import Data.Map (fromListWith, toList)

data Position = Position {xPos :: Int, yPos :: Int} deriving (Eq, Ord, Show)
data VentLine = VentLine {posOne :: Position, posTwo :: Position, range :: [Position]} deriving Show

main :: IO ()
main = do
    inputs <- lines <$> readFile "../input.txt"
    let characters = coordinates inputs

    let part1VentLines = initVentLines nonDiagVentRange characters
    let count = frequency (concat (map range part1VentLines))
    print(length (filter (>=2) (map getSecond count)))

    let part2ventLines = initVentLines ventRange characters
    let count = frequency (concat (map range part2ventLines))
    print(length (filter (>=2) (map getSecond count)))

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

getFirst (a, _) = a
getSecond (_, a) = a

initVentLines :: (Position -> Position -> [Position]) -> [[Int]] -> [VentLine]
initVentLines _ [] = []
initVentLines func (x:y:xs) = [ventLine] ++ initVentLines func xs
 where
    posOne = Position (x !! 0) (x !! 1)
    posTwo = Position (y !! 0) (y !! 1)
    ventLine = VentLine posOne posTwo (func posOne posTwo)

ventRange :: Position -> Position -> [Position]
ventRange posOne posTwo
 | (xPos posOne < xPos posTwo) && (yPos posOne < yPos posTwo) = positions [(xPos posOne)..(xPos posTwo)] [(yPos posOne)..(yPos posTwo)]
 | (xPos posOne < xPos posTwo) && (yPos posOne > yPos posTwo) = positions [(xPos posOne)..(xPos posTwo)] [(yPos posOne), ((yPos posOne) - 1)..(yPos posTwo)]
 | (xPos posOne > xPos posTwo) && (yPos posOne < yPos posTwo) = positions [(xPos posTwo)..(xPos posOne)] [(yPos posTwo), ((yPos posTwo) - 1)..(yPos posOne)]
 | (xPos posOne > xPos posTwo) && (yPos posOne > yPos posTwo) = positions [(xPos posTwo)..(xPos posOne)] [(yPos posTwo)..(yPos posOne)]
 | xPos posOne > xPos posTwo = positions [(xPos posTwo)..(xPos posOne)] (replicate (length [(xPos posTwo)..(xPos posOne)]) (yPos posOne))
 | xPos posOne < xPos posTwo = positions [(xPos posOne)..(xPos posTwo)] (replicate (length [(xPos posOne)..(xPos posTwo)]) (yPos posOne))
 | yPos posOne > yPos posTwo = positions (replicate (length [(yPos posTwo)..(yPos posOne)]) (xPos posOne)) [(yPos posTwo)..(yPos posOne)]
 | yPos posOne < yPos posTwo = positions (replicate (length [(yPos posOne)..(yPos posTwo)]) (xPos posOne)) [(yPos posOne)..(yPos posTwo)]

nonDiagVentRange :: Position -> Position -> [Position]
nonDiagVentRange posOne posTwo
 | (xPos posOne /= xPos posTwo) && (yPos posOne /= yPos posTwo) = []
 | xPos posOne > xPos posTwo = positions [(xPos posTwo)..(xPos posOne)] (replicate (length [(xPos posTwo)..(xPos posOne)]) (yPos posOne))
 | xPos posOne < xPos posTwo = positions [(xPos posOne)..(xPos posTwo)] (replicate (length [(xPos posOne)..(xPos posTwo)]) (yPos posOne))
 | yPos posOne > yPos posTwo = positions (replicate (length [(yPos posTwo)..(yPos posOne)]) (xPos posOne)) [(yPos posTwo)..(yPos posOne)]
 | yPos posOne < yPos posTwo = positions (replicate (length [(yPos posOne)..(yPos posTwo)]) (xPos posOne)) [(yPos posOne)..(yPos posTwo)]

positions :: [Int] -> [Int] -> [Position]
positions [] [] = []
positions (x:xs) (y:ys) = [Position x y] ++ positions xs ys

coordinates :: [String] -> [[Int]]
coordinates [] = []
coordinates (x:xs) = coordinatesHelper noArrow ++ coordinates xs
 where
    noArrow = Prelude.map T.unpack (T.splitOn (T.pack " -> ") (T.pack x))

coordinatesHelper :: [String] -> [[Int]]
coordinatesHelper [] = []
coordinatesHelper (x:xs) = [noComma] ++ coordinatesHelper xs
 where
    noComma = Prelude.map read (Prelude.map T.unpack (T.splitOn (T.pack ",") (T.pack x))) :: [Int]


