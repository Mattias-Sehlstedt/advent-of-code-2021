import qualified Data.Text as T

data BingoSlot = BingoSlot {number :: Int, taken :: Bool, xPos :: Int, yPos :: Int} deriving (Eq, Show)

main :: IO ()
main = do
    inputs <- words <$> readFile "../input.txt"
    let numbers = map read (map T.unpack (T.splitOn (T.pack ",") (T.pack (head inputs)))) :: [Int]
    let boards = map stringListToInt (splitEvery 25 (tail inputs))
    let bingoBoards = map (\a -> createBoard a 0) boards
    let result = playBingo numbers bingoBoards
    print(result)

stringListToInt :: [String] -> [Int]
stringListToInt list = map read list :: [Int]

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
 where (first,rest) = splitAt n list

playBingo :: [Int] -> [[BingoSlot]] -> Int
playBingo (x:xs) boards
 | any (==True) (map boardWin updatedBoards) = boardValue (getWinner updatedBoards) * x
 | otherwise = playBingo xs updatedBoards
 where updatedBoards = (map (\a -> updateBoard a x) boards)

getWinner :: [[BingoSlot]] -> [BingoSlot]
getWinner (x:xs)
 | boardWin(x) = x
 | xs == [] = []
 | otherwise = getWinner xs

createBoard :: [Int] -> Int -> [BingoSlot]
createBoard (x:xs) p
 | xs == [] = [BingoSlot x False (p `mod` 5) (p `div` 5)]
 | otherwise = [BingoSlot x False (p `mod` 5) (p `div` 5)] ++ createBoard xs (p + 1)

updateBoard :: [BingoSlot] -> Int -> [BingoSlot]
updateBoard (x:xs) n
 | xs == [] && number x == n = [x {taken = True}]
 | xs == [] = [x]
 | number x == n = [(x {taken = True})] ++ updateBoard xs n
 | otherwise = [x] ++ updateBoard xs n

boardWin :: [BingoSlot] -> Bool
boardWin board = (columnWin board [0..4] || rowWin board [0..4])

columnWin :: [BingoSlot] -> [Int] -> Bool
columnWin board (c:cs)
 | cs == [] && not (length (filter (\a -> (xPos a) == c && taken a) board) == 5) = False
 | length (filter (\a -> (xPos a) == c && taken a) board) == 5 = True
 | otherwise = columnWin board cs

rowWin :: [BingoSlot] -> [Int] -> Bool
rowWin board (r:rs)
 | rs == [] && not (length (filter (\a -> (yPos a) == r && taken a) board) == 5) = False
 | length (filter (\a -> (yPos a) == r && taken a) board) == 5 = True
 | otherwise = rowWin board rs

boardValue :: [BingoSlot] -> Int
boardValue [] = 0
boardValue (x:xs)
 | xs == [] && not (taken x) = number x
 | xs == [] && taken x = 0
 | not (taken x) = (number x) + boardValue xs
 | otherwise = 0 + boardValue xs


