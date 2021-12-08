import qualified Data.Text as T
import Data.Map (fromListWith, toList)
import qualified Data.List as S
import Debug.Trace

main :: IO ()
main = do
    inputs <- lines <$> readFile "../input.txt"
    let input = map read (map T.unpack (T.splitOn (T.pack ",") (T.pack (inputs !! 0)))) :: [Int]
    print (length (simulateDays 80 input))

    print (sum (updateFishList 256 (createFishList initFishList (frequency input))))

simulateDays :: Int -> [Int] -> [Int]
simulateDays 0 state = state
simulateDays x state = simulateDays (x - 1) (updatedState ++ spawnFish state)
 where
    updatedState = map updateState state

spawnFish :: [Int] -> [Int]
spawnFish state = replicate (length (filter (==0) state)) 8

updateState :: Int -> Int
updateState x
 | x == 0 = 6
 | otherwise = (x - 1)

frequency :: [Int] -> [(Int, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

initFishList = [0,1,2,3,4,5,6,7,8]

createFishList :: [Int] -> [(Int, Int)] -> [Int]
createFishList [] _ = []
createFishList x [] = replicate (length x) 0
createFishList (x:xs) (y:ys)
 | x == value = [freq] ++ createFishList xs ys
 | x /= value = [0] ++ createFishList xs ([y] ++ ys)
 where
    value = getFirst y
    freq = getSecond y

updateFishList :: Int -> [Int] -> [Int]
updateFishList 0 array = array
updateFishList days array = updateFishList (days - 1) (((take 6 (tail array)) ++ [(zeroes + (array !! 7))] ++ [array !! 8] ++ [zeroes]))
 where zeroes = head array

getFirst :: (Int, Int) -> Int
getFirst (x, _)= x
getSecond :: (Int, Int) -> Int
getSecond (_, x) = x


