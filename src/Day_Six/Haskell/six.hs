import qualified Data.Text as T
import Data.Map (fromListWith, toList)
import qualified Data.List as S
import Debug.Trace

main :: IO ()
main = do
    inputs <- lines <$> readFile "../input.txt"
    let input = map read (map T.unpack (T.splitOn (T.pack ",") (T.pack (inputs !! 0)))) :: [Int]
    print (length (simulateDays 80 input))

    print (sum (updateFishArray 256 (createFishArray initFishArray (frequency input))))

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

initFishArray = [0,1,2,3,4,5,6,7,8]

createFishArray :: [Int] -> [(Int, Int)] -> [Int]
createFishArray [] _ = []
createFishArray x [] = replicate (length x) 0
createFishArray (x:xs) (y:ys)
 | x == value = [freq] ++ createFishArray xs ys
 | x /= value = [0] ++ createFishArray xs ([y] ++ ys)
 where
    value = getFirst y
    freq = getSecond y

updateFishArray :: Int -> [Int] -> [Int]
updateFishArray 0 array = array
updateFishArray days array = updateFishArray (days - 1) (((take 6 (tail array)) ++ [(zeroes + (array !! 7))] ++ [array !! 8] ++ [zeroes]))
 where zeroes = head array

getFirst :: (Int, Int) -> Int
getFirst (x, _)= x
getSecond :: (Int, Int) -> Int
getSecond (_, x) = x


