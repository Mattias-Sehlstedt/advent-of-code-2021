import qualified Data.Char as T


main :: IO ()
main = do
    inputs <- lines <$> readFile "../input.txt"
    let diag = inputs
    let inputLength = ((length (diag !! 0)) - 1)
    let epsilon = binToDec (read (majorityEpsilon (powerConsumption diag [0..inputLength]) ((fromIntegral (length inputs))/2)))
    let gamma = binToDec (read (majorityGamma (powerConsumption diag [0..inputLength]) ((fromIntegral (length inputs))/2)))
    let oxy = binToDec (read (oxygen inputs 0))
    let carb = binToDec (read (carbon inputs 0))
    print(epsilon * gamma)
    print(oxy * carb)

powerConsumption :: [String] -> [Int] -> [Int]
powerConsumption a (x:xs)
 | xs == [] = [sumAtIndex a x]
 | otherwise = [sumAtIndex a x] ++ powerConsumption a xs

sumAtIndex :: [String] -> Int -> Int
sumAtIndex (x:xs) pos
 | xs == [] = T.digitToInt (x !! pos)
 | otherwise = T.digitToInt (x !! pos) + sumAtIndex xs pos

majorityEpsilon :: [Int] -> Float -> String
majorityEpsilon (x:xs) count
 | xs == [] && y >= count = "1"
 | xs == [] && y < count = "0"
 | y >= count = "1" ++ majorityEpsilon xs count
 | y < count = "0" ++ majorityEpsilon xs count
 where y = fromIntegral x

majorityGamma :: [Int] -> Float -> String
majorityGamma (x:xs) count
 | xs == [] && y >= count = "0"
 | xs == [] && y < count = "1"
 | y >= count = "0" ++ majorityGamma xs count
 | y < count = "1" ++ majorityGamma xs count
 where y = fromIntegral x

oxygen :: [String] -> Int -> String
oxygen x pos
 | length x == 1 = x !! 0
 | (fromIntegral (length (x))/2) <= fromIntegral (length (ones)) = oxygen ones (pos + 1)
 | otherwise = oxygen zeros (pos + 1)
 where
    ones  = filter (\x -> x !! pos == '1') x
    zeros = filter (\x -> x !! pos == '0') x

carbon :: [String] -> Int -> String
carbon x pos
 | length x == 1 = x !! 0
 | (fromIntegral (length (x))/2) > fromIntegral (length (ones)) = carbon ones (pos + 1)
 | otherwise = carbon zeros (pos + 1)
 where
    ones  = filter (\x -> x !! pos == '1') x
    zeros = filter (\x -> x !! pos == '0') x

binToDec :: Integral i => i -> i
binToDec 0 = 0
binToDec i = 2 * binToDec (div i 10) + (mod i 10)