import qualified Data.Text as T

main :: IO ()
main = do
    inputs <- lines <$> readFile "../input.txt"
    let input = coordinates inputs

    print (minimum (map (\a -> (distanceTo a input)) [0..1000]))
    print (minimum (map (\a -> (distanceToB a input)) [0..1000]))

coordinates :: [String] -> [Int]
coordinates [] = []
coordinates (x:xs) = split ++ coordinates xs
 where
    split = map read (map T.unpack (T.splitOn (T.pack ",") (T.pack x))) :: [Int]

alignCrabs :: [Int] -> [Int] -> Int
alignCrabs positions distance = 10

distanceTo :: Int -> [Int] -> Int
distanceTo _ [] = 0
distanceTo x v = sum (map (\a -> (abs (a - x))) v)

distanceToB :: Int -> [Int] -> Int
distanceToB _ [] = 0
distanceToB x v = sum (map (\a -> sum ([0..(abs (a - x))])) v)


