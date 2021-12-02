main :: IO ()
main = do
    inputs <- lines <$> readFile "../input.txt"
    let nums = map read inputs :: [Int]
    let result = valueDecrease(nums)
    print (length [a | a <- result, a])
    let stackedValues = stackValuesByAThreeElementWindow(nums)
    print (length [a | a <- valueDecrease(stackedValues), a])
    
valueDecrease :: [Int] -> [Bool]
valueDecrease (x:y:xs)
 | x < y && xs == [] = [True]
 | x > y && xs == [] = [False]
 | x < y = [True] ++ valueDecrease([y] ++ xs)
 | otherwise = [False] ++ valueDecrease([y] ++ xs)
 
stackValuesByAThreeElementWindow :: [Int] -> [Int]
stackValuesByAThreeElementWindow (x:y:z:xs)
 | xs == [] = [x + y + z]
 | otherwise = [x + y + z] ++ stackValuesByAThreeElementWindow([y] ++ [z] ++ xs)