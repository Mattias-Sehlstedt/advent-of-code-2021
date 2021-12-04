import qualified Data.Text as T

main :: IO ()
main = do
    inputs <- lines <$> readFile "../input.txt"
    let positions = inputs
    let result = movement positions
    print((getFirst result - getSecond result) * getThird result)

    let result2 = movementAim positions 0
    print(get2First result2 * get2Second result2)
    
movement :: [String] -> (Int, Int, Int)
movement (x:xs)
 | xs == [] && word == "forward" = (0, 0, amount)
 | xs == [] && word == "up" = (0, amount, 0)
 | xs == [] && word == "down" = (amount, 0, 0)
 | word == "forward" = add (0, 0, amount) (movement xs)
 | word == "up" = add (0, amount, 0) (movement xs)
 | word == "down" = add (amount, 0, 0) (movement xs)
 where
    packed = (T.splitOn (T.pack " ") (T.pack x))
    word = T.unpack (packed!!0)
    amount = read (T.unpack (packed!!1)) :: Int

movementAim :: [String] -> Int -> (Int, Int)
movementAim (x:xs) aim
 | xs == [] && word == "forward" = ((aim * amount), amount)
 | xs == [] && word == "up" = (0, 0)
 | xs == [] && word == "down" = (0, 0)
 | word == "forward" = add2 (aim * amount, amount) (movementAim xs aim)
 | word == "up" = add2 (0, 0) (movementAim xs (aim - amount))
 | word == "down" = add2 (0, 0) (movementAim xs (aim + amount))
 where
    packed = (T.splitOn (T.pack " ") (T.pack x))
    word = T.unpack (packed!!0)
    amount = read (T.unpack (packed!!1)) :: Int

add :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add (x, y, z) (a, b, c) = (x + a, y + b, z + c)
getFirst :: (Int, Int, Int) -> Int
getFirst (x, _, _)= x
getSecond :: (Int, Int, Int) -> Int
getSecond (_, x, _) = x
getThird :: (Int, Int, Int) -> Int
getThird (_, _, x) = x

add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (x, y) (a, b) = (x + a, y + b)
get2First :: (Int, Int) -> Int
get2First (x, _)= x
get2Second :: (Int, Int) -> Int
get2Second (_, x) = x