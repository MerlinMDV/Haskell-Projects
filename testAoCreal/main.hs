main = do
    x0 <- readFile "diagnostic.txt"
    let diagnostics = lines x0
    print diagnostics
    -- print (column 1 testNums)
    -- print (mostCommon (column 1 testNums))
    -- print (gammaRate testNums 5)
    -- print (epsilonRate (gammaRate testNums 5))
    print (powerConsumption testNums)
    print (powerConsumption diagnostics)

type Bit = Int
type Nums = [String]
type Column = [Char]

column :: Int -> Nums -> Column 
column c [] = []
column c (x:xs) = (x !! (c-1)) : column (c) xs

testNums :: Nums
testNums = ["00100","11110","10110"]

mostCommon :: Column -> Bit
mostCommon ns
    -- if amount of 1s greater than 0s then return 1
    -- im so sorry that you have to read this
    | length [x | x <- ns, (read [x]::Int) > 0] > length [x | x <- ns, (read [x]::Int) < 1] = 1
    | otherwise = 0

-- takes in Nums and length of each num
gammaRate :: Nums -> Int -> [Bit]
gammaRate _ 0 = []
gammaRate ns@(x:xs) l = mostCommon (column (length x - (l-1)) ns) : gammaRate ns (l-1)

-- takes in gammaRate result
epsilonRate :: [Bit] -> [Bit]
epsilonRate [] = []
epsilonRate (x:xs) 
    | x == 1 = 0 : epsilonRate xs
    | otherwise = 1 : epsilonRate xs

b2n :: [Int] -> Int
b2n [] = 0 
b2n (x:xs)
    | x == 1  =  1 + 2 * (b2n xs)
    | otherwise = 2 * (b2n xs)

powerConsumption :: Nums -> Int
powerConsumption ns@(x:xs) = b2n (reverse (gammaRate ns (length x))) * b2n (reverse (epsilonRate (gammaRate ns (length x))))

