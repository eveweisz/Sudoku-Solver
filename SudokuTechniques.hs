module SudokuTechniques where

import SudokuPuzzles
import Data.Matrix as M
import qualified Data.Vector as V
import Data.List as L

failSud :: Matrix [Int]
failSud = fromList 9 9 (repeat [0])

counter :: (Eq a) => [a] -> a -> Int
counter x n = length (filter (==n) x)

counter2 :: [[Int]] -> [Int]
counter2 row = [counter row [i] | i <- [1..(length row)]]

firlist :: Int -> [Int]
firlist i = 1 : [ j * srt i + 1 | j <- [1..srt i - 1]]
    where srt = floor . sqrt . fromIntegral

seclist :: Int -> [Int]
seclist i = [ j * srt i  | j <- [1..srt i]]
    where srt = floor . sqrt . fromIntegral

boxLists :: [a] -> Int -> [[a]]
boxLists x size
    | null x            = []
    | otherwise         = take (srt size) x : boxLists (drop (srt size) x) size
    where srt = floor . sqrt . fromIntegral

-- delete repeated number
delRepeat :: Int -> [Int] -> [Int]
delRepeat n x
    | length x == 1     = x
    | otherwise         = delete n x

removeInvalidsRow :: Int -> Int -> Matrix [Int] -> Matrix [Int]
removeInvalidsRow n row mat
    | counter2 rowList  !! (n-1)  == 1          = mapRow (\_ x -> delRepeat n x) row mat
    | otherwise                                 = mat
    where rowList = V.toList (getRow row mat)

removeInvalidsRow2 :: Int -> Matrix [Int] -> Matrix [Int]
removeInvalidsRow2 row mat = foldl (\x z -> removeInvalidsRow z row x) mat [1..(nrows mat)]

removeIRow :: Matrix [Int] -> Matrix [Int]
removeIRow mat = foldl (flip removeInvalidsRow2) mat [1..(nrows mat)]

removeICol :: Matrix [Int] -> Matrix [Int]
removeICol = M.transpose . removeIRow . M.transpose

removeInvalidsSub :: Int -> Matrix [Int] -> Matrix [Int]
removeInvalidsSub n box
    | (counter2 . toList) box !! (n-1) == 1    = mapPos (\(i,j) x -> delRepeat n x) box
    | otherwise                                = box

removeISub :: Matrix [Int] -> Matrix [Int]
removeISub mat = foldl (flip removeInvalidsSub) mat [1..nrows mat * nrows mat]

boxes :: Matrix [Int] -> [Matrix [Int]]
boxes mat = [removeISub (submatrix a b c d mat) | (a,b) <- zlist, (c,d) <- zlist]
    where zlist = zip (firlist (nrows mat)) (seclist (nrows mat))

removeIBox :: Matrix [Int] -> Matrix [Int]
removeIBox mat = foldl1 (<->) (map (foldl1 (<|>)) (boxLists (boxes mat) (nrows mat)))

-- remove invalid options in rows columns and matrices
removeInvalids :: Matrix [Int] -> Matrix [Int]
removeInvalids = removeIRow . removeICol . removeIBox

removeInvalids2 :: Matrix [Int] -> Matrix [Int]
removeInvalids2 mat
    | mat == removeInvalids mat = mat
    | otherwise                 = (removeInvalids2 . removeInvalids) mat

-- finding hidden singles 
hdCol1 :: Matrix [Int] -> Int -> Int -> Int -> Int
hdCol1 mat col ind num
    | all ((==1) . length) colList  = 0
    | otherwise                     = counter (filter ((>1) . length) colList !! ind) num
    where colList = V.toList (getCol col mat)

hdCol2 :: Matrix [Int] -> Int -> Int -> [Int]
hdCol2 mat col ind = map (hdCol1 mat col ind) [1..nrows mat]

hdCol3 :: Matrix [Int] -> Int -> [Int]
hdCol3 mat col
    | all ((==1) . length) colList   = []
    | otherwise = foldl1 (zipWith (+)) indList
    where colList = V.toList (getCol col mat)
          indList = [hdCol2 mat col ind | ind <- [0..length (filter ((>1) . length) colList) - 1]]    

hdSingles :: Matrix [Int] -> Int -> [Int]
hdSingles mat col = map (+1) (elemIndices 1 (hdCol3 mat col))

concat2 :: [Int] -> Int
concat2 [x] = x
concat2 _ = -1

hdIndex :: Matrix [Int] -> Int -> Int -> (Int,Int)
hdIndex mat col hds = (1 + head (elemIndices True (map (elem hds) colList)), col)
    where colList = V.toList (getCol col mat)

hdCol5 :: Matrix [Int] -> Int -> Matrix [Int]
hdCol5 mat i
    | null (hdSingles mat i) = mat
    | otherwise = setElem [head (hdSingles mat i)] (hdIndex mat i (head (hdSingles mat i))) mat

hdCol6 :: Matrix [Int] -> Matrix [Int]
hdCol6 mat = foldl hdCol5 mat [1..nrows mat]

hdsCol :: Matrix [Int] -> Matrix [Int]
hdsCol mat
    | hdCol6 mat == mat        = mat
    | otherwise                = hdsCol (hdCol6 mat)

hdsRow :: Matrix [Int] -> Matrix [Int]
hdsRow = M.transpose . hdsCol . M.transpose

boxes2 :: Matrix [Int] -> [Matrix [Int]]
boxes2 mat = [submatrix a b c d mat | (a,b) <- zlist, (c,d) <- zlist]
    where zlist = zip (firlist (nrows mat)) (seclist (nrows mat))

getBoxList :: Matrix [Int] -> Int -> [[Int]]
getBoxList mat i = toList (boxes2 mat !! i)

hdsBox2 :: Matrix [Int] -> Int -> Int -> Int -> Int
hdsBox2 mat i k n
    | all ((==1) . length) (getBoxList mat i) = 0
    | otherwise = counter (filter ((>1) . length) (getBoxList mat i)!!k) n

hdsBox3 :: Matrix [Int] -> Int -> Int -> [Int]
hdsBox3 mat i k = map (hdsBox2 mat i k) [1..nrows mat]

hdsBox4 :: Matrix [Int] -> Int -> [Int]
hdsBox4 mat i
    | all ((==1) . length) boxList   = []
    | otherwise = foldl1 (zipWith (+)) indList
    where boxList = getBoxList mat i
          indList = [hdsBox3 mat i k | k <- [0..length (filter ((>1) . length) boxList) - 1]]


boxIndex :: Matrix [Int] -> Int -> (Int,Int)
boxIndex mat pos = ((pos-1) `div` nrows mat + 1, (pos-1) `mod` nrows mat + 1)

boxIndex2 :: Matrix [Int] -> Int -> Int -> Int
boxIndex2 mat i hds = 1 + head (elemIndices True (map (elem hds) (getBoxList mat i)))

hdSingles2 :: Matrix [Int] -> Int -> [Int]
hdSingles2 mat i = elemIndices 1 (hdsBox4 mat i)

hdsBox5 :: Matrix [Int] -> Int -> [Int] -> Matrix [Int]
hdsBox5 mat i hList
    | null hList = box
    | otherwise  = setElem [head hList + 1] (boxIndex box (boxIndex2 mat i (head hList + 1))) box
    where box = boxes2 mat !! i

hdsBox6 :: Matrix [Int] -> [Matrix [Int]]
hdsBox6 mat = [hdsBox5 mat i (hdSingles2 mat i) | i <- [0..nrows mat - 1]]

hdsBox7 :: Matrix [Int] -> Matrix [Int]
hdsBox7 mat = foldl1 (<->) (map (foldl1 (<|>)) (boxLists (hdsBox6 mat) (nrows mat)))

hdsBox :: Matrix [Int] -> Matrix [Int]
hdsBox mat
    | hdsBox7 mat == mat    = mat
    | otherwise             = hdsBox (hdsBox7 mat)

hidSingles :: Matrix [Int] -> Matrix [Int]
hidSingles = removeInvalids2 . hdsBox . removeInvalids2 . hdsCol . removeInvalids2 . hdsRow

-- naked pairs and naked triples
-- pairs
findEquivRow :: Matrix [Int] -> Int -> Int -> [Int]
findEquivRow mat i k
    | length (filter (== row !! k) row) == 2        = row !! k
    | otherwise                                     = []
    where row = V.toList (getRow i mat)

findPairsRow :: Matrix [Int] -> Int -> [[Int]]
findPairsRow mat i = filter ((2==) . length) (nub $ map (findEquivRow mat i) [0..nrows mat - 1])

delPairs :: [Int] -> [Int] -> [Int]
delPairs p x
    | p == x || length x == 1       = x
    | otherwise                     = x \\ p

nakPair1 :: Matrix [Int] -> Int -> [Int] -> Matrix [Int]
nakPair1 mat row pair
    | null (findPairsRow mat row)      = mat
    | otherwise                        = mapRow (\_ x -> delPairs pair x) row mat

nakPair2 :: Matrix [Int] -> Int -> Matrix [Int]
nakPair2 mat row = foldl (`nakPair1` row) mat (findPairsRow mat row)

nakPairRow :: Matrix [Int] -> Matrix [Int]
nakPairRow mat = foldl nakPair2 mat [1..nrows mat]

nakPairCol :: Matrix [Int] -> Matrix [Int]
nakPairCol = M.transpose . nakPairRow . M.transpose

nakPairs :: Matrix [Int] -> Matrix [Int]
nakPairs mat
    | nakPair == mat      = mat
    | otherwise           = nakPairs nakPair
    where nakPair = (nakPairBox . nakPairCol . nakPairRow) mat

findEquivBox :: Matrix [Int] -> Int -> Int -> [Int]
findEquivBox mat i k
    | length (filter (== box !! k) box) == 2        = box !! k
    | otherwise                                     = []
    where box = getBoxList mat i

findPairsBox :: Matrix [Int] -> Int -> [[Int]]
findPairsBox mat i = filter ((2==) . length) $ nub $ map (findEquivBox mat i) [0..nrows mat - 1]

nakPairBox1 :: Matrix [Int] -> Int -> Matrix [Int]
nakPairBox1 mat i
    | null pairs            = boxes2 mat !! i
    | otherwise             = fromList srt srt boxlist
    where pairs = findPairsBox mat i
          srt = (floor . sqrt . fromIntegral . nrows) mat
          boxlist = map (delPairs $ head pairs) $ getBoxList mat i

nakPairBox2 :: Matrix [Int] -> [Matrix [Int]]
nakPairBox2 mat = [nakPairBox1 mat i | i <- [0..nrows mat - 1]]

nakPairBox :: Matrix [Int] -> Matrix [Int]
nakPairBox mat = foldl1 (<->) $ map (foldl1 (<|>)) $ boxLists (nakPairBox2 mat) (nrows mat)

-- triples
findTriplesRow1 :: Matrix [Int] -> Int -> [[Int]]
findTriplesRow1 mat i = filter (\x -> length x == 2 || length x  == 3) (V.toList (getRow i mat))

isTriple :: [[Int]] -> Bool
isTriple triplist
    | (length . nub . concat) triplist == 3        = True
    | otherwise                                    = False

findTriple :: [[Int]] -> [Int]
findTriple tripslist = (sort . nub . concat . concat) $ filter isTriple possibleTrips
    where possibleTrips = filter ((3==) . length) $ subsequences tripslist

delNaked :: [Int] -> [Int] -> [Int]
delNaked t x
    | x `isSubsequenceOf` t     = x
    | otherwise                 = x \\ t

remTriples1 :: Matrix [Int] -> Int -> Matrix [Int]
remTriples1 mat row
    | null triple     = mat
    | otherwise       = mapRow (\_ x -> delNaked triple  x) row mat
    where triple = findTriple $ findTriplesRow1 mat row

remTriplesRow :: Matrix [Int] -> Matrix [Int]
remTriplesRow mat = foldl remTriples1 mat [1..nrows mat]

remTriplesCol :: Matrix [Int] -> Matrix [Int]
remTriplesCol = M.transpose . remTriplesRow . M.transpose

findTriplesBox :: Matrix [Int] -> Int -> [[Int]]
findTriplesBox mat i = filter (\x -> length x == 2 || length x  == 3) box
    where box = getBoxList mat i

remTriplesBox :: Matrix [Int] -> Int -> Matrix [Int]
remTriplesBox mat i
    | null triples            = boxes2 mat !! i
    | otherwise               = fromList srt srt boxlist
    where triples = findTriple $ findTriplesBox mat i
          srt = (floor . sqrt . fromIntegral . nrows) mat
          boxlist = map (delNaked triples) $ getBoxList mat i

remT2 :: Matrix [Int] -> [Matrix [Int]]
remT2 mat = [remTriplesBox mat i | i <- [0..nrows mat - 1]]

remT3 :: Matrix [Int] -> Matrix [Int]
remT3 mat = foldl1 (<->) $ map (foldl1 (<|>)) $ boxLists (remT2 mat) (nrows mat)


nakTriples :: Matrix [Int] -> Matrix [Int]
nakTriples mat
    | nakTrip == mat      = mat
    | otherwise           = nakTriples nakTrip
    where nakTrip = (remT3 . remTriplesCol . remTriplesRow) mat

-- quadruples
findQuadruplesRow1 :: Matrix [Int] -> Int -> [[Int]]
findQuadruplesRow1 mat i = filter (\x -> length x == 2 || length x  == 3 || length x == 4) row
    where row = V.toList (getRow i mat)

isQuadruple :: [[Int]] -> Bool
isQuadruple quadlist
    | (length . nub . concat) quadlist == 4        = True
    | otherwise                                    = False

findQuad :: [[Int]] -> [Int]
findQuad quadlist = (sort . nub . concat . concat) $ filter isQuadruple possibleQuads
    where possibleQuads = filter ((4==) . length) $ subsequences quadlist

remQuads1 :: Matrix [Int] -> Int -> Matrix [Int]
remQuads1 mat row
    | null quad    = mat
    | otherwise    = mapRow (\_ x -> delNaked quad x) row mat
    where quad = findQuad $ findQuadruplesRow1 mat row

remQuadsRow :: Matrix [Int] -> Matrix [Int]
remQuadsRow mat = foldl remQuads1 mat [1..nrows mat]

remQuadsCol :: Matrix [Int] -> Matrix [Int]
remQuadsCol = M.transpose . remQuadsRow . M.transpose

findQuadsBox :: Matrix [Int] -> Int -> [[Int]]
findQuadsBox mat i = filter (\x -> length x == 2 || length x  == 3 || length x == 4) box
    where box = getBoxList mat i

remQuadsBox1 :: Matrix [Int] -> Int -> Matrix [Int]
remQuadsBox1 mat i
    | null quads                = boxes2 mat !! i
    | otherwise                 = fromList srt srt boxlist
    where quads = findQuad $ findQuadsBox mat i
          srt = (floor . sqrt . fromIntegral . nrows) mat
          boxlist = map (delNaked quads) $ getBoxList mat i

remQuadsBox2 :: Matrix [Int] -> [Matrix [Int]]
remQuadsBox2 mat = [remQuadsBox1 mat i | i <- [0..nrows mat - 1]]

remQuadsBox :: Matrix [Int] -> Matrix [Int]
remQuadsBox mat = foldl1 (<->) $ map (foldl1 (<|>)) $ boxLists (remQuadsBox2 mat) (nrows mat)


nakQuads :: Matrix [Int] -> Matrix [Int]
nakQuads mat
    | nakQuad == mat    = mat
    | otherwise         = nakQuads nakQuad
    where nakQuad = (remQuadsBox . remQuadsCol . remQuadsRow) mat
