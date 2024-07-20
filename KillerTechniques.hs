module KillerTechniques where
import Sudoku
import Data.List as L
import Data.Matrix as M
import KillerCages
import KillerPuzzles
import SudokuTechniques


combinations :: Int -> Int -> Int -> [[Int]]
combinations total cagelen size
    | cagelen == 0         = [[]]
    | otherwise            = nub [sort (x : xs) | x <- [1..size], xs <- combinations (total - x) (cagelen - 1) size, x + sum xs == total, x `notElem` xs]

findBaseCell :: [[Int]] -> Int -> [[Int]]
findBaseCell ops bsize = [ops !! i | i <- findIndices ((==bsize) . length) ops]

newCombs1 :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
newCombs1 bsize tot clen size basec = concatMap newcombs [0..bsize - 1]
    where newcombs i = combinations (tot - head basec !! i) (clen - 1) size

newCombs2 :: KillerSudoku -> Int -> Int -> [[Int]]
newCombs2 ks n bsize = newCombs1 bsize (getTotal c) (cLen c) (nrows (getSudoku ks)) basec
    where c = getCages ks !! n
          basec = findBaseCell (cageGetElem ks n) bsize

dropAt :: Int -> [[Int]] -> [[Int]]
dropAt i x = take i x ++ drop (i+1) x

isCombValid :: [Int] -> [[Int]] -> Bool
isCombValid [x] othercells                  = any (\y -> x `elem` y) othercells
isCombValid (x:xs) othercells
    | all (\y -> x `notElem` y) othercells  = False
    | length (ind x) == length othercells   = isCombValid xs othercells
    | otherwise                             = any (isCombValid xs . (`dropAt` othercells)) (ind x)
    where ind x = findIndices (elem x) othercells

validCombos :: [[Int]] -> [[Int]] -> [[Int]]
validCombos [] othercells                  = []
validCombos (x:xs) othercells
    | x `isCombValid` othercells              = x : validCombos xs othercells
    | otherwise                              = validCombos xs othercells

newCombsSingle :: KillerSudoku -> Int -> [[Int]]
newCombsSingle ks n = validCombos (newCombs2 ks n 1) (filter (/= head basec) (cageGetElem ks n))
    where basec = findBaseCell (cageGetElem ks n) 1

delInvalidCs :: [Int] -> [Int] -> [[Int]] -> Int -> [Int]
delInvalidCs basec cell posscells size
    | basec == cell     = cell
    | otherwise         = cell \\ ([1..size]\\ sort (concat posscells))

delInvalidsSingle :: KillerSudoku -> Int -> Int -> [Int]
delInvalidsSingle ks n k = delInvalidCs basec (cge !! k) (newCombsSingle ks n) size
    where cge = cageGetElem ks n
          size = nrows $ getSudoku ks
          basec = head $ findBaseCell cge 1

delInvalids :: KillerSudoku -> Int -> Int -> Int -> [Int]
delInvalids ks n k bsize = delInvalidCs basec (cge !! k) (newCombs2 ks n bsize) size
    where cge = cageGetElem ks n
          size = nrows $ getSudoku ks
          basec = head $ findBaseCell cge bsize

delInvalidsCs2 :: [Int] -> [Int] -> [[Int]] -> Int -> [Int]
delInvalidsCs2 basec cell posscells size = cell \\ ([1..size]\\ sort (concat posscells))

delInvalidsAlt :: KillerSudoku -> Int -> Int -> Int -> [Int]
delInvalidsAlt ks n m bsize = delInvalidsCs2 basec (cge !! m) (newCombs2 ks n bsize) size
    where cge = cageGetElem ks n
          size = nrows $ getSudoku ks
          basec = head $ findBaseCell cge bsize

setNewCombs :: KillerSudoku -> Int -> Int -> Int -> Int -> KillerSudoku
setNewCombs ks n k m bsize
    | null b                 = ks
    | bsize == 1             = KillerSudoku (setElem (delInvalidsSingle ks n k) (co !! k) s) c
    | length b == length co  = KillerSudoku (setElem (delInvalidsAlt ks n m bsize) (co !! m) s) c
    | otherwise              = KillerSudoku (setElem (delInvalids ks n k bsize) (co !! k) s) c
    where b = findBaseCell (cageGetElem ks n) bsize
          co = getCells (getCages ks !! n)
          s = getSudoku ks
          c = getCages ks


setCombs1 :: KillerSudoku -> Int -> Int -> Int -> KillerSudoku
setCombs1 ks n k bsize = foldl (\ks' m ->setNewCombs ks' n k m bsize) ks lst
    where lst = [1..length (cageGetElem ks n) - 1]

setCombs2 :: KillerSudoku -> Int -> Int -> KillerSudoku
setCombs2 ks n bsize = foldl (\ks' k -> setCombs1 ks' n k bsize) ks lst
    where lst = [0..length (cageGetElem ks n) - 1]

setCombs3 :: KillerSudoku -> Int -> KillerSudoku
setCombs3 ks bsize = foldl (\ks' n -> setCombs2 ks' n bsize) ks lst
    where lst = [0..length (getCages ks) - 1]

setCombs :: KillerSudoku -> KillerSudoku
setCombs ks = foldl setCombs3 ks [1..nrows (getSudoku ks) - 1]

-- rule of 45 (for a 3x3) (so also rule of 10, 136)
boxCoords :: Int -> [[(Int,Int)]]
boxCoords size = [[(a,b) | a <- lst i , b <- lst j] | i <- [0..srt - 1], j <- [0..srt - 1]]
    where f = firlist size
          s = seclist size
          srt = (floor . sqrt . fromIntegral) size
          lst x = [f !! x..s !! x]

cellCoords :: KillerSudoku -> [[(Int,Int)]]
cellCoords ks = map cells [0..length (getCages ks) - 1]
    where cells i = getCells $ getCages ks !! i

-- row innie and outies
coordsRowAny :: Int -> [(Int,Int)] -> Bool
coordsRowAny row = any (\ (a,b) -> a == row)

coordsRowAll :: Int -> [(Int,Int)] -> Bool
coordsRowAll row = all (\ (a,b) -> a == row)

findCoordsRowAny :: KillerSudoku -> Int -> [[(Int,Int)]]
findCoordsRowAny ks row = filter (coordsRowAny row) (cellCoords ks)

findCoordsRowAll :: KillerSudoku -> Int -> [[(Int,Int)]]
findCoordsRowAll ks row = filter (coordsRowAll row) (cellCoords ks)

isInnieRow :: KillerSudoku -> Int -> Bool
isInnieRow ks row = length anyAllDiff == 1 && (length (filter inRow (concat anyAllDiff)) == 1)
    where anyAllDiff = findCoordsRowAny ks row \\ findCoordsRowAll ks row
          inRow (a,b) = a == row

isOutieRow :: KillerSudoku -> Int -> Bool
isOutieRow ks row = length anyAllDiff == 1 && (length (filter outRow (concat anyAllDiff)) == 1)
    where anyAllDiff = findCoordsRowAny ks row \\ findCoordsRowAll ks row
          outRow (a,b) = a /= row

innieCoordsRow :: KillerSudoku -> Int -> (Int,Int)
innieCoordsRow ks row = head $ filter (\ (a,b) -> a == row) (concat anyAllDiff)
    where anyAllDiff = findCoordsRowAny ks row \\ findCoordsRowAll ks row

outieCoordsRow :: KillerSudoku -> Int -> (Int,Int)
outieCoordsRow ks row = head $ filter (\ (a,b) -> a /= row) (concat anyAllDiff)
    where anyAllDiff = findCoordsRowAny ks row \\ findCoordsRowAll ks row

coordsToTotal :: [(Int,Int)] -> Cage -> Int
coordsToTotal coords cage
    | getCells cage == coords   = getTotal cage
    | otherwise                 = 0

getTotalCoords :: KillerSudoku -> [(Int,Int)] -> Int
getTotalCoords ks coords = head $ filter (/=0) $ map (coordsToTotal coords) (getCages ks)

ruleOf :: KillerSudoku -> Int
ruleOf ks = sum [1..nrows (getSudoku ks)]

innieValRow :: KillerSudoku -> Int -> Int
innieValRow ks row = ruleOf ks - sum (map (getTotalCoords ks) (findCoordsRowAll ks row))

outieValRow :: KillerSudoku -> Int -> Int
outieValRow ks row = sum (map (getTotalCoords ks) (findCoordsRowAny ks row)) - ruleOf ks

repInnieRow :: KillerSudoku -> Int -> KillerSudoku
repInnieRow ks row
    | isInnieRow ks row         = KillerSudoku (setElem val (innieCoordsRow ks row) s) c
    | otherwise                 = ks
    where s = getSudoku ks
          c = getCages ks
          val = [innieValRow ks row]

repOutieRow :: KillerSudoku -> Int -> KillerSudoku
repOutieRow ks row
    | isOutieRow ks row         = KillerSudoku (setElem val (outieCoordsRow ks row) s) c
    | otherwise                 = ks
    where s = getSudoku ks
          c = getCages ks
          val = [outieValRow ks row]

inOutRow :: KillerSudoku -> KillerSudoku
inOutRow ks = foldl repInnieRow (foldl repOutieRow ks lst) lst
    where lst = [1..nrows (getSudoku ks)]

-- column innie and outies
coordsColAll :: Int -> [(Int,Int)] -> Bool
coordsColAll col = all (\ (a,b) -> b == col)

coordsColAny :: Int -> [(Int,Int)] -> Bool
coordsColAny col = any (\ (a,b) -> b == col)

findCoordsColAny :: KillerSudoku -> Int -> [[(Int,Int)]]
findCoordsColAny ks col = filter (coordsColAny col) (cellCoords ks)

findCoordsColAll :: KillerSudoku -> Int -> [[(Int,Int)]]
findCoordsColAll ks col = filter (coordsColAll col) (cellCoords ks)

isInnieCol :: KillerSudoku -> Int -> Bool
isInnieCol ks col = length anyAllDiff == 1 && (length (filter inCol (concat anyAllDiff)) == 1)
    where anyAllDiff = findCoordsColAny ks col \\ findCoordsColAll ks col
          inCol (a,b) = b == col

isOutieCol :: KillerSudoku -> Int -> Bool
isOutieCol ks col = length anyAllDiff == 1 && (length (filter outCol (concat anyAllDiff)) == 1)
    where anyAllDiff = findCoordsColAny ks col \\ findCoordsColAll ks col
          outCol (a,b) = b /= col

innieCoordsCol :: KillerSudoku -> Int -> (Int,Int)
innieCoordsCol ks col = head $ filter (\ (a,b) -> b == col) (concat anyAllDiff)
    where anyAllDiff = findCoordsColAny ks col \\ findCoordsColAll ks col


outieCoordsCol :: KillerSudoku -> Int -> (Int,Int)
outieCoordsCol ks col = head $ filter (\ (a,b) -> b /= col) (concat anyAllDiff)
    where anyAllDiff = findCoordsColAny ks col \\ findCoordsColAll ks col

innieValCol :: KillerSudoku -> Int -> Int
innieValCol ks col = ruleOf ks - sum (map (getTotalCoords ks) (findCoordsColAll ks col))

outieValCol :: KillerSudoku -> Int -> Int
outieValCol ks col = sum (map (getTotalCoords ks) (findCoordsColAny ks col)) - ruleOf ks

repInnieCol :: KillerSudoku -> Int -> KillerSudoku
repInnieCol ks col
    | isInnieCol ks col         = KillerSudoku (setElem val (innieCoordsCol ks col) s) c
    | otherwise                 = ks
    where s = getSudoku ks
          c = getCages ks
          val = [innieValCol ks col]


repOutieCol :: KillerSudoku -> Int -> KillerSudoku
repOutieCol ks col
    | isOutieCol ks col         = KillerSudoku (setElem val (outieCoordsCol ks col) s) c
    | otherwise                 = ks
    where s = getSudoku ks
          c = getCages ks
          val = [outieValCol ks col]

inOutCol :: KillerSudoku -> KillerSudoku
inOutCol ks = foldl repInnieCol (foldl repOutieCol ks lst) lst
    where lst = [1..nrows (getSudoku ks)]

-- box innie and outies
coordsBoxAny :: Int -> Int -> [(Int,Int)] -> Bool
coordsBoxAny size box = any (\ (a,b) -> (a,b) `elem` (boxCoords size !! box))

coordsBoxAll :: Int -> Int -> [(Int,Int)] -> Bool
coordsBoxAll size box = all (\ (a,b) -> (a,b) `elem` (boxCoords size !! box))

findCoordsBoxAny :: KillerSudoku -> Int -> [[(Int,Int)]]
findCoordsBoxAny ks box = filter (coordsBoxAny (nrows $ getSudoku ks) box) (cellCoords ks)

findCoordsBoxAll :: KillerSudoku -> Int -> [[(Int,Int)]]
findCoordsBoxAll ks box = filter (coordsBoxAll (nrows $ getSudoku ks) box) (cellCoords ks)

isInnieBox :: KillerSudoku -> Int -> Bool
isInnieBox ks box = length anyAllDiff == 1 && (length (filter inBox (concat anyAllDiff)) == 1)
    where anyAllDiff = findCoordsBoxAny ks box \\ findCoordsBoxAll ks box
          inBox (a,b) = (a,b) `elem` (boxCoords (nrows $ getSudoku ks) !! box)

isOutieBox :: KillerSudoku -> Int -> Bool
isOutieBox ks box = length anyAllDiff == 1 && (length (filter outBox (concat anyAllDiff)) == 1)
    where anyAllDiff = findCoordsBoxAny ks box \\ findCoordsBoxAll ks box
          outBox (a,b) = (a,b) `notElem` (boxCoords (nrows $ getSudoku ks) !! box)

innieCoordsBox :: KillerSudoku -> Int -> (Int,Int)
innieCoordsBox ks box = head $ filter (\ (a,b) -> (a,b) `elem` (boxCoords size !! box)) cAnyAllDiff
    where cAnyAllDiff = concat $ findCoordsBoxAny ks box \\ findCoordsBoxAll ks box
          size = nrows $ getSudoku ks

    
outieCoordsBox :: KillerSudoku -> Int -> (Int,Int)
outieCoordsBox ks box = head $ filter (\ (a,b) -> (a,b) `notElem` (boxCoords size !! box)) cAnyAllDiff
    where cAnyAllDiff = concat $ findCoordsBoxAny ks box \\ findCoordsBoxAll ks box
          size = nrows $ getSudoku ks

innieValBox :: KillerSudoku -> Int -> Int
innieValBox ks box = ruleOf ks - sum (map (getTotalCoords ks) (findCoordsBoxAll ks box))

outieValBox :: KillerSudoku -> Int -> Int
outieValBox ks box = sum (map (getTotalCoords ks) (findCoordsBoxAny ks box)) - ruleOf ks

repInnieBox :: KillerSudoku -> Int -> KillerSudoku
repInnieBox ks box
    | isInnieBox ks box         = KillerSudoku (setElem val (innieCoordsBox ks box) s) c
    | otherwise                 = ks
    where s = getSudoku ks
          c = getCages ks
          val = [innieValBox ks box]

repOutieBox :: KillerSudoku -> Int -> KillerSudoku
repOutieBox ks box
    | isOutieBox ks box         = KillerSudoku (setElem val (outieCoordsBox ks box) s) c
    | otherwise                 = ks
    where s = getSudoku ks
          c = getCages ks
          val = [outieValBox ks box]

inOutBox :: KillerSudoku -> KillerSudoku
inOutBox ks = foldl repInnieBox (foldl repOutieBox ks lst) lst
    where lst = [0..nrows (getSudoku ks) - 1]

inOut :: KillerSudoku -> KillerSudoku
inOut = inOutBox . inOutRow . inOutCol