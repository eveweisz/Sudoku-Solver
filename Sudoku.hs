module Sudoku where
import SudokuTechniques
import SudokuPuzzles
import Data.Matrix as M
import qualified Data.Vector as V
import Data.List as L

-- checking for errors
noRowError :: Matrix [Int] -> Bool
noRowError mat = and [(all (<=1). counter2 . V.toList) (getRow i mat) | i <- [1..(nrows mat)]]

noColError :: Matrix [Int] -> Bool
noColError mat = and [(all (<=1) . counter2 . V.toList) (getCol i mat) | i <- [1..(nrows mat)]]

noBoxError :: Matrix [Int] -> Bool
noBoxError mat = and [(all (<=1) . counter2 . toList) (submatrix a b c d mat) | (a,b) <- zlist , (c,d) <- zlist]
    where zlist = zip (firlist (nrows mat)) (seclist (nrows mat))

noError :: Matrix [Int] -> Bool
noError mat = noBoxError mat && noColError mat && noRowError mat

-- replace element if position empty
emptyRep :: [Int] -> Int -> [Int]
emptyRep x i
    | x /= []   = x
    | otherwise = [1..i]

-- replace all Empty cells
repAllEmpty :: Matrix [Int] -> Matrix [Int]
repAllEmpty mat = mapPos (\_ x -> emptyRep x (nrows mat)) mat

-- check for empty cells
completedSudoku :: Matrix [Int] -> Bool
completedSudoku mat = all (==1) [length (V.toList (getRow i mat)!!j) | i <- [1..nrows mat] , j <- [0..nrows mat - 1]]

-- finding indices of smallest option cells
indexLen :: Matrix [Int] -> Int -> (Int,Int)
indexLen mat i
    | null fI    = (1,-1)
    | otherwise  = (head fI `div` nrows mat + 1, head fI `mod` nrows mat + 1)
    where fI = findIndices ((==i) . length) (toList mat)

findLeastOption :: Matrix [Int] -> (Int,Int)
findLeastOption mat = head (filter (/= (1,-1)) [indexLen mat i | i <- [2..(nrows mat)]])

fLOgetElem :: Matrix [Int] -> [[Int]]
fLOgetElem mat = map (: []) (mat ! findLeastOption mat)

sudokuTechniques :: Matrix [Int] -> Matrix [Int]
sudokuTechniques mat
    | tech == mat   = mat
    | otherwise     = sudokuTechniques tech
    where tech = (nakQuads . nakTriples . nakPairs . hidSingles . removeInvalids2) mat

replacements :: Matrix [Int] -> [Matrix [Int]]
replacements mat = filter noError [ tech $ setElem i (findLeastOption mat) mat | i <- fLOgetElem mat]
    where tech = sudokuTechniques . removeInvalids2

-- solver
solver :: Matrix [Int] -> Matrix [Int]
solver = backtracker . sudokuTechniques . removeInvalids2 . repAllEmpty

backtracker :: Matrix [Int] -> Matrix [Int]
backtracker sud
    | completedSudoku sud       = sud
    | otherwise                 = (first . replacements) sud
    where first [] = failSud
          first (sud : suds)
            | sud == failSud || backtracker sud == failSud    = first suds
            | otherwise                                       = backtracker sud