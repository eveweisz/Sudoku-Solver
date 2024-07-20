module KillerSudoku where

import Data.Matrix as M
import qualified Data.Vector as V
import Data.List as L
import SudokuTechniques
import Sudoku as S hiding (backtracker, replacements)
import KillerCages
import KillerPuzzles
import KillerTechniques

-- cage error
incompleteCell :: [[Int]] -> Bool
incompleteCell = any (\i -> length i > 1)

repeatError :: [[Int]] -> Bool
repeatError x = length (nub x) == length x || incompleteCell x

totalError :: [[Int]] -> Int -> Bool
totalError x total = incompleteCell x || (sum . concat) x == total

cageError1 :: KillerSudoku -> Int -> Bool
cageError1 ks n = totalError (cageGetElem ks n) total && repeatError (cageGetElem ks n)
    where total = getTotal (getCages ks!!n)

cageError :: KillerSudoku -> Bool
cageError ks = all (cageError1 ks) [0..length (getCages ks)-1]

-- combine the row, column and box error functions. Outputs true if no errors.
noksError :: KillerSudoku -> Bool
noksError ks = cageError ks && noError (getSudoku ks)

-- replace all Empty cells
options :: Cage -> Int -> [Int]
options cage size = (sort . nub . concat) (combinations (getTotal cage) (cLen cage) size)

compCells :: [[Int]] -> Int -> Int -> (Int,Int)
compCells mlist size i = (fI !! i `div` size + 1, fI !! i `mod` size + 1)
    where fI = findIndices ((==1) . length) mlist

compCells2 :: [[Int]] -> Int -> [(Int,Int)]
compCells2 mlist size = [compCells mlist size i | i <- lst]
    where lst = [0..length (findIndices ((==1) . length) mlist)-1]

coordList :: KillerSudoku -> Int -> [(Int,Int)]
coordList ks n = getCells (getCages ks !! n) \\ compCells2 (toList s) (nrows s)
    where s = getSudoku ks

repksEmpty1 :: Int -> Int -> KillerSudoku -> KillerSudoku
repksEmpty1 k n ks = KillerSudoku (setElem (options (cKS !! n) (nrows mat)) (coords !! k) mat) cKS
    where cKS = getCages ks
          mat = getSudoku ks
          coords = coordList ks n

repksEmpty2 :: Int -> KillerSudoku -> KillerSudoku
repksEmpty2 n ks = foldl (\x k -> repksEmpty1 k n x) ks [0..length (coordList ks n) - 1]

repksEmpty :: KillerSudoku -> KillerSudoku
repksEmpty ks = foldl (flip repksEmpty2) ks [0..length (getCages ks)-1]

-- remove invalid options in rows columns and boxes
removeInvalids3 :: KillerSudoku -> KillerSudoku
removeInvalids3 ks = KillerSudoku ((removeInvalids2 . getSudoku) ks) (getCages ks) 

-- killer sudoku version of sudoku techniques
ksSudokuTechniques :: KillerSudoku -> KillerSudoku
ksSudokuTechniques ks = KillerSudoku (sudokuTechniques (getSudoku ks)) (getCages ks)

killerTechniques :: KillerSudoku -> KillerSudoku
killerTechniques ks
    | ks == tech        = ks
    | otherwise         = killerTechniques tech
    where tech = setCombs ks

replacements :: KillerSudoku -> [KillerSudoku]
replacements ks = filter noksError [tech (KillerSudoku (setElem i (S.findLeastOption mat) mat) (getCages ks)) | i <- fLOgetElem mat]
    where mat = getSudoku ks
          tech = killerTechniques . ksSudokuTechniques . removeInvalids3

techniques :: KillerSudoku -> KillerSudoku
techniques ks
    | tech == ks      = ks
    | otherwise       = techniques tech
    where tech = (killerTechniques . ksSudokuTechniques . removeInvalids3) ks

ksSolver :: KillerSudoku -> KillerSudoku
ksSolver = backtracker . techniques . inOut . repksEmpty

backtracker :: KillerSudoku -> KillerSudoku
backtracker Fail = Fail
backtracker ks
    | completedSudoku (getSudoku ks) = ks
    | otherwise   = (first . replacements) ks
    where first [] = Fail
          first (ks : kss)
            | ks == Fail || backtracker ks == Fail    = first kss
            | otherwise                               = backtracker ks

