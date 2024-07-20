module KillerCages where

import Data.Matrix

data Cage = Cage Int [(Int,Int)]
        deriving Show

instance Eq Cage where
    Cage x1 y1 == Cage x2 y2 = x1 == x2 && y1 == y2
    x /= y = not (x == y)

data KillerSudoku = KillerSudoku (Matrix [Int]) [Cage] | Fail
        deriving Show

instance Eq KillerSudoku where
    (==) :: KillerSudoku -> KillerSudoku -> Bool
    KillerSudoku x1 y1 == KillerSudoku x2 y2 = x1 == x2 && y1 == y2
    Fail == Fail = True
    KillerSudoku _ _ == Fail = False
    Fail == KillerSudoku _ _ = False
    (/=) :: KillerSudoku -> KillerSudoku -> Bool
    x /= y = not (x == y)

-- extract information from killer sudoku
getSudoku :: KillerSudoku -> Matrix [Int]
getSudoku (KillerSudoku m c) = m

getCages :: KillerSudoku -> [Cage]
getCages (KillerSudoku m c) = c

-- extract information from cage type
getTotal :: Cage -> Int
getTotal (Cage t c) = t

getCells :: Cage -> [(Int,Int)]
getCells (Cage t c) = c

cLen :: Cage -> Int
cLen (Cage t c) = length c

-- get the elements from the nth cage, starting at 0
cageGetElem :: KillerSudoku -> Int -> [[Int]]
cageGetElem ks n = [getElem i j (getSudoku ks) | (i,j) <- getCells (getCages ks !! n)]
