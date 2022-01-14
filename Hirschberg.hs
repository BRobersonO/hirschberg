module Hirschberg where
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import Data.Array ( (!), listArray )
import qualified Data.Array as Array
import Data.List (elemIndex)
import Data.Maybe (fromJust)

hirsch :: [Char] -> [Char] -> IO [()]
hirsch x y = mapM putStrLn $ makeAligned $ hirschberg x y

hirschberg :: [Char] -> [Char] -> [(Char, Char)]
hirschberg string1 string2 =  align len1 len2
    where
        len1 = length string1
        len2 = length string2
        mid = len1 `div` 2
        leftx = take mid string1
        rightx = drop mid string1
        cut = fromJust (elemIndex (minimum lst) lst)
            where lst = zipWith (+) (cost leftx string2)  (reverse (cost rightx (reverse string2)))
        lefty = take cut string2
        righty = drop cut string2
        
        align 0 0 = []
        align 0 y = ('-', string2 !! (y - 1)) : align 0 (y - 1)
        align x 0 = (string1 !! (x - 1), '-') : align (x - 1) 0
        align 1 y = stdalign string1 string2
        align x 1 = stdalign string1 string2
        align x y = hirschberg leftx lefty ++ hirschberg rightx righty

cost :: [Char] -> [Char] -> [Int]
cost [] [] = [0]
cost string1 string2 = costFunc (0,0) matM --nx2 Matrix
    where
        strx = '-' : string1 -- Makes Strings one-indexed
        stry = '-' : string2
        m = length strx
        n = length stry
        matM = IntMap.empty
        
        costFunc (0, j) matM
            | j < n - 1 = costFunc (0, j + 1) (IntMap.insert j j matM)
            | j == n - 1 = costFunc (1, 0) (IntMap.insert j j matM)
        costFunc (i, 0) matM = costFunc (i, 1) (IntMap.insert n i matM)
        costFunc (i, j) matM
            | (i, j) == (m - 1, n - 1) = finalRow (IntMap.insert (j + n) (findMin matM (i,j)) matM) 0 n IntMap.empty -- LAST ELEM
            | j == n - 1 = costFunc (i + 1, 0) $ overwrite (IntMap.insert (j + n) (findMin matM (i,j)) matM) 0 n -- OVERWRITE
            | otherwise = costFunc (i, j + 1) (IntMap.insert (j + n) (findMin matM (i,j)) matM) -- NORMAL CASE
            where
                finalRow matM i n finalM
                    | i < n - 1 = finalRow matM (i + 1) n (IntMap.insert i (matM IntMap.! (i + n)) finalM)
                    | otherwise = map snd $ IntMap.toList $ IntMap.insert i (matM IntMap.! (i + n)) finalM
                overwrite matM i n
                    | i < n - 1 = overwrite (IntMap.insert i (matM IntMap.! (i + n)) matM) (i + 1) n
                    | otherwise = IntMap.insert i (matM IntMap.! (i + n)) matM
                findMin matM (i,j) =  minimum [ (matM IntMap.! (j - 1)) + charCompare (i,j),
                                                (matM IntMap.! (j + n - 1)) + 1,
                                                (matM IntMap.! j) + 1]
                charCompare (i,j)   | strx !! i == stry !! j = 0
                                    | otherwise              = 1

-- stdalign inspired by "Dynamic Programming in Haskell" by Christian Neukirchen pub: March 2006
stdalign :: [Char] -> [Char] -> [(Char, Char)]
stdalign string1 string2 = reverse $ calcElem len1 len2 
    where
        len1 = length string1
        len2 = length string2
        a = '-' : string1 -- makes strings one-indexed
        b = '-' : string2

        matrixM = listArray((0,0),(len1,len2)) [scoringFunct x y | x <- [0..len1],y <- [0..len2]]

        infix 5 *@*
        (*@*) i j = matrixM Array.! (i,j)

        scoringFunct 0 _ = 0
        scoringFunct _ 0 = 0
        scoringFunct x y = maximum [(x - 1 *@* y - 1) + charCompare x y,
                                     x - 1 *@* y,
                                     x     *@* y - 1]
            where charCompare x y | a !! x == b !! y = 1
                                  | otherwise        = 0

        calcElem 0 0 = []
        calcElem x y 
            | x == 0 = ('-' , b !! y) : calcElem 0 (y - 1)
            | y == 0 = (a !! x , '-' ) : calcElem (x - 1) 0
            | x *@* y == x *@* y - 1 = ('-' , b !! y) : calcElem x (y - 1)
            | x *@* y == x - 1 *@* y = (a !! x , '-' ) : calcElem (x - 1) y
            | otherwise = (a !! x , b !! y) : calcElem (x - 1) (y - 1) 
                                               
makeAligned :: [(a, a)] -> [[a]]
makeAligned lst = [map fst lst,map snd lst] -- transforms list of tuples into the alignment