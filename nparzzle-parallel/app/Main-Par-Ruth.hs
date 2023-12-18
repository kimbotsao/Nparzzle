module Main (main) where
import Data.Maybe (mapMaybe, fromMaybe, maybeToList)
import Data.Vector (Vector, (!), (//))
import Data.List.Split (splitOn)
import Data.Text (replace, pack, unpack)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Parallel.Strategies (parList, parMap, rseq, rdeepseq, using)

type Board = Vector Int
-- type Board = [[Int]]
data Direction = UP | DOWN | LEFT | RIGHT deriving Eq

data Puzzle = Puzzle 
    { board     :: Board
    , dist      :: Int
    , dim       :: Int
    , zero     :: Int
    , moves     :: Int
    , previous  :: Maybe Puzzle
    } deriving (Show, Eq, Ord)

initPuzzle :: [Int] -> Puzzle
initPuzzle xs = Puzzle b d n z 0 Nothing
    where
        b = V.fromList xs
        n = dimension b
        d = totalDist b n
        z = fromMaybe (error "Could not find zero tile") (V.elemIndex 0 b)

-- Convert matrix indices to array indices
matrix2array :: Int -> Int -> Int -> Int
matrix2array n row col = n * row + col

-- Convert array indices to matrix indices
array2matrix :: Int -> Int -> (Int, Int)
array2matrix n i = (i `div` n, i `mod` n)

-- Returns dimension of a board
dimension :: Board -> Int
dimension = round . sqrt . fromIntegral . V.length

-- Manhattan distance
manhattan :: Int -> Int -> Int -> Int  -> Int
manhattan v n i j = if v == 0 then 0 else rowDist + colDist
    where
        rowDist = abs (i - ((v-1) `div` n))
        colDist = abs (j - ((v-1) `mod` n))

-- Applies heuristic to all tiles
totalDist :: Board -> Int -> Int
totalDist b n = sum [manhattan (b ! matrix2array n i j) n i j | i <- [0..n-1], j <- [0..n-1]]

-- Swap tiles
swap :: Puzzle -> Int -> Int -> Puzzle
swap p i j = p { board = b
                 , dist = totalDist b n
                 , zero = k
                 , moves = moves p + 1
                 , previous = Just p }
    where
        k = matrix2array n i j
        b = prev // [(zero p, prev ! k), (k, 0)]
        prev = board p
        n = dim p

-- Move tile after checking valid move
move :: Puzzle -> Direction -> Maybe Puzzle
move p dir = case dir of
    UP -> if i <= 0   then Nothing else Just $ swap p (i-1) j
    DOWN -> if i >= n-1 then Nothing else Just $ swap p (i+1) j
    LEFT  -> if j <= 0   then Nothing else Just $ swap p i (j-1)
    RIGHT  -> if j >= n-1 then Nothing else Just $ swap p i (j+1)
    where
        (i, j) = array2matrix n (zero p)
        n = dim p

{-
-- Get all possible child node states (neighbors)
neighbors :: Puzzle -> [Puzzle]
neighbors p = mapMaybe (move p) [UP, DOWN, LEFT, RIGHT]
-}

-- Corrected function to handle Maybe Puzzle
fromMaybeMove :: Puzzle -> Direction -> [Puzzle]
fromMaybeMove p dir = maybeToList (move p dir)

-- Use a parallel map with rseq for evaluation
parallelNeighbors :: Puzzle -> [Puzzle]
parallelNeighbors p = concat $ parMap rseq (\dir -> fromMaybeMove p dir) [UP, DOWN, LEFT, RIGHT]

solve :: Puzzle -> Puzzle
solve p = go (PQ.fromList [(dist p, p)])
    where
        go fr = if dist puzzle == 0 
                then puzzle 
                else go fr2
            where
                ((_, puzzle), fr1) = PQ.deleteFindMin fr

                ns = case previous puzzle of
                    Nothing -> parallelNeighbors puzzle
                    Just n  -> filter (\x -> board x /= board n) (parallelNeighbors puzzle)

                ps  = zip [moves q + dist q | q <- ns] ns
                fr2 = foldr (uncurry PQ.insert) fr1 ps

boards :: Puzzle -> [[Int]]
boards p = map V.toList (reverse $ brds p)
    where
        brds q = case previous q of
            Nothing -> [board q]
            Just r  -> board q : brds r

toBoard :: String -> [Int]
toBoard input = toIntBoard (words <$> (drop 1 . clearInput . lines $ input))

clearInput :: [String] -> [String]
clearInput xs = filter (/="") $ map (head . splitOn "#") xs

toIntBoard :: [[String]] -> [Int]
toIntBoard = concatMap (map read)

checkArgs :: [String] -> IO [String]
checkArgs a = if null a then putStrLn "Usage: stack exec nparzzle-exe <file>" >> exitSuccess else pure a

main :: IO ()
main = do
    args <- checkArgs =<< getArgs
    txt <- readFile $ head args
    let gameList = splitOn "#" txt
        games = map toBoard gameList
    -- print gameList
    -- print games
    -- print $ length games
    let sols = map (solve . initPuzzle) games
    -- Print each solution using the boards function
    mapM_ (print . boards) sols
