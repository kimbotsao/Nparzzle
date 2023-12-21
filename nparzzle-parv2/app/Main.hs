module Main (main) where
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Vector (Vector, (!), (//))
import Data.List.Split (splitOn)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Monad ( forM, void )
import Control.Parallel.Strategies
import Control.Parallel (par, pseq)
import Control.DeepSeq (NFData, rnf)
import GHC.Conc (pseq)
import Control.Concurrent ( newEmptyMVar, newMVar, forkIO, tryPutMVar, takeMVar, putMVar, readMVar, killThread)
import GHC.IO (unsafePerformIO)

type Board = Vector Int
data Direction = UP | DOWN | LEFT | RIGHT deriving Eq
instance NFData Direction where
    rnf dir = dir `seq` ()

data Puzzle = Puzzle 
    { board     :: Board
    , dist      :: Int
    , dim       :: Int
    , zero     :: Int
    , moves     :: Int
    , previous  :: Maybe Puzzle
    } deriving (Show, Eq, Ord)

instance NFData Puzzle where
    rnf puzzle = puzzle `seq` ()

initPuzzle :: [Int] -> Puzzle
initPuzzle xs = Puzzle b d dm z 0 Nothing
    where
        b = V.fromList xs
        d = totalDist b dm
        dm = dimension b
        z = fromMaybe (error "Couldn't find zero tile") (V.elemIndex 0 b)

dimension :: Board -> Int
dimension = round . sqrt . fromIntegral . V.length

matrix2array :: Int -> Int -> Int -> Int
matrix2array n row col = n * row + col

array2matrix :: Int -> Int -> (Int, Int)
array2matrix n i = (i `div` n, i `mod` n)

manhattan :: Int -> Int -> Int -> Int  -> Int
manhattan v n i j = 
    if v == 0 
        then 0 
        else rowDist + colDist
    where
        rowDist = abs (i - ((v-1) `div` n))
        colDist = abs (j - ((v-1) `mod` n))

totalDist :: Board -> Int -> Int
totalDist b n = sum [manhattan (b ! matrix2array n i j) n i j | i <- [0..n-1], j <- [0..n-1]]

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

move :: Puzzle -> Direction -> Maybe Puzzle
move p dir = case dir of
    UP -> if i > 0 
        then Just $ swap p (i-1) j else Nothing
    DOWN -> if i < n-1 
        then Just $ swap p (i+1) j else Nothing
    LEFT -> if j > 0
        then Just $ swap p i (j-1) else Nothing
    RIGHT -> if j < n-1 
        then Just $ swap p i (j+1) else Nothing
    where
        (i, j) = array2matrix n (zero p)
        n = dim p

parMapMaybe :: NFData b => (a -> Maybe b) -> [a] -> [b]
parMapMaybe f xs = runEval $ parList rdeepseq (mapMaybe f xs)

neighbors :: Puzzle -> [Puzzle]
neighbors p = mapMaybe (move p) [UP, DOWN, LEFT, RIGHT]
-- neighbors p = parMapMaybe (move p) [UP, DOWN, LEFT, RIGHT]

oldSolve :: Puzzle -> Puzzle
oldSolve p = go (PQ.fromList [(dist p, p)])
    where
        go frontier = if dist puzzle == 0 
                then puzzle
                else go frontier2
            where
                ((_, puzzle), frontier1) = PQ.deleteFindMin frontier

                ns = case previous puzzle of
                    Nothing -> neighbors puzzle
                    Just n  -> filter (\x -> board x /= board n) (neighbors puzzle)

                ps  = zip [moves q + dist q | q <- ns] ns
                frontier2 = foldr (uncurry PQ.insert) frontier1 ps

solve :: Puzzle -> IO Puzzle
solve p = do 
    let k   = 5
        psq = PQ.fromList [(dist p, p)]
    
    if PQ.size psq < k then do
        goSolve psq
    else do 
        -- let length = PQ.size psq 
        complete <- newEmptyMVar
        -- inprog <- newMVar length
        threads <- forM [uncurry PQ.singleton x| x <- PQ.toList psq] $ \ipsq -> forkIO $ do
            -- unsafePerformIO(goSolve ipsq)
            pSolved <- goSolve ipsq
            void (tryPutMVar complete pSolved)
        ret <- readMVar complete
        mapM_ killThread threads
        return ret

goSolve :: PQ.MinPQueue Int Puzzle -> IO Puzzle
goSolve frontier = if dist puzzle == 0 
                   then return puzzle
                   else goSolve frontier2
  where
    ((_, puzzle), frontier1) = PQ.deleteFindMin frontier

    ns = case previous puzzle of
      Nothing -> neighbors puzzle
      Just n  -> filter (\x -> board x /= board n) (neighbors puzzle)

    ps  = zip [moves q + dist q | q <- ns] ns
    frontier2 = foldr (uncurry PQ.insert) frontier1 ps


boards :: Puzzle -> [[Int]]
boards p = map V.toList (reverse $ brds p)
    where
        brds q = case previous q of
            Nothing -> [board q]
            Just r  -> board q : brds r

steps :: IO Puzzle -> IO Int
steps p = do
    pUnwrapped <- p
    let ret = length (boards pUnwrapped) - 1
    return ret

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
    -- let sols = map (oldSolve . initPuzzle) games
    let sols = map (solve . initPuzzle) games
    -- mapM_ (print . boards) sols
    -- mapM_ (print . steps) sols
    mapM_ (\sol -> do
             numSteps <- steps sol
             print numSteps
          ) sols