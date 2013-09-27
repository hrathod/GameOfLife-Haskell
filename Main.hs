
module Main where

import qualified Data.Map as M
import qualified Data.Maybe as N
import qualified System.Environment as E
import qualified System.IO as S
import qualified Control.Monad as C

type Point = (Integer, Integer)
data CellState = Alive | Dead
type Cells = M.Map Point CellState
data World = World { width :: Integer
                   , height :: Integer
                   , cells :: Cells
                   } deriving (Show)

instance Show CellState where
  show Alive = "*"
  show Dead = "-"

status :: Point -> Cells -> Maybe CellState
status = M.lookup

neighbors :: Point -> [Point]
neighbors p =
  [(fst p + x, snd p + y) | x <- [-1,0,1], y <- [-1,0,1], x /= 0 || y /= 0]

liveNeighbors :: Point -> Cells -> Integer
liveNeighbors p c = foldr ((+) . numValue) 0 $ neighbors p
  where numValue pt = case status pt c of
          Just Alive -> 1
          _ -> 0

updatePoint :: Maybe CellState -> Integer -> CellState
updatePoint Nothing _ = Dead
updatePoint (Just Alive) n
  | n < 2 || n > 3 = Dead
  | otherwise = Alive
updatePoint (Just Dead) n
  | n == 3 = Alive
  | otherwise = Dead

updateWorld :: World -> World
updateWorld world = World {width = width world, height = height world, cells = updateCells}
  where
    c = cells world
    updateCells = M.fromList [(k,updatePoint (status k c) (liveNeighbors k c)) | k <- M.keys c]

createWorld :: Integer -> Integer -> [Point] -> World
createWorld w h l = World { width = w, height = h, cells = createCells}
  where createCells = M.fromList [((x,y), state (x,y)) | x <- [1 .. w], y <- [1 .. h]]
        state (x,y) = if (x,y) `elem` l then Alive else Dead

convertRow :: World -> Integer -> String
convertRow world n = foldl (++) "" [ show $ state (x,n) | x <- [1 .. w] ]
  where c = cells world
        w = width world
        state p = N.fromJust $ status p c

printWorld :: World -> IO ()
printWorld world = mapM_ putStrLn [convertRow world x | x <- [ 1 .. h ] ]
  where h = height world

eventLoop :: World -> IO ()
eventLoop world = do
  printWorld world
  putStrLn ""
  putStr "Enter 'q' to quit, or Enter for next step: "
  S.hFlush S.stdout
  c <- getLine
  C.when (c /= "q") $ eventLoop $ updateWorld world

usage :: IO ()
usage = do putStrLn "Please provide width height init"
           putStrLn "For example, 6 6 \"[(2,2),(3,2),(2,3),(4,5),(5,4),(5,5)]\""

main :: IO ()
main = do
  args <- E.getArgs
  if length args == 3 then
    let w = read $ head args :: Integer
        h = read (args !! 1) :: Integer
        c = read (args !! 2) :: [(Integer,Integer)] in
    eventLoop $ createWorld w h c
  else
    usage

