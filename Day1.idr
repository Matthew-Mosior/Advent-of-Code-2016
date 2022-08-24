module Day1

import Control.Monad.State.Interface
import Control.Monad.State.State
import Control.Monad.Identity
import Data.Either
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.String.Extra
import System.File


public export
data GridDirection = North
                   | South
                   | East
                   | West

public export
Eq GridDirection where
  North == North = True
  South == South = True
  East  == East  = True
  West  == West  = True
  _     == _     = False

public export
Show GridDirection where
  show North = "North"
  show South = "South"
  show East  = "East"
  show West  = "West"

public export
getUpdatedGridCoor :  List (String,String) 
                   -> State (GridDirection,(Int,Int)) (Int,Int)
getUpdatedGridCoor Nil         = do
  (_,finaltuple) <- get
  pure finaltuple
getUpdatedGridCoor ((x,y)::xs) = do
  (dir,(xcoor,ycoor)) <- get
  case dir of
       North => if x == "L"
                  then do put (West,(xcoor - (cast y),ycoor))
                          getUpdatedGridCoor xs
                  else do put (East,(xcoor + (cast y),ycoor))
                          getUpdatedGridCoor xs
       South => if x == "L"
                  then do put (East,(xcoor + (cast y),ycoor))
                          getUpdatedGridCoor xs
                  else do put (West,(xcoor - (cast y),ycoor))
                          getUpdatedGridCoor xs
       East  => if x == "L"
                  then do put (North,(xcoor,ycoor + (cast y)))
                          getUpdatedGridCoor xs
                  else do put (South,(xcoor,ycoor - (cast y)))
                          getUpdatedGridCoor xs
       West  => if x == "L"
                  then do put (South,(xcoor,ycoor - (cast y)))
                          getUpdatedGridCoor xs
                  else do put (North,(xcoor,ycoor + (cast y)))
                          getUpdatedGridCoor xs

public export
calcDistanceFormula : (GridDirection,(Int,Int)) -> Int
calcDistanceFormula (_,(b,c)) = b + c

main : IO ()
main = do
  --Read input.txt.
  inputfile <- readFile "input.txt" 
  --Process inputfile.
  case inputfile of
       Left  error    => putStrLn $ show error
       Right filedata => do let filedatas      : List (String,String)
                                filedatas      = forget $
                                                 map (\x => (take 1 x,drop 1 x)) $
                                                 map trim                        $
                                                 split (== ',') filedata
                            let gridstartstate : (GridDirection,(Int,Int))
                                gridstartstate = (North,(0,0))
                            let gridfinalstate : (GridDirection,(Int,Int))
                                gridfinalstate = execState gridstartstate (getUpdatedGridCoor filedatas) 
                            let distancefromorigin : Int
                                distancefromorigin = calcDistanceFormula gridfinalstate
                            putStrLn ("AOC 2016 day 1, part 1 answer: " ++ show distancefromorigin)
