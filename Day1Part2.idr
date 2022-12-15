module Day1Part2

import Control.Monad.State.Interface
import Control.Monad.State.State
import Control.Monad.Identity
import Data.Either
import Data.List as DL
import Data.List1
import Data.Maybe
import Data.String
import Prelude.Types
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
generateDiscreteGridCoors : GridDirection
                          -> (String,String)
                          -> (Int,Int)
                          -> List (Int,Int)
generateDiscreteGridCoors dir (x,y) (a,b) =
  case dir of
    North => if x == "L"
               then zip [(a-1)..(a-(cast y))]
                        (DL.replicate (Prelude.Types.List.length [(a-1)..(a-(cast y))])
                                      b
                        )
               else zip [(a+1)..(a+(cast y))]
                        (DL.replicate (Prelude.Types.List.length [(a+1)..(a+(cast y))])
                                      b
                        )
    South => if x == "L"
               then zip [(a+1)..(a+(cast y))]
                        (DL.replicate (Prelude.Types.List.length [(a+1)..(a+(cast y))])
                                      b
                        )
               else zip [(a-1)..(a-(cast y))]
                        (DL.replicate (Prelude.Types.List.length [(a-1)..(a-(cast y))])
                                      b
                        )
    East  => if x == "L"
               then zip (DL.replicate (Prelude.Types.List.length [(b+1)..(b+(cast y))])
                                      a
                        )
                        [(b+1)..(b+(cast y))]
               else zip (DL.replicate (Prelude.Types.List.length [(b-1)..(b-(cast y))])
                                      a
                        )
                        [(b-1)..(b-(cast y))]
    West  => if x == "L"
               then zip (DL.replicate (Prelude.Types.List.length [(b-1)..(b-(cast y))])
                                      a
                        )
                        [(b-1)..(b-(cast y))]
               else zip (DL.replicate (Prelude.Types.List.length [(b+1)..(b+(cast y))])
                                      a
                        )
                        [(b+1)..(b+(cast y))]

public export
getUpdatedGridCoor :  List (String,String) 
                   -> State (GridDirection,(Int,Int),List (Int,Int)) (Int,Int)
getUpdatedGridCoor Nil         = do
  (_,finaltuple,_) <- get
  pure finaltuple
getUpdatedGridCoor ((x,y)::xs) = do
  (dir,(xcoor,ycoor),prevcoors) <- get
  let updatedgridcoors = generateDiscreteGridCoors dir
                                                   (x,y)
                                                   (xcoor,ycoor)
  case dir of
       North => if x == "L" &&
                   not (isNil $ DL.intersect updatedgridcoors prevcoors)
                  then do let newhead = head' $ DL.intersect updatedgridcoors prevcoors
                          case newhead of
                            Nothing => do (_,finaltuple,_) <- get
                                          pure finaltuple
                            Just h  => do put (West,h,prevcoors ++ updatedgridcoors)
                                          (_,finaltuple,_) <- get
                                          pure finaltuple
                  else if x == "L" &&
                          not (elem (xcoor - (cast y),ycoor) prevcoors)
                    then do put (West,(xcoor - (cast y),ycoor),prevcoors ++ updatedgridcoors)
                            getUpdatedGridCoor xs
                    else if x == "R" &&
                            not (isNil $ DL.intersect updatedgridcoors prevcoors)
                            then do let newhead = head' $ DL.intersect updatedgridcoors prevcoors
                                    case newhead of
                                      Nothing => do (_,finaltuple,_) <- get
                                                    pure finaltuple
                                      Just h  => do put (East,h,prevcoors ++ updatedgridcoors)
                                                    (_,finaltuple,_) <- get
                                                    pure finaltuple
                            else do put (East,(xcoor + (cast y),ycoor),prevcoors ++ updatedgridcoors)
                                    getUpdatedGridCoor xs
       South => if x == "L" &&
                   not (isNil $ DL.intersect updatedgridcoors prevcoors)
                  then do let newhead = head' $ DL.intersect updatedgridcoors prevcoors
                          case newhead of
                            Nothing => do (_,finaltuple,_) <- get
                                          pure finaltuple
                            Just h  => do put (East,h,prevcoors ++ updatedgridcoors)
                                          (_,finaltuple,_) <- get
                                          pure finaltuple
                  else if x == "L" &&
                          not (elem (xcoor + (cast y),ycoor) prevcoors)
                    then do put (East,(xcoor + (cast y),ycoor),prevcoors ++ updatedgridcoors)
                            getUpdatedGridCoor xs
                    else if x == "R" &&
                            not (isNil $ DL.intersect updatedgridcoors prevcoors)
                      then do let newhead = head' $ DL.intersect updatedgridcoors prevcoors
                              case newhead of
                                Nothing => do (_,finaltuple,_) <- get
                                              pure finaltuple
                                Just h  => do put (West,h,prevcoors ++ updatedgridcoors)
                                              (_,finaltuple,_) <- get
                                              pure finaltuple
                      else do put (West,(xcoor - (cast y),ycoor),prevcoors ++ updatedgridcoors)
                              getUpdatedGridCoor xs
       East  => if x == "L" &&
                   not (isNil $ DL.intersect updatedgridcoors prevcoors)
                  then do let newhead = head' $ DL.intersect updatedgridcoors prevcoors
                          case newhead of
                            Nothing => do (_,finaltuple,_) <- get
                                          pure finaltuple
                            Just h  => do put (North,h,prevcoors ++ updatedgridcoors)
                                          (_,finaltuple,_) <- get
                                          pure finaltuple
                  else if x == "L" &&
                          not (elem (xcoor,ycoor + (cast y)) prevcoors)
                    then do put (North,(xcoor,ycoor + (cast y)),prevcoors ++ updatedgridcoors)
                            getUpdatedGridCoor xs
                    else if x == "R" &&
                            not (isNil $ DL.intersect updatedgridcoors prevcoors)
                      then do let newhead = head' $ DL.intersect updatedgridcoors prevcoors
                              case newhead of
                                Nothing => do (_,finaltuple,_) <- get
                                              pure finaltuple
                                Just h  => do put (South,h,prevcoors ++ updatedgridcoors)
                                              (_,finaltuple,_) <- get
                                              pure finaltuple
                      else do put (South,(xcoor,ycoor - (cast y)),prevcoors ++ updatedgridcoors)
                              getUpdatedGridCoor xs
       West  => if x == "L" &&
                   not (isNil $ DL.intersect updatedgridcoors prevcoors)
                  then do let newhead = head' $ DL.intersect updatedgridcoors prevcoors
                          case newhead of
                            Nothing => do (_,finaltuple,_) <- get
                                          pure finaltuple
                            Just h  => do put (South,h,prevcoors ++ updatedgridcoors)
                                          (_,finaltuple,_) <- get
                                          pure finaltuple
                  else if x == "L" &&
                          not (elem (xcoor,ycoor - (cast y)) prevcoors)
                    then do put (South,(xcoor,ycoor - (cast y)),prevcoors ++ updatedgridcoors)
                            getUpdatedGridCoor xs
                    else if x == "R" &&
                            not (isNil $ DL.intersect updatedgridcoors prevcoors)
                      then do let newhead = head' $ DL.intersect updatedgridcoors prevcoors
                              case newhead of
                                Nothing => do (_,finaltuple,_) <- get
                                              pure finaltuple
                                Just h  => do put (North,h,prevcoors ++ updatedgridcoors)
                                              (_,finaltuple,_) <- get
                                              pure finaltuple
                      else do put (North,(xcoor,ycoor + (cast y)),prevcoors ++ updatedgridcoors)
                              getUpdatedGridCoor xs

public export
calcDistanceFormula : (GridDirection,(Int,Int),List (Int,Int)) -> Int
calcDistanceFormula (_,(b,c),_) = b + c

main : IO ()
main = do
  --Read input.txt.
  inputfile <- readFile "input.txt" 
  --Process inputfile.
  case inputfile of
       Left  error    => putStrLn $ show error
       Right filedata => do let filedatas      : List (String,String)
                                filedatas      = map (\(a,b) => (fastPack a,fastPack b)) $
                                                 forget                                  $
                                                 map (\x => (DL.take 1 x,DL.drop 1 x))   $
                                                 map fastUnpack                          $
                                                 map trim                                $
                                                 split (== ',') filedata
                            let gridstates : (GridDirection,(Int,Int),List (Int,Int))
                                gridstates = (North,(0,0),singleton (0,0))
                            let gridfinalstate : (GridDirection,(Int,Int),List (Int,Int))
                                gridfinalstate = execState gridstates (getUpdatedGridCoor filedatas)
                            let distancefromorigin : Int
                                distancefromorigin = calcDistanceFormula gridfinalstate
                            putStrLn ("AOC 2016 day 1, part 2 answer: " ++ show distancefromorigin)

