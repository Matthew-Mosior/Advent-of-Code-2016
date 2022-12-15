module Day3Part2

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
record Triangle where
  constructor MkTriangle
  sideA : Int
  sideB : Int
  sideC : Int

toTriangle : List (Maybe Int)
           -> Maybe Triangle
toTriangle (Just _ :: (Just _ :: (Just _ :: (_ :: _)))) = Nothing
toTriangle (Just _ :: (Just _ :: (Nothing :: _)))       = Nothing
toTriangle [Just _, Just _]                             = Nothing
toTriangle (Just _ :: (Nothing :: _))                   = Nothing
toTriangle [Just _]                                     = Nothing
toTriangle (Nothing :: _)                               = Nothing
toTriangle Nil                                          = Nothing
toTriangle ((Just x)::(Just y)::(Just z)::[])           =
  Just $ MkTriangle (cast x) (cast y) (cast z)

isTriangle : Maybe Triangle
           -> Bool
isTriangle Nothing   = False
isTriangle (Just xs) = 
  if xs.sideA + xs.sideB > xs.sideC &&
     xs.sideA + xs.sideC > xs.sideB &&
     xs.sideB + xs.sideC > xs.sideA
    then True
    else False

countTriangles : List Bool
               -> Int
countTriangles Nil     = 0
countTriangles (x::xs) =
  if x == True
    then 1 + (countTriangles xs)
    else countTriangles xs

chunksOf : Int -> List (Maybe Int) -> List (List (Maybe Int))
chunksOf _ Nil = Nil 
chunksOf i ls  =
  [take (cast i) ls]
  ++
  (chunksOf i (drop (cast i) ls))

main : IO ()
main = do
  --Read input.txt.
  inputfile <- readFile "input.txt" 
  --Process inputfile.
  case inputfile of
       Left  error    => putStrLn $ show error
       Right filedata => do let filedatas : List (List (Maybe Int))
                                filedatas = chunksOf 3             $
                                            concat                 $
                                            transpose              $
                                            map (map parseInteger) $ 
                                            map words              $
                                            map trim               $
                                            lines                  $
                                            trim filedata
                            let triangledata : List (Maybe Triangle)
                                triangledata = map toTriangle filedatas
                            let alltrianglebools : List Bool
                                alltrianglebools = map isTriangle triangledata
                            let numoftriangles : Int
                                numoftriangles = countTriangles alltrianglebools
                            putStrLn ("AOC 2016 day 3, part 2 answer: " ++ show numoftriangles)
