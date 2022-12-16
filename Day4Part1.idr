module Day4Part1

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
record EncyptedData where
  constructor MkEncyrptedData
  encryptedName : List Char
  sectorID : Maybe Int
  checkSum : List Char

toEncryptedData : (List Char,(Maybe Int,List Char))
                -> EncyptedData
toEncryptedData (a,(b,c)) = 
  MkEncyrptedData a b c 

isRealRoom : EncyptedData
           -> (Bool,EncyptedData)
isRealRoom xs = do
  let mcchars = take 5                    $
                nub                       $ 
                concat                    $
                reverse                   $ 
                sortBy (comparing length) $
                reverse                   $
                sortBy (comparing length) $
                map forget                $
                group                     $
                sort xs.encryptedName
  if xs.checkSum == mcchars
    then (True,xs)
    else (False,xs)

sumSectorIDs : List (Bool,EncyptedData)
             -> Int
sumSectorIDs Nil         = 0
sumSectorIDs ((a,b)::xs) =
  if a == True
    then case b.sectorID of
           Nothing       => sumSectorIDs xs
           Just sectorid => sectorid + (sumSectorIDs xs)
    else sumSectorIDs xs

main : IO ()
main = do
  --Read input.txt.
  inputfile <- readFile "input.txt" 
  --Process inputfile.
  case inputfile of
       Left  error    => putStrLn $ show error
       Right filedata => do let filedatas : List (List Char,(Maybe Int,List Char))
                                filedatas = map (\(a,(b,c)) => (filter (\x => x /= '-') a,
                                                                 (parseInteger $ fastPack b,
                                                                  filter (\x => x /= ']' && x /= '[') c
                                                                 )
                                                               ))                 $
                                            map (\(a,b) => (a,break (== '[') b))  $
                                            map (\(a,b) => (reverse b,reverse a)) $
                                            map (break (== '-'))                  $
                                            map reverse                           $
                                            map fastUnpack                        $
                                            map trim                              $
                                            lines                                 $
                                            trim filedata
                            let encrypteddata : List EncyptedData
                                encrypteddata = map toEncryptedData filedatas
                            let boolrealrooms : List (Bool,EncyptedData)
                                boolrealrooms = map isRealRoom encrypteddata
                            let numberofrealrooms : Int
                                numberofrealrooms = sumSectorIDs boolrealrooms
                            putStrLn ("AOC 2016 day 4, part 1 answer: " ++ show numberofrealrooms)
