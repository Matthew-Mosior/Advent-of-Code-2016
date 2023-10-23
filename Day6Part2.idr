module Day6Part2

import Control.Monad.State.Interface
import Control.Monad.State.State
import Control.Monad.Identity
import Data.Either
import Data.List as DL
import Data.List1
import Data.Maybe
import Data.String
import Prelude.Interfaces
import Prelude.Types
import System.File

main : IO ()
main = do
  --Read input.txt.
  inputfile <- readFile "input.txt" 
  --Process inputfile.
  case inputfile of
       Left  error    => putStrLn $ show error
       Right filedata => do let filedatas : List (List Char)
                                filedatas = transpose      $ 
                                            map fastUnpack $
                                            map trim       $
                                            lines          $
                                            trim filedata
                            let leastcommon : String
                                leastcommon = joinBy "" $
                                              map (\x => case (last' x) of
                                                           Nothing => ""
                                                           Just h  => h
                                                  ) $
                                              map (map (\x => case (head' x) of
                                                           Nothing => ""
                                                           Just h  => singleton h
                                                       )
                                                  ) $
                                              map (map (forget)) $
                                              map (\x => sortBy (flip $ comparing length) . DL.group . sort $ x
                                                  )
                                              filedatas
                            putStrLn ("AOC 2016 day 6, part 2 answer: " ++ leastcommon)
