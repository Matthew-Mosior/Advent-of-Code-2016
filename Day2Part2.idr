module Day2Part2

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
data Key = One
         | Two
         | Three
         | Four
         | Five
         | Six
         | Seven
         | Eight
         | Nine
         | KA
         | KB
         | KC
         | KD

public export
Eq Key where
  One   == One   = True
  Two   == Two   = True
  Three == Three = True
  Four  == Four  = True
  Five  == Five  = True
  Six   == Six   = True
  Seven == Seven = True
  Eight == Eight = True
  Nine  == Nine  = True
  KA    == KA    = True
  KB    == KB    = True
  KC    == KC    = True
  KD    == KD    = True
  _     == _     = False

public export
Show Key where
  show One   = "One"
  show Two   = "Two"
  show Three = "East"
  show Four  = "West"
  show Five  = "Five"
  show Six   = "Six"
  show Seven = "Seven"
  show Eight = "Eight"
  show Nine  = "Nine"
  show KA    = "A"
  show KB    = "B"
  show KC    = "C"
  show KD    = "D"

data KeyPadInput = U
                 | D
                 | L
                 | R

public export
Eq KeyPadInput where
  U == U = True
  D == D = True
  L == L = True
  R == R = True
  _ == _ = True

public export
Show KeyPadInput where
  show U = "UP"
  show D = "DOWN"
  show L = "LEFT"
  show R = "RIGHT"

public export
toKeyPadInput : List (List Char)
              -> List (List KeyPadInput)
toKeyPadInput Nil     = Nil 
toKeyPadInput (x::xs) =
  (map (\y => if y == 'U'
                then U
                else if y == 'D'
                  then D
                  else if y == 'L'
                    then L
                    else R) x)
  ::
  (toKeyPadInput xs)

public export
keyPadMovement : List KeyPadInput
               -> State Key Key
keyPadMovement Nil     = do
  finalkey <- get
  pure finalkey
keyPadMovement (x::xs) = do
  ckey <- get
  case x of
    U => case ckey of
           One   => keyPadMovement xs
           Two   => keyPadMovement xs
           Three => do put One
                       keyPadMovement xs
           Four  => keyPadMovement xs 
           Five  => keyPadMovement xs
           Six   => do put Two
                       keyPadMovement xs
           Seven => do put Three
                       keyPadMovement xs
           Eight => do put Four
                       keyPadMovement xs
           Nine  => do keyPadMovement xs
           KA    => do put Six
                       keyPadMovement xs
           KB    => do put Seven
                       keyPadMovement xs
           KC    => do put Eight
                       keyPadMovement xs
           KD    => do put KB
                       keyPadMovement xs
    D => case ckey of
           One   => do put Three
                       keyPadMovement xs
           Two   => do put Six
                       keyPadMovement xs
           Three => do put Seven
                       keyPadMovement xs
           Four  => do put Eight
                       keyPadMovement xs
           Five  => keyPadMovement xs
           Six   => do put KA
                       keyPadMovement xs
           Seven => do put KB
                       keyPadMovement xs
           Eight => do put KC
                       keyPadMovement xs
           Nine  => keyPadMovement xs
           KA    => keyPadMovement xs
           KB    => do put KD
                       keyPadMovement xs
           KC    => keyPadMovement xs
           KD    => keyPadMovement xs
    L => case ckey of
           One   => keyPadMovement xs
           Two   => keyPadMovement xs
           Three => do put Two
                       keyPadMovement xs
           Four  => do put Three
                       keyPadMovement xs
           Five  => keyPadMovement xs
           Six   => do put Five
                       keyPadMovement xs
           Seven => do put Six
                       keyPadMovement xs
           Eight => do put Seven 
                       keyPadMovement xs
           Nine  => do put Eight
                       keyPadMovement xs
           KA    => keyPadMovement xs
           KB    => do put KA
                       keyPadMovement xs
           KC    => do put KB
                       keyPadMovement xs
           KD    => keyPadMovement xs
    R => case ckey of
           One   => keyPadMovement xs
           Two   => do put Three
                       keyPadMovement xs
           Three => do put Four
                       keyPadMovement xs
           Four  => keyPadMovement xs
           Five  => do put Six
                       keyPadMovement xs
           Six   => do put Seven
                       keyPadMovement xs
           Seven => do put Eight
                       keyPadMovement xs
           Eight => do put Nine
                       keyPadMovement xs
           Nine  => keyPadMovement xs
           KA    => do put KB
                       keyPadMovement xs
           KB    => do put KC
                       keyPadMovement xs
           KC    => keyPadMovement xs
           KD    => keyPadMovement xs

keyPadMovementAll : List (List KeyPadInput)
                  -> Key
                  -> List Key
keyPadMovementAll Nil     _   = Nil
keyPadMovementAll (x::xs) ckey = do
  let fkey : Key
      fkey = execState ckey (keyPadMovement x)
  (fkey :: (keyPadMovementAll xs
                              fkey))

main : IO ()
main = do
  --Read input.txt.
  inputfile <- readFile "input.txt" 
  --Process inputfile.
  case inputfile of
       Left  error    => putStrLn $ show error
       Right filedata => do let filedatas : List (List Char) 
                                filedatas = lines' $
                                            fastUnpack filedata
                            let keypadinstructions : List (List KeyPadInput)
                                keypadinstructions = toKeyPadInput filedatas
                            let finalkeysequence : List Key
                                finalkeysequence = keyPadMovementAll keypadinstructions
                                                                     Five
                            putStrLn ("AOC 2016 day 2, part 2 answer: " ++ (unwords $ map show finalkeysequence))
