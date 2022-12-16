module Day4Part2

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

public export
data Alphabet = A
              | B
              | C
              | D
              | E
              | F
              | G
              | H
              | I
              | J
              | K
              | L
              | M
              | N
              | O
              | P
              | Q
              | R
              | S
              | T
              | U
              | V
              | W
              | X
              | Y
              | Z
              | Space

public export
Eq Alphabet where
  A     == A     = True
  B     == B     = True
  C     == C     = True
  D     == D     = True
  E     == E     = True
  F     == F     = True
  G     == G     = True
  H     == H     = True
  I     == I     = True
  J     == J     = True
  K     == K     = True
  L     == L     = True
  M     == M     = True
  N     == N     = True
  O     == O     = True
  P     == P     = True
  Q     == Q     = True
  R     == R     = True
  S     == S     = True
  T     == T     = True
  U     == U     = True
  V     == V     = True
  W     == W     = True
  X     == X     = True
  Y     == Y     = True
  Z     == Z     = True
  Space == Space = True
  _     == _     = False 

public export
Show Alphabet where
  show A     = "a"
  show B     = "b"
  show C     = "c"
  show D     = "d"
  show E     = "e"
  show F     = "f"
  show G     = "g"
  show H     = "h"
  show I     = "i"
  show J     = "j"
  show K     = "k"
  show L     = "l"
  show M     = "m"
  show N     = "n"
  show O     = "o"
  show P     = "p"
  show Q     = "q"
  show R     = "r"
  show S     = "s"
  show T     = "t"
  show U     = "u"
  show V     = "v"
  show W     = "w"
  show X     = "x"
  show Y     = "y"
  show Z     = "z"
  show Space = " "

toAlphabet : Char
           -> Alphabet
toAlphabet x =
  case x of
    'a' => A
    'b' => B
    'c' => C
    'd' => D
    'e' => E
    'f' => F
    'g' => G
    'h' => H
    'i' => I
    'j' => J
    'k' => K
    'l' => L
    'm' => M
    'n' => N
    'o' => O
    'p' => P
    'q' => Q
    'r' => R
    's' => S
    't' => T
    'u' => U
    'v' => V
    'w' => W
    'x' => X
    'y' => Y
    'z' => Z 
    _   => Space

fromAlphabet : Alphabet
             -> Char
fromAlphabet x =
  case x of
    A     => 'a' 
    B     => 'b'
    C     => 'c'
    D     => 'd'
    E     => 'e'
    F     => 'f'
    G     => 'g'
    H     => 'h'
    I     => 'i'
    J     => 'j'
    K     => 'k'
    L     => 'l'
    M     => 'm'
    N     => 'n'
    O     => 'o'
    P     => 'p'
    Q     => 'q'
    R     => 'r'
    S     => 's'
    T     => 't'
    U     => 'u'
    V     => 'v'
    W     => 'w'
    X     => 'x'
    Y     => 'y'
    Z     => 'z'
    _     => ' '

alphabetForwardShift : List Int
                     -> State Alphabet Alphabet
alphabetForwardShift Nil     = do
  finalchar <- get
  pure finalchar 
alphabetForwardShift (c::cs) = do
  currentchar <- get
  case currentchar of
    A     => do put B
                alphabetForwardShift cs
    B     => do put C
                alphabetForwardShift cs
    C     => do put D
                alphabetForwardShift cs
    D     => do put E
                alphabetForwardShift cs
    E     => do put F
                alphabetForwardShift cs
    F     => do put G
                alphabetForwardShift cs 
    G     => do put H
                alphabetForwardShift cs 
    H     => do put I
                alphabetForwardShift cs 
    I     => do put J
                alphabetForwardShift cs 
    J     => do put K
                alphabetForwardShift cs 
    K     => do put L
                alphabetForwardShift cs 
    L     => do put M
                alphabetForwardShift cs 
    M     => do put N
                alphabetForwardShift cs 
    N     => do put O
                alphabetForwardShift cs 
    O     => do put P
                alphabetForwardShift cs 
    P     => do put Q
                alphabetForwardShift cs 
    Q     => do put R
                alphabetForwardShift cs 
    R     => do put S
                alphabetForwardShift cs 
    S     => do put T
                alphabetForwardShift cs 
    T     => do put U
                alphabetForwardShift cs 
    U     => do put V
                alphabetForwardShift cs 
    V     => do put W
                alphabetForwardShift cs 
    W     => do put X
                alphabetForwardShift cs 
    X     => do put Y
                alphabetForwardShift cs 
    Y     => do put Z
                alphabetForwardShift cs 
    Z     => do put A
                alphabetForwardShift cs 
    Space => do put Space
                finalchar <- get
                pure finalchar

cycleAlphabet : Maybe Int
              -> List Char
              -> List Char
cycleAlphabet _        Nil     = Nil
cycleAlphabet Nothing  _       = Nil
cycleAlphabet (Just x) (y::ys) = 
  if y == ' '
    then (' ' :: (cycleAlphabet (Just x) ys))
    else do let sectoridl : List Int
                sectoridl = [1..x]
            let nextdecryptedchar : Alphabet
                nextdecryptedchar = execState (toAlphabet y) (alphabetForwardShift sectoridl)
            (fromAlphabet nextdecryptedchar :: (cycleAlphabet (Just x) ys))

main : IO ()
main = do
  --Read input.txt.
  inputfile <- readFile "input.txt" 
  --Process inputfile.
  case inputfile of
       Left  error    => putStrLn $ show error
       Right filedata => do let filedatas : List (List Char,(Maybe Int,List Char))
                                filedatas = map (\(a,(b,c)) => (a,
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
                            let decryptedroomnames : List (List Char)
                                decryptedroomnames = map (\x => cycleAlphabet x.sectorID
                                                                              x.encryptedName)
                                                     encrypteddata
                            let room : Maybe Nat
                                room = head' $ findIndices (\x => isInfixOf "north" x)
                                                           (map fastPack decryptedroomnames)
                            case room of
                              Nothing => putStrLn "Couldn't solve AOC 2016 day 4, part 2."
                              Just ri => case (inBounds ri encrypteddata) of
                                              No  _ => putStrLn "Couldn't solve AOC 2016 day 4, part 2."
                                              Yes _ => do let finalsectorid : Maybe Int
                                                              finalsectorid = .sectorID $
                                                                              index ri encrypteddata
                                                          case finalsectorid of
                                                               Nothing  => putStrLn "Couldn't solve AOC 2016 day 4, part 2."
                                                               Just fsi => putStrLn ("AOC 2016 day 4, part 2 answer: " ++ show fsi)
