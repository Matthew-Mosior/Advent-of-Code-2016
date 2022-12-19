module Day4Part2OpF

import Control.Monad.State.Interface
import Control.Monad.State.State
import Control.Monad.Identity
import Control.Monad.ST
import Data.Either
import Data.List as DL
import Data.List1
import Data.Maybe
import Data.String
import Prelude.Types
import System.File


public export
record EncryptedData where
  constructor MkEncryptedData
  encryptedName : List Char
  sectorID : Maybe Int
  checkSum : List Char

toEncryptedData : (List Char,(Maybe Int,List Char))
                -> EncryptedData
toEncryptedData (a,(b,c)) = 
  MkEncryptedData a b c

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
                     -> Alphabet
                     -> ST s Alphabet 
alphabetForwardShift Nil a = do
  finalalphabetchar <- newSTRef a
  rfinalalphabetchar <- readSTRef finalalphabetchar
  pure rfinalalphabetchar
alphabetForwardShift cs a  = do
  currentalphabetchar <- newSTRef a
  iAFS cs
       currentalphabetchar
  rcurrentalphabetchar <- readSTRef currentalphabetchar
  pure rcurrentalphabetchar
    where
      iAFS : List Int
           -> STRef s Alphabet 
           -> ST s ()
      iAFS Nil     _ = pure ()
      iAFS (c::cs) a = do
        cc <- readSTRef a
        case cc of
          A     => do writeSTRef a
                                 B
                      iAFS cs
                           a
          B     => do writeSTRef a
                                 C
                      iAFS cs
                           a
          C     => do writeSTRef a
                                 D
                      iAFS cs
                           a
          D     => do writeSTRef a
                                 E
                      iAFS cs
                           a
          E     => do writeSTRef a
                                 F
                      iAFS cs
                           a
          F     => do writeSTRef a
                                 G
                      iAFS cs
                           a
          G     => do writeSTRef a
                                 H
                      iAFS cs
                           a
          H     => do writeSTRef a
                                 I
                      iAFS cs
                           a
          I     => do writeSTRef a
                                 J
                      iAFS cs
                           a
          J     => do writeSTRef a
                                 K
                      iAFS cs
                           a 
          K     => do writeSTRef a
                                 L
                      iAFS cs
                           a
          L     => do writeSTRef a
                                 M
                      iAFS cs
                           a
          M     => do writeSTRef a
                                 N
                      iAFS cs
                           a
          N     => do writeSTRef a
                                 O
                      iAFS cs
                           a
          O     => do writeSTRef a
                                 P
                      iAFS cs
                           a
          P     => do writeSTRef a
                                 Q
                      iAFS cs
                           a 
          Q     => do writeSTRef a
                                 R
                      iAFS cs
                           a 
          R     => do writeSTRef a
                                 Day4Part2OpF.S
                      iAFS cs
                           a 
          S     => do writeSTRef a
                                 T
                      iAFS cs
                           a 
          T     => do writeSTRef a
                                 U
                      iAFS cs
                           a 
          U     => do writeSTRef a
                                 V
                      iAFS cs
                           a 
          V     => do writeSTRef a
                                 W
                      iAFS cs
                           a 
          W     => do writeSTRef a
                                 X
                      iAFS cs
                           a 
          X     => do writeSTRef a
                                 Y
                      iAFS cs
                           a 
          Y     => do writeSTRef a
                                 Day4Part2OpF.Z
                      iAFS cs
                           a 
          Z     => do writeSTRef a
                                 A
                      iAFS cs
                           a 
          Space => do writeSTRef a
                                 Space
                      pure () 

cycleAlphabet : Maybe Int
              -> List Char
              -> List Char
cycleAlphabet _        Nil     = Nil
cycleAlphabet Nothing  _       = Nil
cycleAlphabet (Just x) (y::ys) = do
  let sectoridl : List Int
      sectoridl = [1..x]
  let nextdecryptedchar : Alphabet
      nextdecryptedchar = runST $ alphabetForwardShift sectoridl
                                                       (toAlphabet y)
  (fromAlphabet nextdecryptedchar :: (cycleAlphabet (Just x) ys))

filterAndDecryptRoomName : List EncryptedData
                         -> EncryptedData
filterAndDecryptRoomName Nil     = MkEncryptedData Nil Nothing Nil
filterAndDecryptRoomName (x::xs) = do
  let currentdecryptedroomname : List Char
      currentdecryptedroomname = cycleAlphabet x.sectorID
                                               x.encryptedName
  if isInfixOf "north" (fastPack currentdecryptedroomname)
    then x
    else (filterAndDecryptRoomName xs)

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
                            let encrypteddata : List EncryptedData
                                encrypteddata = map toEncryptedData filedatas
                            let decryptedroomname : EncryptedData
                                decryptedroomname = filterAndDecryptRoomName encrypteddata
                            let finalsectorid : Maybe Int
                                finalsectorid = .sectorID decryptedroomname
                            case finalsectorid of
                                 Nothing  => putStrLn "Couldn't parse sector ID for AOC 2016 day 4, part 2."
                                 Just fsi => putStrLn ("AOC 2016 day 4, part 2 answer: " ++ show fsi)
