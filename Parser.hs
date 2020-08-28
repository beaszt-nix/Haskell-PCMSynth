import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Builder       as B

import           Data.Binary.Put                ( runPut )
import           Data.Maybe
import           Data.Char
import           Data.Either
import           Text.Parsec.String
import           Text.Parsec.Char
import           Text.Parsec
import           PCM
import           Wave

data RootNote = A | B | C | D | E | F | G | R deriving Show

data Note = Note Int RootNote Bool deriving Show

data Rhythm = Whole
            | Half
            | Quarter
            | Eight
            | Sixteen
            | ThirtyTwo deriving Show

noteExp :: Note -> Int
noteExp (Note reg r isSharp) =
  let rootval = case r of
        C -> 0
        D -> 2
        E -> 4
        F -> 5
        G -> 7
        A -> 9
        B -> 11
        R -> -1000
      isSh = if isSharp then 1 else 0
  in  3 + rootval + isSh + (reg - 5) * 12

mapNote :: (Floating a) => (Rhythm, Note) -> (a, Int, Bool)
mapNote (rh, n) =
  let len' = len rh
      no   = noteExp n
  in  (len', no, no <= -900)
 where
  len rh = case rh of
    Whole     -> 4.0
    Half      -> 2.0
    Quarter   -> 1.0
    Eight     -> 0.5
    Sixteen   -> 0.25
    ThirtyTwo -> 0.125

parseNote :: Parser (Rhythm, Note)
parseNote = do

  note' <- oneOf "ABCDEFGR"
  sharp <- option False $ char '#' >> return True
  reg   <- many digit >>= return . regProc
  rh    <- oneOf "WHQEST"

  return
    $ ( fromMaybe Quarter . charToRh $ rh
      , Note reg ((fromJust . charToRoot) note') sharp
      )
 where
  regProc :: [Char] -> Int
  regProc [] = 0
  regProc xs = read xs

  charToRoot :: Char -> Maybe RootNote
  charToRoot c = return $ case c of
    'A' -> A
    'B' -> B
    'C' -> C
    'D' -> D
    'E' -> E
    'F' -> F
    'G' -> G
    'R' -> R
  charToRh :: Char -> Maybe Rhythm
  charToRh c = return $ case c of
    'W' -> Whole
    'H' -> Half
    'Q' -> Quarter
    'E' -> Eight
    'S' -> Sixteen
    'T' -> ThirtyTwo

parseMujik :: Parser [(Rhythm, Note)]
parseMujik = parseNote `sepBy` spaces

main = do
  let ps = defaultPlayerState { volume = 0.1 }
      wv = Wave { wSampleRate   = round $ sampleRate ps
                , bitsPerSample = 32
                , channels      = 1
                , dataChunk     = B.empty
                }
  res <-
    readFile "sampleTex.mujik"
    >>= return
    .   B.toLazyByteString
    .   foldMap (note ps . mapNote)
    .   concatMap p'
    .   lines

  let wav = mkWave wv { dataChunk = BL.toStrict res }
  BL.writeFile "output.wav" $ runPut wav
  where p' = fromRight [] . runParser parseMujik () "sampleTex.mujik"
