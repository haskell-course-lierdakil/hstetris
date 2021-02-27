{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
module Game where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Data.Coerce
import System.Random (uniform, uniformR, StdGen)
import System.Random.Stateful (Uniform(..), uniformRM)

fieldHeight, fieldWidth, fieldSize, startRow :: Int
fieldHeight = 20
fieldWidth = 10
fieldSize = fieldHeight*fieldWidth
startRow = -2

tetHeight, tetWidth, tetSize :: Int
tetHeight = 4
tetWidth = 4
tetSize = tetHeight*tetWidth

data Tetromino = I | O | J | L | T | S | Z deriving (Show, Enum, Bounded, Eq)

instance Uniform Tetromino where
  -- uniformM :: StatefulGen g m => g -> m aSource
  uniformM g = toEnum <$> uniformRM (fromEnum (minBound :: Tetromino), fromEnum (maxBound :: Tetromino)) g

newtype FieldG a = Field { unField :: Vector a }
  deriving (Show, Functor, Applicative, Monad, Traversable, Foldable)
type Field = FieldG (Maybe Tetromino)
newtype TetrominoGridG a = TetrominoGrid { unTet :: Vector a }
  deriving (Show, Functor, Applicative, Monad, Traversable, Foldable)
type TetrominoGrid = TetrominoGridG (Maybe Tetromino)
data GridPos = GridPos { gpX :: Int, gpY :: Int } deriving (Show)
data GameState = GameState {
    gsGridPos :: !GridPos
  , gsField :: !Field
  , gsFallingTetro :: !TetrominoGrid
  , gsNextTetro :: !TetrominoGrid
  , gsScore :: !Word
  , gsStdGen :: !StdGen
  , gsFinished :: !Bool
  , gsPlayerName :: !String
  , gsScoreTable :: ![(String, Word)]
  , gsTotalDelay :: !Float
  } deriving Show

class Index2D c where
  (!) :: (Coercible (Vector a) (c a)) => c a -> (Int, Int) -> a
  row :: (Coercible (Vector a) (c a)) => c a -> Int -> Vector a
  width :: c a -> Int
  mapWithIndex :: (Coercible (Vector a) (c a), Coercible (Vector b) (c b))
               => ((Int, Int) -> a -> b) -> c a -> c b
  --
  (!) g (i, j) = coerce g V.! (i*width g + j)
  row g n = V.slice (n*width g) (width g) $ coerce g
  mapWithIndex f g = app (V.imap (\i -> f (quotRem i (width g)))) g

instance Index2D FieldG where
  width _ = fieldWidth

instance Index2D TetrominoGridG where
  width _ = tetWidth

stencil :: Tetromino -> TetrominoGrid
stencil t = TetrominoGrid . V.fromList $ go t
  where
  o = Nothing
  w = Just t
  go I = [
      o, o, w, o
    , o, o, w, o
    , o, o, w, o
    , o, o, w, o
    ]
  go O = [
      o, o, o, o
    , o, w, w, o
    , o, w, w, o
    , o, o, o, o
    ]
  go J = [
      o, o, o, o
    , o, o, w, o
    , o, o, w, o
    , o, w, w, o
    ]
  go L = [
      o, o, o, o
    , o, w, o, o
    , o, w, o, o
    , o, w, w, o
    ]
  go T = [
      o, o, o, o
    , o, o, w, o
    , o, w, w, o
    , o, o, w, o
    ]
  go S = [
      o, o, o, o
    , o, w, o, o
    , o, w, w, o
    , o, o, w, o
    ]
  go Z = [
      o, o, o, o
    , o, o, w, o
    , o, w, w, o
    , o, w, o, o
    ]

nextStencil :: StdGen -> (TetrominoGrid, StdGen)
nextStencil g =
  let (a, (b, g')) = uniformR (0,3) <$> uniform g
  in (foldr ($) (stencil a) (replicate b rotTet), g')

initState :: StdGen -> GameState
initState g = GameState {
    gsGridPos = GridPos 3 startRow
  , gsField = Field $ V.replicate fieldSize Nothing
  , gsFallingTetro = cur
  , gsNextTetro = next
  , gsScore = 0
  , gsStdGen = g'
  , gsFinished = False
  , gsPlayerName = ""
  , gsScoreTable = []
  , gsTotalDelay = 0
  }
  where
  (next, (cur, g')) = nextStencil <$> nextStencil g

gameStep :: Float -> GameState -> GameState
gameStep delay curState@GameState{..}
  | gsFinished
  = curState
  | gsTotalDelay >= 0.2, isJust firstLine
  = curState{
      gsField = app removeLine gsField
    , gsScore = gsScore + 100
    , gsTotalDelay = 0
    }
  | not $ canPlace (0, 0) curState{gsGridPos=GridPos 3 0} gsNextTetro
  = curState{gsFinished = True}
  | gsTotalDelay >= min 1 (1000 / fromIntegral gsScore)
  = (realGameStep curState){gsTotalDelay=0}
  | otherwise
  = curState{gsTotalDelay=gsTotalDelay+delay}
  where
  firstLine = listToMaybe [ r*fieldWidth | r <- [0..fieldHeight-1]
                                         , all isJust $ row gsField r
                                         ]
  removeLine :: Vector (Maybe Tetromino) -> Vector (Maybe Tetromino)
  removeLine g
    | Just ix <- firstLine
    = V.concat [V.replicate fieldWidth Nothing, V.take ix g, V.drop (ix+fieldWidth) g]
    | otherwise = g

realGameStep :: GameState -> GameState
realGameStep curState@GameState{..}
  | canPlace (0, 1) curState gsFallingTetro
  = curState{gsGridPos = gsGridPos{gpY = gpY gsGridPos + 1 }}
  | otherwise
  = curState{
      gsGridPos = GridPos 3 startRow
    , gsNextTetro = nextTetro
    , gsFallingTetro = gsNextTetro
    , gsField = app (V.// placeTet) gsField
    , gsStdGen = nextStdGen
    }
  where
  (nextTetro, nextStdGen) = nextStencil gsStdGen
  placeTet = [(j*fieldWidth + i, gsFallingTetro ! (l, k))
    | k <- [0..3]
    , let i = gpX gsGridPos + k
    , l <- [0..3]
    , let j = max 0 (gpY gsGridPos) + l
    , isJust $ gsFallingTetro ! (l, k)
    ]

moveLeft, moveRight, rotate :: GameState -> GameState
moveLeft curState@GameState{gsGridPos=gp@GridPos{..}, ..}
  | canPlace (-1, 0) curState gsFallingTetro
  = curState{gsGridPos=gp{gpX=gpX-1}}
  | otherwise = curState
moveRight curState@GameState{gsGridPos=gp@GridPos{..}, ..}
  | canPlace (1, 0) curState gsFallingTetro
  = curState{gsGridPos=gp{gpX=gpX+1}}
  | otherwise = curState

rotate curState@GameState{gsFallingTetro=tet}
  | canPlace (0, 0) curState rot
  = curState{gsFallingTetro=rot}
  | otherwise = curState
  where rot = rotTet tet

rotTet :: TetrominoGrid -> TetrominoGrid
rotTet tet = TetrominoGrid $ V.generate tetSize $ \ix ->
       let (i,j) = quotRem ix tetWidth
       in unTet tet V.! ((tetHeight-1-j)*tetWidth + i)

canPlace :: (Int, Int) -> GameState -> TetrominoGrid -> Bool
canPlace (dx, dy) GameState{..} tet = and
  [ j >= 0 && j < 20 && i >= 0 && i < 10 && isNothing (gsField ! (j, i))
  | k <- [0..3]
  , let i = gpX gsGridPos + k + dx
  , l <- [0..3]
  , let j = max 0 (gpY gsGridPos) + l + dy
  , isJust $ tet ! (l, k)
  ]

appendName :: Char -> GameState -> GameState
appendName c gs@GameState{..} = gs{gsPlayerName=c:gsPlayerName}

backspaceName :: GameState -> GameState
backspaceName gs@GameState{..} = gs{gsPlayerName=drop 1 gsPlayerName}

stopGame :: GameState -> GameState
stopGame gs = gs{gsFinished=True}

slamDown :: GameState -> GameState
slamDown gs@GameState{gsGridPos=gsGridPos@GridPos{..}, ..}
  | canPlace (0, 1) gs gsFallingTetro
  = slamDown gs{gsGridPos = gsGridPos{gpY = gpY + 1 }}
  | otherwise = gs{gsTotalDelay=0}

app :: (Coercible a b, Coercible c d) => (a -> c) -> b -> d
app f = coerce . f . coerce
