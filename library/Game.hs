{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
module Game where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import System.Random (uniform, uniformR, StdGen)
import System.Random.Stateful (Uniform(..), uniformRM)

fieldHeight, fieldWidth, fieldSize :: Int
fieldHeight = 20
fieldWidth = 10
fieldSize = fieldHeight*fieldWidth

tetHeight, tetWidth, tetSize :: Int
tetHeight = 4
tetWidth = 4
tetSize = tetHeight*tetWidth

data Tetramino = I | O | J | L | T | S | Z deriving (Show, Enum, Bounded, Eq)

instance Uniform Tetramino where
  -- uniformM :: StatefulGen g m => g -> m aSource
  uniformM g = toEnum <$> uniformRM (fromEnum (minBound :: Tetramino), fromEnum (maxBound :: Tetramino)) g

newtype FieldG a = Field { unField :: Vector a }
  deriving (Show, Functor, Applicative, Monad, Traversable, Foldable)
type Field = FieldG (Maybe Tetramino)
newtype TetraminoGridG a = TetraminoGrid { unTet :: Vector a }
  deriving (Show, Functor, Applicative, Monad, Traversable, Foldable)
type TetraminoGrid = TetraminoGridG (Maybe Tetramino)
data GridPos = GridPos { gpX :: Int, gpY :: Int } deriving (Show)
data GameState = GameState {
    gsGridPos :: !GridPos
  , gsField :: !Field
  , gsFallingTetra :: !TetraminoGrid
  , gsNextTetra :: !TetraminoGrid
  , gsScore :: !Word
  , gsStdGen :: !StdGen
  , gsFinished :: !Bool
  , gsPlayerName :: !String
  , gsScoreTable :: ![(String, Word)]
  , gsTotalDelay :: !Float
  } deriving Show

class Index2D c where
  (!) :: c a -> (Int, Int) -> a
  row :: c a -> Int -> Vector a
  width :: c a -> Int
  vec :: c a -> Vector a
  con :: Vector a -> c a
  mapWithIndex :: ((Int, Int) -> a -> b) -> c a -> c b
  --
  (!) g (i, j) = vec g V.! (i*width g + j)
  row g n = V.slice (n*width g) (width g) $ vec g
  mapWithIndex f g = con $ V.imap (\i -> f (quotRem i (width g))) (vec g)

instance Index2D FieldG where
  width _ = fieldWidth
  vec = unField
  con = Field

instance Index2D TetraminoGridG where
  width _ = tetWidth
  vec = unTet
  con = TetraminoGrid


stencil :: Tetramino -> TetraminoGrid
stencil t = TetraminoGrid . V.fromList $ go t
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

nextStencil :: StdGen -> (TetraminoGrid, StdGen)
nextStencil g =
  let (a, (b, g')) = uniformR (0,3) <$> uniform g
  in (foldr ($) (stencil a) (replicate b rotTet), g')

initState :: StdGen -> GameState
initState g = GameState {
    gsGridPos = GridPos 3 (-4)
  , gsField = Field $ V.replicate fieldSize Nothing
  , gsFallingTetra = cur
  , gsNextTetra = next
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
  | gsTotalDelay < min 1 (1000 / fromIntegral gsScore)
  = curState{gsTotalDelay=gsTotalDelay+delay}
gameStep _ gs = (realGameStep gs){gsTotalDelay=0}

realGameStep :: GameState -> GameState
realGameStep curState@GameState{..}
  | isJust firstLine
  = curState{
      gsField = Field . removeLine . unField $ gsField
    , gsScore = gsScore + 100
    }
  | canPlace (0, 1) curState gsFallingTetra
  = curState{gsGridPos = gsGridPos{gpY = gpY gsGridPos + 1 }}
  | not $ canPlace (0, 0) curState{gsGridPos=GridPos 3 0} gsNextTetra
  = curState{gsFinished = True}
  | otherwise
  = curState{
      gsGridPos = GridPos 3 (-4)
    , gsNextTetra = nextTetra
    , gsFallingTetra = gsNextTetra
    , gsField = Field $ unField gsField V.// placeTet
    , gsStdGen = nextStdGen
    }
  where
  (nextTetra, nextStdGen) = nextStencil gsStdGen
  placeTet = [(j*fieldWidth + i, gsFallingTetra ! (l, k))
    | k <- [0..3]
    , let i = gpX gsGridPos + k
    , l <- [0..3]
    , let j = max 0 (gpY gsGridPos) + l
    , isJust $ gsFallingTetra ! (l, k)
    ]
  firstLine = listToMaybe [ r*fieldWidth | r <- [0..fieldHeight-1]
                                         , all isJust $ row gsField r
                                         ]
  removeLine g
    | Just ix <- firstLine
    = V.concat [V.replicate fieldWidth Nothing, V.take ix g, V.drop (ix+fieldWidth) g]
    | otherwise = g

moveLeft, moveRight, rotate :: GameState -> GameState
moveLeft curState@GameState{gsGridPos=gp@GridPos{..}, ..}
  | canPlace (-1, 0) curState gsFallingTetra
  = curState{gsGridPos=gp{gpX=gpX-1}}
  | otherwise = curState
moveRight curState@GameState{gsGridPos=gp@GridPos{..}, ..}
  | canPlace (1, 0) curState gsFallingTetra
  = curState{gsGridPos=gp{gpX=gpX+1}}
  | otherwise = curState

rotate curState@GameState{gsFallingTetra=tet}
  | canPlace (0, 0) curState rot
  = curState{gsFallingTetra=rot}
  | otherwise = curState
  where rot = rotTet tet

rotTet :: TetraminoGrid -> TetraminoGrid
rotTet tet = TetraminoGrid $ V.generate tetSize $ \ix ->
       let (i,j) = quotRem ix tetWidth
       in unTet tet V.! ((tetHeight-1-j)*tetWidth + i)

canPlace :: (Int, Int) -> GameState -> TetraminoGrid -> Bool
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
slamDown gs@GameState{gsGridPos=GridPos{..}}
  | gpY == -4 = gs
  | otherwise = slamDown $ realGameStep gs
