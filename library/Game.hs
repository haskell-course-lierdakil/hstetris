{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Game where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Data.Maybe
import Data.List
import System.Random (uniform, StdGen, mkStdGen)
import System.Random.Stateful (Uniform(..), uniformRM)
import GHC.Exts (toList)

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

type Field = Vector (Maybe Tetramino)
type TetraminoStencil = Vector Bool
type TetraminoGrid = Vector (Maybe Tetramino)
data GridPos = GridPos { gpX :: Int, gpY :: Int } deriving (Show)
data GameState = GameState {
    gsGridPos :: !GridPos
  , gsField :: !Field
  , gsFallingTetra :: !TetraminoGrid
  , gsNextTetra :: !TetraminoGrid
  , gsScore :: !Word
  , gsLevel :: !Word8
  , gsStdGen :: !StdGen
  } deriving Show

stencil :: Tetramino -> TetraminoGrid
stencil t = fillStencil t . V.fromList $ go t
  where
  o = False
  w = True
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

  fillStencil tet = fmap $ \case
    True -> Just tet
    False -> Nothing

initState :: GameState
initState = GameState {
    gsGridPos = GridPos 3 0
  , gsField = V.replicate fieldSize Nothing
  , gsFallingTetra = stencil cur
  , gsNextTetra = stencil next
  , gsScore = 0
  , gsLevel = 0
  , gsStdGen = g''
  }
  where
  g = mkStdGen 42
  (cur, g') = uniform g
  (next, g'') = uniform g'

gameStep :: GameState -> GameState
gameStep curState@GameState{..}
  | isJust firstLine
  = curState{
      gsField = removeLine gsField
    , gsScore = gsScore + 100
    }
  | canPlace (0, 1) curState gsFallingTetra
  = curState{gsGridPos = gsGridPos{gpY = gpY gsGridPos + 1 }}
  | not $ canPlace (0, 0) curState{gsGridPos=GridPos 3 0} gsNextTetra
  = curState
  | otherwise
  = curState{
      gsGridPos = GridPos 3 0
    , gsNextTetra = nextTetra
    , gsFallingTetra = gsNextTetra
    , gsField = gsField V.// placeTet
    , gsStdGen = nextStdGen
    }
  where
  (tetra, nextStdGen) = uniform gsStdGen
  nextTetra = stencil tetra
  placeTet = [(j*fieldWidth + i, gsFallingTetra V.! tetIdx)
    | k <- [0..3]
    , let i = gpX gsGridPos + k
    , l <- [0..3]
    , let j = gpY gsGridPos + l
          tetIdx = tetWidth * l + k
    , isJust $ gsFallingTetra V.! tetIdx
    ]
  firstLine = listToMaybe [ ix | row <- [0..fieldHeight-1]
                               , let ix = row*fieldWidth
                               , all isJust $ V.slice ix fieldWidth gsField
                               ]
  removeLine g
    | Just ix <- firstLine
    = V.concat [V.replicate 10 Nothing, V.take ix g, V.drop (ix+fieldWidth) g]
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
  where
  rot = V.generate tetSize $ \ix ->
         let (i,j) = quotRem ix tetWidth
         in tet V.! ((tetHeight-1-j)*tetWidth + i)

canPlace :: (Int, Int) -> GameState -> TetraminoGrid -> Bool
canPlace (dx, dy) GameState{..} tet = and
  [ j >= 0 && j < 20 && i >= 0 && i < 10 && isNothing (gsField V.! (j*fieldWidth + i))
  | k <- [0..3]
  , let i = gpX gsGridPos + k + dx
  , l <- [0..3]
  , let j = gpY gsGridPos + l + dy
  , isJust $ tet V.! (tetWidth * l + k)
  ]

mapWithIndex width f = V.imap (\i -> f (quotRem i width))
mapFieldWithIndex = mapWithIndex fieldWidth
mapTetWithIndex = mapWithIndex tetWidth
