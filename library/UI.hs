{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NegativeLiterals #-}
module UI where

import Graphics.Gloss
import Game
import Data.Foldable hiding (toList)
import Debug.Trace
import Graphics.Gloss.Interface.IO.Interact
import qualified Data.String.Interpolate as I

ui :: IO ()
ui = play d white 5 initState draw control advance
  where
  d = InWindow "Tetris" (800, 600) (0, 0)
  advance _ = tr . gameStep
  draw GameState{..}
    = rectangleWire 250 500
    <> drawField gsField
    <> drawFalling gsGridPos gsFallingTetra
    <> (translate (-250*1.5) 250 . scale 0.2 0.2 $ text [I.i|Score: #{gsScore}|])
    <> drawFalling (GridPos 14 1) gsNextTetra
    <> translate 280 175 (translate -10 80 (scale 0.2 0.2 (text "Next")) <> rectangleWire 150 150)
  drawField field = fold $ mapWithIndex (go 0 0) field
  tr x = trace (show x) x
  go shiftX shiftY (i, j) val
    | Just tet <- val =
      let top = -250
          left = -250/2
          step = 25
          c2c k l = (left+fromIntegral (l+shiftX)*step, negate $ top+fromIntegral (k+shiftY)*step)
      in color (tetColor tet) $ polygon [
        c2c i j, c2c i (j+1), c2c (i+1) (j+1), c2c (i+1) j
        ]
    | otherwise = blank
  drawFalling GridPos{..} tetra = fold $ mapWithIndex (go gpX gpY) tetra
  control (EventKey (SpecialKey KeyLeft) Down _ _) = moveLeft
  control (EventKey (SpecialKey KeyRight) Down _ _) = moveRight
  control (EventKey (SpecialKey KeyUp) Down _ _) = Game.rotate
  control _ = id
  -- 500 = 20; 250 = 10
  -- top-left field is (675,50)

tetColor :: Tetramino -> Color
tetColor I = black
tetColor O = red
tetColor J = violet
tetColor L = magenta
tetColor T = azure
tetColor S = orange
tetColor Z = cyan
