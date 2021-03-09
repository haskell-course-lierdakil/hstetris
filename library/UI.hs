{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
module UI where

import Graphics.Gloss
import Game
import Data.Foldable hiding (toList)
import Data.Binary
import Data.List
import Data.Char
import System.Random
import System.Exit
import qualified Data.Ord as Ord
import Control.Exception
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Network.HTTP.Req
import Control.Retry
import System.FilePath
import System.Directory
import Graphics.Gloss.Interface.IO.Game
import qualified Data.String.Interpolate as I
import Control.Concurrent.Async

data ExtendedState = ExtendedState { esWorld :: !GameState, esGotOnlineScore :: !Bool }

myHttpConfig :: HttpConfig
myHttpConfig = defaultHttpConfig {
    httpConfigRetryPolicy = retryPolicy $ const Nothing
  }

scoreServer :: Url 'Https
scoreServer = https "hstetris.livid.pp.ru" /: "score"

getOnlineScore :: IO [(String, Word)]
getOnlineScore = fmap (M.toList . responseBody) . runReq myHttpConfig $ req GET
  scoreServer
  NoReqBody
  jsonResponse -- specify how to interpret response
  mempty

ui :: IO ()
ui = do
    stdGen <- newStdGen
    score <- loadScore
    let gameState = (initState stdGen){gsScoreTable=score}
        state = ExtendedState { esWorld = gameState, esGotOnlineScore = False }
    withAsync getOnlineScore $ \scoreAsync ->
      playIO d white 60 state
        (pure . draw . esWorld)
        control
        (advance scoreAsync)
  where
  d = FullScreen --InWindow "Tetris" (800, 600) (0, 0)
  advance scoreAsync delay state@ExtendedState{..}
    | esGotOnlineScore
    = pure state{esWorld=gameStep delay esWorld}
    | otherwise
    = do asyncScore <- poll scoreAsync
         case asyncScore of
           Just (Right sc) -> do
             newScore <- saveScore' (gsScoreTable esWorld <> sc)
             pure state{ esWorld=(gameStep delay esWorld){gsScoreTable=newScore}
                       , esGotOnlineScore=True}
           _ -> pure state{esWorld=gameStep delay esWorld}
  draw GameState{..}
    | gsGamePhase == Finished
    = translate -200 100 (scale 0.2 0.2 $ text [I.i|Score: #{gsScore}|])
    <> translate -200 0 (scale 0.2 0.2 $ text "Enter name:")
    <> translate -200 -100 (scale 0.2 0.2 . text $ reverse gsPlayerName)
    | otherwise
    = rectangleWire 250 500
    <> drawField gsField
    <> (if gsGamePhase == Falling then drawFalling gsGridPos gsFallingTetro else mempty)
    <> (translate (-250*1.5) 190 . scale 0.15 0.15 $ drawTopScore gsScoreTable)
    <> (translate (-250*1.5) 250 . scale 0.2 0.2 $ text [I.i|Score: #{gsScore}|])
    <> drawFalling (GridPos 14 1) gsNextTetro
    <> translate 280 175 (translate -10 80 (scale 0.2 0.2 (text "Next")) <> rectangleWire 150 150)
  drawField field = fold $ mapWithIndex (go 0 0) field
  drawTopScore = foldMap (uncurry formatScoreLine) . zip [0..]
  formatScoreLine n (name, score) = translate 0 (-250*n) $ text [I.i|#{name}: #{score}|]
  go shiftX shiftY (i, j) val
    | (j+shiftX) < 0 || (i+shiftY) < 0 = blank
    | Just tet <- val =
      let top = -250
          left = -250/2
          step = 25
          c2c k l = (left+fromIntegral (l+shiftX)*step, negate $ top+fromIntegral (k+shiftY)*step)
      in color (tetColor tet) $ polygon [
        c2c i j, c2c i (j+1), c2c (i+1) (j+1), c2c (i+1) j
        ]
    | otherwise = blank
  drawFalling GridPos{..} = fold . mapWithIndex (go gpX gpY)
  control (EventKey k Down _ _) w
    | (gsGamePhase . esWorld) w == Finished
    = modifyWorld w $ case k of
        SpecialKey KeyEnter -> const $ do
          newScore <- saveScore (esWorld w)
          stdGen <- newStdGen
          pure (initState stdGen){gsScoreTable = newScore}
        SpecialKey KeyBackspace -> pure . backspaceName
        SpecialKey KeyDelete -> pure . backspaceName
        SpecialKey KeyEsc -> const exitSuccess
        Char '\b' -> pure . backspaceName
        Char c | isAscii c -> pure . appendName c
        _ -> pure
  control (EventKey k Down _ _) w = modifyWorld w $ pure . case k of
      SpecialKey KeyLeft   -> moveLeft
      SpecialKey KeyRight  -> moveRight
      SpecialKey KeyDown   -> slamDown
      SpecialKey KeyUp     -> Game.rotate
      SpecialKey KeyEsc    -> stopGame
      _ -> id
  control _ w = pure w
  modifyWorld :: Monad m => ExtendedState -> (GameState -> m GameState) -> m ExtendedState
  modifyWorld es@ExtendedState{..} f = f esWorld >>= \newWorld -> pure es{esWorld=newWorld}

tetColor :: Tetromino -> Color
tetColor I = black
tetColor O = red
tetColor J = violet
tetColor L = magenta
tetColor T = azure
tetColor S = orange
tetColor Z = cyan

getScoreFile :: IO FilePath
getScoreFile = (</> ".config" </> "tetris" </> "score.dat") <$> getHomeDirectory

postOnlineScore :: [(String, Word)] ->  IO ()
postOnlineScore score = do
  _ <- runReq myHttpConfig (req POST
    scoreServer
    (ReqBodyJson score)
    lbsResponse
    mempty)
    `catch` (\e -> print (e :: HttpException) >> return undefined)
  return ()

saveScore :: GameState -> IO [(String, Word)]
saveScore w = saveScore' ((reverse $ gsPlayerName w, gsScore w) : gsScoreTable w)

saveScore' :: [(String, Word)] -> IO [(String, Word)]
saveScore' w = do
  scorefile <- getScoreFile
  createDirectoryIfMissing True $ takeDirectory scorefile
  let score = take 10 $ nub $ sortOn (Ord.Down . snd) w
  _ <- async $ postOnlineScore score
  BS.writeFile scorefile $ encode score
  return score

loadScore :: IO [(String, Word)]
loadScore = do
  scorefile <- getScoreFile
  f <- BS.readFile scorefile
  case decodeOrFail f of
    Left _ -> return []
    Right (_, _, result) -> return result
  `catch` (\e -> print (e :: SomeException) >> return [])
