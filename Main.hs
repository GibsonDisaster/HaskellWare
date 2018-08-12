{-# LANGUAGE TemplateHaskell #-}

module Main where
  import Graphics.Gloss.Interface.IO.Game
  import Control.Monad
  import Control.Lens
  import System.Random
  import MiniGames
  import Types
  import Util

  initGame :: Game
  initGame = Game {
    _gameTimer = 0.0,
    _allMinigames = minigamesList,
    _currentMiniGame = titleScreen
  }

  renderGame :: Game -> IO Picture
  renderGame g = (_mgRender (_currentMiniGame g)) g

  handleEvents :: Event -> Game -> IO Game
  handleEvents e g = (_mgHandleEvents (_currentMiniGame g)) e g

  updateGame :: Float -> Game -> IO Game
  updateGame f g = do
    let t = (_gameTimer g)
    let (newGame, allGames) = shuffle (_allMinigames g)
    if t >= 7.0 then (_mgUpdate newGame g { _allMinigames = (allGames), _currentMiniGame = newGame, _gameTimer = 0.0 }) else (_mgUpdate (_currentMiniGame g)) g { _gameTimer = (((_gameTimer) g) + f) }

  main :: IO ()
  main = playIO (InWindow "HaskellWare" (600, 600) (660, 240)) black 60 initGame renderGame handleEvents updateGame