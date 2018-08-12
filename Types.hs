{-# LANGUAGE TemplateHaskell #-}

module Types where
  import Graphics.Gloss.Interface.IO.Game
  import Control.Lens

  data Special = Counter { _counter :: Int, _doneCounting :: Bool, _countAns :: Int, _countCoords :: [(Float, Float)], _countPlaced :: Bool }
               | Heavier { _hSides :: ([Float], [Float]), _hAns :: Float, _hChoice :: [Float], _hChosen :: Bool }
               | WordChoice { _wcAns :: String, _wcChosen :: Bool, _wcChoices :: [String] }
               | NoSpec
               deriving (Show, Eq)

  data MiniGame = MiniGame {
    _mgTitle :: String,
    _mgWon :: Bool,
    _mgPictures :: Picture,
    _mgSpecial :: Special,
    _mgPlayer :: Player,
    _mgUpdate :: Game -> IO Game,
    _mgHandleEvents :: Event -> Game -> IO Game,
    _mgRender :: Game -> IO Picture
  }

  data Game = Game {
    _gameTimer :: Float,
    _allMinigames :: [MiniGame],
    _currentMiniGame :: MiniGame
  } deriving Eq

  data Player = Player {
    _pX :: Float,
    _pY :: Float
  } deriving Eq

  instance Eq MiniGame where
    (==) (MiniGame t w pics spec pl _ _ _) (MiniGame t2 w2 pics2 spec2 pl2 _ _ _) = t == t2 && w == w2 && pics == pics2 && spec == spec2 && pl == pl2

  makeLenses ''Special
  makeLenses ''MiniGame
  makeLenses ''Game
  makeLenses ''Player