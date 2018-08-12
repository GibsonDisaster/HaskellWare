{-# LANGUAGE TemplateHaskell #-}

module Util where
  import Graphics.Gloss.Game
  import Control.Monad
  import Control.Lens
  import System.Random
  import Types

  debug :: String -> IO ()
  debug = appendFile "log.txt"

  getRandCoord :: IO (Float, Float)
  getRandCoord = do
    x <- randomRIO (-200.0, 200.0)
    y <- randomRIO (-200.0, 200.0)
    return (x, y)

  shuffle :: [a] -> (a, [a])
  shuffle l = (h, r)
    where
      h = head l
      r = (drop 1 l) ++ [h]

  {- Lens Functions -}
  setWin :: Game -> Bool -> Game
  setWin g b = set (currentMiniGame . mgWon) b g

  changePlayerX :: Game -> Float -> Game
  changePlayerX g f = over (currentMiniGame . mgPlayer . pX) (+ f) g

  changePlayerY :: Game -> Float -> Game
  changePlayerY g f = over (currentMiniGame . mgPlayer . pY) (+ f) g

  setPlayerX :: Game -> Float -> Game
  setPlayerX g f = set (currentMiniGame . mgPlayer . pX) f g

  setPlayerY :: Game -> Float -> Game
  setPlayerY g f = set (currentMiniGame . mgPlayer . pY) f g