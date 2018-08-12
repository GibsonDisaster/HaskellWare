{-# LANGUAGE TemplateHaskell #-}

module MiniGames where
  import Graphics.Gloss.Interface.IO.Game
  import Graphics.Gloss.Game
  import Control.Monad
  import Control.Lens
  import Types
  import Util  

  minigamesList :: [MiniGame]
  minigamesList = [mgMakeGreen, mgCount, mgHeavy]

  {- Title Screen -}

  titleScreen :: MiniGame
  titleScreen = MiniGame { 
    _mgTitle = "Title Screen", 
    _mgWon = False, 
    _mgPictures = Pictures [],
    _mgSpecial = NoSpec,
    _mgPlayer = Player { _pX = 0.0, _pY = 0.0 },
    _mgUpdate = tU, 
    _mgHandleEvents = tHE, 
    _mgRender = tR
  }

  tR :: Game -> IO Picture
  tR g = do
    let splash = translate 0 0 $ png "MainImage.jpg"
    return $ Pictures [splash]

  tHE :: Event -> Game -> IO Game
  tHE (EventKey (Char _) Down _ _) g = return $ setWin g True
  tHE _ g = return g

  tU :: Game -> IO Game
  tU g = return g

  {- Green Minigame -}

  mgMakeGreen :: MiniGame
  mgMakeGreen = MiniGame { 
    _mgTitle = "Make Green!", 
    _mgWon = False, 
    _mgPictures = Pictures [],
    _mgSpecial = NoSpec,
    _mgPlayer = Player { _pX = 0.0, _pY = 0.0 },
    _mgUpdate = mgMakeGreenU, 
    _mgHandleEvents = mgMakeGreenHE, 
    _mgRender = mgMakeGreenR
  }

  mgMakeGreenR :: Game -> IO Picture
  mgMakeGreenR g = do
    let tip = translate (-100) (-100) $ color white $ scale 0.5 0.5 $ text "Make Green!"
    let player = translate (_pX (_mgPlayer (_currentMiniGame g))) (_pY (_mgPlayer (_currentMiniGame g))) $ color (if (_mgWon (_currentMiniGame g)) then green else blue) $ circleSolid 80
    let goal = translate 200.0 200.0 $ color yellow $ circleSolid 80
    let won = if _mgWon (_currentMiniGame g) then translate 0 0 (color red (text "WIN")) else Pictures []
    return (Pictures $ [tip, goal, player, won])

  mgMakeGreenHE :: Event -> Game -> IO Game
  mgMakeGreenHE (EventKey (Char 'w') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgPlayer = (_mgPlayer (_currentMiniGame g)) { _pY = (_pY (_mgPlayer (_currentMiniGame g))) + 20 } } }
  mgMakeGreenHE (EventKey (Char 's') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgPlayer = (_mgPlayer (_currentMiniGame g)) { _pY = (_pY (_mgPlayer (_currentMiniGame g))) - 20 } } }
  mgMakeGreenHE (EventKey (Char 'a') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgPlayer = (_mgPlayer (_currentMiniGame g)) { _pX = (_pX (_mgPlayer (_currentMiniGame g))) - 20 } } }
  mgMakeGreenHE (EventKey (Char 'd') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgPlayer = (_mgPlayer (_currentMiniGame g)) { _pX = (_pX (_mgPlayer (_currentMiniGame g))) + 20 } } } 
  mgMakeGreenHE _ g = return g

  mgMakeGreenU :: Game -> IO Game
  mgMakeGreenU g = do
    putStrLn (show $ _mgWon (_currentMiniGame g))
    let px = (_pX (_mgPlayer (_currentMiniGame g)))
    let py = (_pY (_mgPlayer (_currentMiniGame g)))
    let winCondition = px == 200 && py == 200
    if winCondition then return $ setWin g True else return g

  {- Counting Minigame -}

  mgCount :: MiniGame
  mgCount = MiniGame {
    _mgTitle = "Count!",
    _mgWon = False,
    _mgSpecial = Counter 0 False 5 [] False,
    _mgPictures = Pictures [],
    _mgPlayer = Player { _pX = 0.0, _pY = 0.0 },
    _mgUpdate = mgCountU,
    _mgHandleEvents = mgCountHE,
    _mgRender = mgCountR
  }

  mgCountR :: Game -> IO Picture
  mgCountR g = do
    let background = translate 0 0 $ png "FrogBackground.png"
    let countText = translate 185 (-185) $ scale 0.15 0.15 $ color white $ text (show (_counter (_mgSpecial (_currentMiniGame g))))
    let pics = map (\(x, y) -> translate x y $ color red $ png "Frog.png") (_countCoords (_mgSpecial (_currentMiniGame g)))
    let hand = translate 220 (-220) $ scale 1.5 1.5  $ png "Counter.png"
    let won = if _mgWon (_currentMiniGame g) then translate 0 0 (color red (text "WIN")) else Pictures []
    return $ Pictures ([background, won, hand, countText] ++ pics)

  mgCountHE :: Event -> Game -> IO Game
  mgCountHE (EventKey (Char 'z') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgSpecial = (_mgSpecial (_currentMiniGame g)) { _counter = (_counter (_mgSpecial (_currentMiniGame g))) + 1 } } }
  mgCountHE (EventKey (Char 'x') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgSpecial = (_mgSpecial (_currentMiniGame g)) { _doneCounting = True } } }
  mgCountHE _ g = return g

  mgCountU :: Game -> IO Game
  mgCountU g = do
    let num = _countAns (_mgSpecial (_currentMiniGame g))
    coords <- replicateM num getRandCoord
    let g' = if (_countPlaced (_mgSpecial (_currentMiniGame g))) then
               g
             else
               g { _currentMiniGame = (_currentMiniGame g) { _mgSpecial = (_mgSpecial (_currentMiniGame g)) { _countCoords = coords, _countPlaced = True } } }
    putStrLn (show $ (_mgWon (_currentMiniGame g')))
    if (_doneCounting (_mgSpecial (_currentMiniGame g'))) && not (_mgWon (_currentMiniGame g')) then 
      (if (_counter (_mgSpecial (_currentMiniGame g'))) == (_countAns (_mgSpecial (_currentMiniGame g'))) then return $ setWin g' True else return g') 
    else 
      return g'

  {- Heavier -}

  mgHeavy :: MiniGame
  mgHeavy = MiniGame {
    _mgTitle = "Which is heavier!",
    _mgWon = False,
    _mgSpecial = Heavier ([3, 2, 3], [3, 1, 1]) 8 [1, 1, 1] False,
    _mgPictures = Pictures [],
    _mgPlayer = Player { _pX = 0.0, _pY = 0.0 },
    _mgUpdate = mgHeavyU,
    _mgHandleEvents = mgHeavyHE,
    _mgRender = mgHeavyR
  }

  mgHeavyR :: Game -> IO Picture
  mgHeavyR g = do
    let side1Pic1 = translate (-100) 100 $ color red $ circleSolid (20.0 * (head (fst (_hSides (_mgSpecial (_currentMiniGame g))))))
    let side1Pic2 = translate (-100) 0 $ color red $ circleSolid (20.0 * ((head . drop 1) (fst (_hSides (_mgSpecial (_currentMiniGame g))))))
    let side1Pic3 = translate (-100) (-100) $ color red $ circleSolid (20.0 * (last (fst (_hSides (_mgSpecial (_currentMiniGame g))))))
    let side2Pic1 = translate 100 100 $ color blue $ circleSolid (20.0 * (head (snd (_hSides (_mgSpecial (_currentMiniGame g))))))
    let side2Pic2 = translate 100 0 $ color blue $ circleSolid (20.0 * ((head . drop 1) (snd (_hSides (_mgSpecial (_currentMiniGame g))))))
    let side2Pic3 = translate 100 (-100) $ color blue $ circleSolid (20.0 * (last (snd (_hSides (_mgSpecial (_currentMiniGame g))))))
    let choice = translate (_pX (_mgPlayer (_currentMiniGame g))) (_pY (_mgPlayer (_currentMiniGame g))) $ color green $ circleSolid 30
    let won = if _mgWon (_currentMiniGame g) then translate 0 0 (color red (text "WIN")) else Pictures []
    return $ Pictures [side1Pic1, side1Pic2, side1Pic3, side2Pic1, side2Pic2, side2Pic3, choice, won]

  mgHeavyHE :: Event -> Game -> IO Game
  mgHeavyHE (EventKey (Char 'a') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgSpecial = (_mgSpecial (_currentMiniGame g)) { _hChoice = fst (_hSides (_mgSpecial (_currentMiniGame g))) }, _mgPlayer = (_mgPlayer (_currentMiniGame g)) { _pX = (-100) } } }
  mgHeavyHE (EventKey (Char 'd') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgSpecial = (_mgSpecial (_currentMiniGame g)) { _hChoice = snd (_hSides (_mgSpecial (_currentMiniGame g))) }, _mgPlayer = (_mgPlayer (_currentMiniGame g)) { _pX = (100) } } }
  mgHeavyHE (EventKey (Char 'x') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgSpecial = (_mgSpecial (_currentMiniGame g)) { _hChosen = True } } }
  mgHeavyHE _ g = return g
  --Bug where player can 'choose' and then move to correct one afterward. Change to not allow movement if player has chosen
  mgHeavyU :: Game -> IO Game
  mgHeavyU g = do
    putStrLn (show $ (_mgWon (_currentMiniGame g)))
    let g' = if (_hChosen (_mgSpecial (_currentMiniGame g))) then
               if (_hAns (_mgSpecial (_currentMiniGame g)) == (foldr (+) 0.0 (_hChoice (_mgSpecial (_currentMiniGame g))))) then g { _currentMiniGame = (_currentMiniGame g) { _mgWon = True } } else g
             else
               g
    return g'

  {- Word Choice -}

  mgWordChoice :: MiniGame
  mgWordChoice = MiniGame {
    _mgTitle = "Compliment!",
    _mgWon = False,
    _mgSpecial = WordChoice "Amazing" False ["Amazing", "The Smallest", "Smelly", "The Weirdest"],
    _mgPictures = Pictures [],
    _mgPlayer = Player { _pX = 0.0, _pY = 0.0 },
    _mgUpdate = mgWordChoiceU,
    _mgHandleEvents = mgWordChoiceHE,
    _mgRender = mgWordChoiceR
  }

  mgWordChoiceR :: Game -> IO Picture
  mgWordChoiceR g = do
    let one = translate 0 100 $ scale 0.5 0.5 $ text ((_wcChoices (_mgSpecial (_currentMiniGame g))) !! 0)
    let player = translate (_pX (_mgPlayer (_currentMiniGame g))) (_pY (_mgPlayer (_currentMiniGame g))) $ color green $ circleSolid 30 
    return $ Pictures [one, player]

  mgWordChoiceHE :: Event -> Game -> IO Game
  mgWordChoiceHE (EventKey (Char 'w') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgPlayer = (_mgPlayer (_currentMiniGame g)) { _pX = 0, _pY = 100 } } }
  mgWordChoiceHE (EventKey (Char 's') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgPlayer = (_mgPlayer (_currentMiniGame g)) { _pX = 0, _pY = (-100) } } }
  mgWordChoiceHE (EventKey (Char 'a') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgPlayer = (_mgPlayer (_currentMiniGame g)) { _pX = (-100), _pY = 0 } } }
  mgWordChoiceHE (EventKey (Char 'd') Down _ _) g = return g { _currentMiniGame = (_currentMiniGame g) { _mgPlayer = (_mgPlayer (_currentMiniGame g)) { _pX = (100), _pY = 0 } } }
  mgWordChoiceHE _ g = return g

  mgWordChoiceU :: Game -> IO Game
  mgWordChoiceU = return