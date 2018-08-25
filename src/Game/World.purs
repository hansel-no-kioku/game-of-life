module Game.World
  ( newWorld
  ) where


import Prelude

import Data.MatrixWithPos (Pos)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.FSM (Machine, machine)
import Game.Config (style)
import Game.Plate (Plate, changeGeneration, newPlate, togglePlate)
import Game.View.Message as V
import Game.World.Message as W


type State =
  { plate ∷ Plate
  , generation ∷ Int
  }

newWorld ∷ Effect (Machine W.Msg V.Msg)
newWorld = machine step initialState W.Init V.Init
  where
    step (W.RunningState isRunning) state = stepRunningState isRunning state
    step (W.Toggle pos isRunning) state = stepToggle pos isRunning state
    step (W.Elapse) state = stepElapse state
    step _ state = pure $ Tuple Nothing state


initialState ∷ State
initialState =
  let plate = newPlate style.worldSize.width style.worldSize.height
  in  {plate, generation: 1}


stepRunningState ∷ Boolean → State → Effect (Tuple (Maybe V.Msg) State)
stepRunningState isRunning state =
  pure $ Tuple (Just $ V.Update state.plate isRunning state.generation) state


stepToggle ∷ Pos → Boolean → State → Effect (Tuple (Maybe V.Msg) State)
stepToggle pos isRunning state = do
  let newPlate = togglePlate pos state.plate
      msg = V.Update newPlate isRunning state.generation
  pure $ Tuple (Just msg) state{plate = newPlate}


stepElapse ∷ State → Effect (Tuple (Maybe V.Msg) State)
stepElapse state = ado
  let plate = changeGeneration state.plate
      generation = state.generation + 1
      msg = V.Update plate true generation
  in Tuple (Just msg) {plate, generation}
