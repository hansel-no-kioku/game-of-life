module Game.Controller
  ( newController
  ) where


import Prelude

import Data.MatrixWithPos (Pos)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.FSM (Machine, machine)
import Game.Config (stepTime)
import Game.Controller.Message as C
import Game.World.Message as W
import Phina (Duration, msec)

type State =
  { isRunning ∷ Boolean
  , elapsedTime ∷ Duration
  , isPointing ∷ Boolean
  , pos ∷ Pos
  }

newController ∷ Effect (Machine C.Msg W.Msg)
newController = machine step initialState C.Init W.Init
  where
    initialState =  { isRunning: false
                    , elapsedTime: msec 0.0
                    , isPointing: false
                    , pos: zero
                    }

    step (C.PointStart pos) state = stepPointStart pos state
    step (C.PointMove pos) state = stepPointMove pos state
    step C.PointEnd state = pure $ Tuple Nothing state{isPointing = false}
    step C.ToggleRunningState state = stepToggleRunningState state
    step (C.Elapse deltaTime) state = stepElapse deltaTime state
    step _ state = pure $ Tuple Nothing state


stepPointStart ∷ Pos → State → Effect (Tuple (Maybe W.Msg) State)
stepPointStart pos state = ado
  let newState = state{isPointing = true, pos = pos}
  in  Tuple (Just $ W.Toggle pos state.isRunning) newState


stepPointMove ∷ Pos → State → Effect (Tuple (Maybe W.Msg) State)
stepPointMove pos state = pure $
  if state.isPointing && pos /= state.pos
    then Tuple (Just $ W.Toggle pos state.isRunning) state{pos = pos}
    else Tuple Nothing state


stepToggleRunningState ∷ State → Effect (Tuple (Maybe W.Msg) State)
stepToggleRunningState state = ado
  let isRunning = not state.isRunning
  in Tuple (Just $ W.RunningState isRunning) state{isRunning = isRunning}


stepElapse ∷ Duration → State → Effect (Tuple (Maybe W.Msg) State)
stepElapse deltaTime state = pure $
  if state.isRunning
    then
      let elapsedTime = state.elapsedTime + deltaTime
      in if elapsedTime >= stepTime
          then Tuple (Just W.Elapse) state{elapsedTime = elapsedTime - stepTime}
          else Tuple Nothing state{elapsedTime = elapsedTime}
    else
      Tuple Nothing state

