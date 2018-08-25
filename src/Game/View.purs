module Game.View
  ( newView
  ) where


import Prelude

import Data.Array (zip, (..))
import Data.Int (floor)
import Data.Matrix (Matrix, fromArray, zipWith)
import Data.MatrixWithPos (toMatrix)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Effect.FSM (Machine, machine)
import Game.Assets (plate, uma, umaAnimation)
import Game.Config (style)
import Game.Controller.Message as C
import Game.Event (Control(..))
import Game.Plate (Creature(..), Plate)
import Game.View.Message as V
import Phina (class WritableProp, Button, DisplayScene, Label, Sprite, addChildB', addChildToB, flare, getSize, getSizeB, getSpan, getSpanPos, make, newButton, newDisplayElement, newGrid, newLabel, newSprite, onPointEndB, onPointMoveB, onPointStartB, onPushB, playAnimationB, setAnimationB, setInteractiveB, setPositionB, setProp, setProps, setPropsB, setText, setTextB, update)
import Type.Prelude (SProxy(..))


type State =
  { scene ∷ DisplayScene
  , cells ∷ Matrix Sprite
  , startButton ∷ Button
  , generation ∷ Label
  }


newView ∷ DisplayScene → Effect (Machine V.Msg Unit)
newView scene = do
  state ← initialState scene
  machine step state V.Init unit
  where
    step (V.Update plate isRunning generation) state =
      stepUpdate plate isRunning generation state
    step _ state = pure $ Tuple Nothing state


initialState ∷ DisplayScene → Effect State
initialState scene = do
  addTitle

  plate ← addPlate
  cells ← addCells plate
  startButton ← addStartButton
  generation ← addGeneration

  pure $ {scene, cells, startButton, generation}

  where
    addTitleLabel (Tuple text span) = do
      pos ← getSpanPos span.x span.y scene
      void $ make (newLabel style.title) do
        setTextB text
        setPositionB pos
        addChildToB scene

    addTitle = traverse_ addTitleLabel $ zip style.titleText style.layout.title

    addPlate = make (newDisplayElement style.plate) do
      size ← getSizeB
      addChildB' $ newSprite plate {x: size.width / 2.0, y: size.height / 2.0}
      addChildToB scene
      setInteractiveB true
      onPointStartB \p _ → flareMsg C.PointStart p
      onPointMoveB \p _ → flareMsg C.PointMove p
      onPointEndB \_ _ → flare Control C.PointEnd scene

    flareMsg msg {pointer} = do
      let pos = pointer.position -- - {x: left, y: top}
          x = floor $ pos.x / style.cellSize.width
          y = floor $ pos.y / style.cellSize.height
      log $ show pointer.position
      flare Control (msg {x, y}) scene


    addCells plate = do
      plateSize ← getSize plate

      let cellSize = style.cellSize
          cols = style.worldSize.width
          rows = style.worldSize.height
          gridX = newGrid plateSize.width cols false $ cellSize.width / 2.0
          gridY = newGrid plateSize.height rows false $ cellSize.height / 2.0

      fromArray <$>
        for (0..(rows - 1)) \cy → do
          for (0..(cols - 1)) \cx → do
            let pos = {x: getSpan cx gridX, y: getSpan cy gridY}
            make (newSprite uma style.creature) do
              setPositionB pos
              setAnimationB umaAnimation
              playAnimationB "default" false
              update $ setVisible false
              addChildToB plate

    addStartButton = do
      let startButtonSpan = style.layout.startButton
      startButtonPos ← getSpanPos startButtonSpan.x startButtonSpan.y scene
      make (newButton style.startButton.common) do
        setPropsB style.startButton.start
        addChildToB scene
        setPositionB startButtonPos
        onPushB \_ → flare Control C.ToggleRunningState scene

    addGeneration = do
      let labelSpan = style.layout.generationLabel
      labelPos ← getSpanPos labelSpan.x labelSpan.y scene
      _ ← make (newLabel style.generationLabel) do
        setPositionB labelPos
        addChildToB scene

      let generationSpan = style.layout.generation
      generationPos ← getSpanPos generationSpan.x generationSpan.y scene
      make (newLabel style.generation) do
        setPositionB generationPos
        addChildToB scene


stepUpdate ∷ Plate → Boolean → Int → State → Effect (Tuple (Maybe Unit) State)
stepUpdate plate isRunning generation state = do
  traverse_ (\c → updateCreature (fst c) (snd c))
              $ zipWith Tuple (toMatrix plate) state.cells

  let startButtonStyle = if isRunning then style.startButton.stop
                                      else style.startButton.start
  _ ← setProps startButtonStyle state.startButton

  _ ← setText (show generation) state.generation

  pure $ Tuple Nothing state

  where
    updateCreature exist = setVisible $ exist == Exist


setVisible ∷ ∀ a. WritableProp a "visible" Boolean ⇒ Boolean → a → Effect a
setVisible = setProp (SProxy ∷ SProxy "visible")
