module Game.Main
  ( mainScene
  ) where

import Prelude

import Effect.FSM (connect, send)
import Game.Config (style)
import Game.Controller (newController)
import Game.Controller.Message (Msg(..))
import Game.Event as E
import Game.View (newView)
import Game.World (newWorld)
import Phina (Async, DisplayScene, getProp, on, popup, setProps, setUpdater, toSceneHandle)
import Type.Prelude (SProxy(..))

mainScene ∷ Async DisplayScene {}
mainScene = flip popup {} $ toSceneHandle \_ exit scene → do
  _ ← setProps style.scene scene

  controller ← newController
  world ← newWorld
  view ← newView scene

  _ ← connect controller world
  _ ← connect world view

  _ ← on E.Control (\e _ → send e controller) scene
  _ ← setUpdater (updater controller) scene

  pure scene

  where
    updater controller app _ = do
      deltaTime ← getProp (SProxy ∷ SProxy "deltaTime") app
      send (Elapse deltaTime) controller
