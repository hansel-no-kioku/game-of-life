module Game
  ( game
  ) where

import Prelude

import Effect (Effect)
import Game.Config (config)
import Game.Main (mainScene)
import Phina (GameScenes(..), StartScene(..), foreverAsync, launchAsync', newGame, popup, runGame, splashScene)

game ∷ Effect Unit
game = newGame config scenes >>= runGame

  where
    scenes = SceneListDefault Main $ \_ _ → launchAsync' do
      _ ← popup splashScene {}
      foreverAsync mainScene
