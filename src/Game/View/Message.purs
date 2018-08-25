module Game.View.Message
  ( Msg(..)
  ) where

import Game.Plate (Plate)

data Msg  = Init
          | Update Plate Boolean Int
