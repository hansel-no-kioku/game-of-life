module Game.Event
  ( Control(..)
  ) where


import Game.Controller.Message (Msg)
import Phina (class Event)

data Control = Control

instance eventControl ∷ Event Control Msg where
  event _ = "control"
