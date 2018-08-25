module Game.Controller.Message
  ( Msg(..)
  ) where


import Data.MatrixWithPos (Pos)
import Phina (Duration)


data Msg  = Init
          | PointStart Pos
          | PointMove Pos
          | PointEnd
          | ToggleRunningState
          | Elapse Duration
