module Game.World.Message
  ( Msg(..)
  ) where


import Data.MatrixWithPos (Pos)


data Msg  = Init
          | RunningState Boolean
          | Toggle Pos Boolean
          | Elapse
