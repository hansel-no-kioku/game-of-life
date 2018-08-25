module Game.Plate
  ( Creature(..)
  , toggle
  , Plate
  , newPlate
  , togglePlate
  , changeGeneration
  ) where


import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (peeks, pos)
import Control.Extend (extend)
import Data.Foldable (foldl)
import Data.Matrix (replicate)
import Data.MatrixWithPos (MatrixWithPos, Pos, fromMatrix)
import Data.Maybe (Maybe(..))


data Creature = Absent
              | Exist

derive instance eqCreature ∷ Eq Creature

toggle ∷ Creature → Creature
toggle Absent = Exist
toggle Exist = Absent


type Plate = MatrixWithPos Creature


newPlate ∷ Int → Int → Plate
newPlate w h = fromMatrix Absent $ replicate w h Absent


togglePlate ∷ Pos → Plate → Plate
togglePlate position = extend \plate →
  let now = extract plate
      position' = pos plate
  in  if (Just position) == position' then toggle now else now


changeGeneration ∷ Plate → Plate
changeGeneration = extend \plate →
  let now = extract plate
      num = countNeighbor plate
  in  case now of
    Absent | num == 3 → Exist
    Exist  | num <= 1 → Absent
    Exist  | num >= 4 → Absent
    _                 → now

  where
    countNeighbor plate = foldl (\num dir → num + count plate dir) 0 directions

    count plate dir = case peeks ((_ + dir) <$> _) plate of
      Absent → 0
      Exist → 1

    directions =
      [ {x: -1, y: -1}
      , {x:  0, y: -1}
      , {x:  1, y: -1}
      , {x:  1, y:  0}
      , {x:  1, y:  1}
      , {x:  0, y:  1}
      , {x: -1, y:  1}
      , {x: -1, y:  0}
      ]

