module Game.Assets
  ( assets
  , plate
  , uma
  , umaAnimation
  ) where


import Prelude

import Data.Maybe (Maybe(..))
import Phina (Assets, ImageAsset(..), SpriteSheetAsset, animation, makeAssets, spriteSheet)


assets ∷ Assets
assets = makeAssets
  { image: [plate, uma]
  , spritesheet: [umaAnimation]
  }

img ∷ String → String
img = ("img/" <> _)

plate ∷ ImageAsset
plate = ImageAsset $ img "plate.png"

uma ∷ ImageAsset
uma = ImageAsset $ img "uma.png"

umaAnimation ∷ SpriteSheetAsset
umaAnimation = spriteSheet "uma"
    { frame:
        { width: 32
        , height: 32
        , cols: 2
        , rows: 1
        }
    , animations:
        [ animation "default"
            { frames: [0, 1]
            , next: Just "default"
            , frequency: 6
            }
        ]
    }
