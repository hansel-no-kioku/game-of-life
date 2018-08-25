module Game.Config
  ( config
  , stepTime
  , style
  ) where

import Prelude

import Data.Int (floor)
import Game.Assets (assets)
import Phina (color, msec)

config =
  { width: 960.0
  , height: 640.0
  , assets
  }


stepTime = msec 125


style =
  { scene:
      { backgroundColor: color "#444"
      }
  , plate:
      { width: plateSize.width
      , height: plateSize.height
      , originX: 0.0
      , originY: 0.0
      -- , padding: 0.0
      -- , fill: color "#333"
      -- , stroke: nullColor
      }

  , cellSize
  , worldSize

  , creature:
      { scaleX: 0.5
      , scaleY: 0.5
      }

  , startButton:
      { common:
          { width: 144.0
          , height: 48.0
          , cornerRadius: 16.0
          , fontColor: color "#eee"
          , stroke: color "#eee"
          , strokeWidth: 4.0
          }
      , start:
          { text: "Start"
          , fill: color "#36c"
          }
      , stop:
          { text: "Stop"
          , fill: color "#c63"
          }
      }

  , generationLabel:
      { text: "Generation"
      , fontSize: 28.0
      , fill: color "white"
      }

  , generation:
      { text: "1"
      , fontSize: 24.0
      , fill: color "white"
      }

  , title:
      { fontSize: 32.0
      , fontFamily: "cursive"
      , fill: color "white"
      , stroke: color "yellow"
      , strokeWidth: 1.0
      }
  , titleText: ["Conway's", "Game of Life"]

  , layout:
      { startButton:
          { x: 14
          , y: 2
          }
      , generationLabel:
          { x: 14
          , y: 5
          }
      , generation:
          { x: 14
          , y: 6
          }
      , title:
          [ { x: 14
            , y: 13
            }
          , { x: 14
            , y: 14
            }
          ]
      }
  }


plateSize ∷ {width ∷ Number, height ∷ Number}
plateSize =
  { width: 720.0
  , height: 640.0
  }


cellSize ∷ {width ∷ Number, height ∷ Number}
cellSize =
  { width: 16.0
  , height: 16.0
  }


worldSize ∷ {width ∷ Int, height ∷ Int}
worldSize =
  let width = floor $ plateSize.width / cellSize.width
      height = floor $ plateSize.height / cellSize.height
  in {width, height}
