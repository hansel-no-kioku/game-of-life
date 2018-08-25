module Data.MatrixWithPos
  ( Pos
  , MatrixPos
  , MatrixWithPos
  , fromMatrix
  , toMatrix
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Store (class ComonadStore)
import Control.Extend (class Extend)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Matrix (Matrix, index)
import Data.Maybe (Maybe(..), fromMaybe)


type Pos = {x ∷ Int, y ∷ Int}

type MatrixPos = Maybe Pos


newtype MatrixWithPos a = MatrixWithPos {pos ∷ MatrixPos, off ∷ a, matrix ∷ Matrix a}

instance functorMatrixWithPos ∷ Functor MatrixWithPos where
  map f (MatrixWithPos {pos, off, matrix})
    = MatrixWithPos {pos, off: f off, matrix: f <$> matrix}

instance extendMatrixWithPos ∷ Extend MatrixWithPos where
  extend f (MatrixWithPos p) =
    let newOff = f $ MatrixWithPos p{pos = Nothing}
        newMatrix = mapWithIndex (\i _ → f $ MatrixWithPos p{pos = Just i}) p.matrix
    in  MatrixWithPos p{off = newOff, matrix = newMatrix}

instance comonadMatrixWithPos ∷ Comonad MatrixWithPos where
  extract (MatrixWithPos p) = case p.pos of
    Nothing → p.off
    Just pos → fromMaybe p.off $ index pos.x pos.y p.matrix

instance comonadStoreMatrixWithPos
  ∷ ComonadStore (Maybe {x ∷ Int, y ∷ Int}) MatrixWithPos where
    pos (MatrixWithPos p) = p.pos
    peek pos (MatrixWithPos p) = extract $ MatrixWithPos p{pos = pos}


fromMatrix ∷ ∀ a. a → Matrix a → MatrixWithPos a
fromMatrix off matrix = MatrixWithPos {pos: Nothing, off, matrix}

toMatrix ∷ ∀ a. MatrixWithPos a → Matrix a
toMatrix (MatrixWithPos p) = p.matrix
