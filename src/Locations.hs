{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Helpers for tracking source locations of tokens and structures.
module Locations (
  Location (..),
  WithLoc (..),
  startLoc,
  endLoc,
  location,
  value,
) where

import Prettyprinter
import Lens.Micro
import Lens.Micro.TH
import Text.Megaparsec.Pos

-- | The location of a token within a source file.
data Location = Location
  { -- | The start of the token.
    _startLoc :: !SourcePos
  , -- | The end of the token.
    _endLoc :: !SourcePos
  }
  deriving (Eq, Ord, Show)

makeLenses ''Location

instance Semigroup Location where
  l1 <> l2 = Location (l1 ^. startLoc) (l2 ^. endLoc)

-- | Wraps a type with a location.
data WithLoc a = WithLoc
  { _location :: Location
  , _value :: a
  }
  deriving (Eq, Ord, Show)

makeLenses ''WithLoc

instance Pretty a => Pretty (WithLoc a) where
  pretty (WithLoc _ v) = pretty v
  prettyList locs = prettyList $ locs ^.. each . value
