module Typing (
    Sexpr (..),
    Identifier (..)
) where

import qualified Data.List.NonEmpty as NEL

data Sexpr = LSexpr (NEL.NonEmpty Sexpr)
    | ABool Bool
    | AInt Int
    | AStr String
    | AIdent Identifier

    deriving (Show)

newtype Identifier = Identifier { getId :: String }
    deriving (Show)
