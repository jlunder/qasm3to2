module Ast (AstNode (..), SourceRef (..)) where

import Data.Maybe

data SourceRef = TextRef {moduleName :: String, sourceLine :: Int, sourceColumn :: Maybe Int}
  deriving (Eq, Read, Show)

class AstNode a where
  pretty :: a -> String
  sourceRef :: a -> Maybe SourceRef
  sourceRef _ = Nothing
