module Ast (AstNode (..), SourceRef (..)) where

import Data.Maybe

data SourceRef = TextRef {moduleName :: String, sourceLine :: Int, sourceColumn :: Maybe Int}
  deriving (Eq, Read, Show)

class AstNode a where
  tryPretty :: a -> Maybe String
  tryPretty _ = Nothing
  pretty :: a -> String
  pretty x = fromMaybe undefined (tryPretty x)
  sourceRef :: a -> Maybe SourceRef
  sourceRef _ = Nothing
