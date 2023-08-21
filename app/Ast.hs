module Ast where

data SourceRef = TextRef {moduleName :: String, sourceLine :: Int, sourceColumn :: Maybe Int} deriving (Eq, Read, Show)

class AstNode a where
  pretty :: a -> String
  pretty _ = ""
  sourceRef :: a -> Maybe SourceRef
  sourceRef _ = Nothing
