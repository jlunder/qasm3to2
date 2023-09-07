module Ast (AstNode (..), SourceRef (..), isNilNode) where

data SourceRef
  = NilRef
  | TextRef {sourceModule :: String, sourceLine :: Int, sourceColumn :: Maybe Int}
  deriving (Eq, Read, Show)

data AstNode t c where
  NilNode :: AstNode t c
  AstNode :: {astTag :: t, astChildren :: [AstNode t c], astContext :: c} -> AstNode t c
  deriving (Eq, Read, Show)

isNilNode :: AstNode t c -> Bool
isNilNode NilNode = True
isNilNode (AstNode {}) = False
