module Ast (AstNode, pretty) where

class AstNode a where
  pretty :: a -> String
