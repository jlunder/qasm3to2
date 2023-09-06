module Ast (AstNode (..), SourceRef (..), normAst) where

data SourceRef
  = NilRef
  | TextRef {sourceModule :: String, sourceLine :: Int, sourceColumn :: Maybe Int}
  deriving (Eq, Read, Show)

data (Eq t, Read t, Show t, Eq c, Read c, Show c) => AstNode t c
  = NilNode
  | AstNode {astTag :: t, astChildren :: [AstNode t c], astContext :: c}
  deriving (Eq, Read, Show)

-- Normalization removes context from an AST, so that ASTs can be compared
-- irrespective of how the tree was constructed from source text
normAst :: (Eq t, Read t, Show t, Eq c, Read c, Show c) => AstNode t c -> AstNode t ()
normAst (AstNode tag children _) = normAst (AstNode tag (map normAst children) ())
normAst NilNode = NilNode
