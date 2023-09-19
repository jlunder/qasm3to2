module Ast (AstNode (..), SourceRef (..), isNilNode) where

data SourceRef where
  NilRef :: SourceRef
  TextRef :: {sourceModule :: String, sourceLine :: Int, sourceColumn :: Maybe Int} -> SourceRef
  deriving (Eq, Read, Show)

data AstNode t c where
  NilNode :: AstNode t c
  AstNode :: {astTag :: t, astChildren :: [AstNode t c], astContext :: c} -> AstNode t c
  deriving (Eq, Read, Show)

isNilNode :: AstNode t c -> Bool
isNilNode NilNode = True
isNilNode _ = False

{--

-- I'm not sure these are good for anything

instance Functor (AstNode t) where
  fmap :: (a -> b) -> AstNode t a -> AstNode t b
  fmap f NilNode = NilNode
  fmap f (AstNode tag children ctx) = AstNode tag (map (fmap f) children) (f ctx)

instance Foldable (AstNode t) where
  foldMap :: (Monoid m) => (a -> m) -> AstNode t a -> m
  foldMap f NilNode = mempty
  foldMap f (AstNode tag children ctx) = f ctx <> foldMap (foldMap f) children

instance Traversable (AstNode t) where
  sequenceA :: (Applicative f) => AstNode t (f a) -> f (AstNode t a)
  sequenceA NilNode = pure NilNode
  sequenceA (AstNode tag children ctxA) =
    let mappedChildren = traverse sequenceA children
     in (AstNode tag <$> mappedChildren) <*> ctxA

--}
