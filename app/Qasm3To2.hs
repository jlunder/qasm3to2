{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use second" #-}

module Qasm3To2 where

import Control.Monad.Trans.State.Lazy
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

data Type
  = TInt
  | TBool
  | TError
  | TVar Int
  | TArr Type Type
  deriving (Eq, Ord, Read, Show)

data Constraint
  = CEq Type Type
  | CError
  deriving (Eq, Ord, Read, Show)

type ConstraintSet = Set.Set Constraint

type ConstraintList = [Constraint]

type VarId = String

type Env = Map.Map VarId Type

type InferState a = State Int a

data Expr
  = CInt Int
  | CBool Bool
  | Var VarId
  | Plus Expr Expr
  | Minus Expr Expr
  | Equal Expr Expr
  | ITE Expr Expr Expr
  | Abs VarId Expr
  | App Expr Expr
  | LetIn VarId Expr Expr
  deriving (Eq, Ord, Read, Show)

getFreshTVar :: InferState Type
getFreshTVar = do
  i <- get
  put (i + 1)
  return (TVar i)

infer :: Env -> Expr -> InferState (Type, ConstraintSet)
-- CT-Int
infer g (CInt _) = return (TInt, Set.empty)
-- CT-Bool
infer g (CBool _) = return (TBool, Set.empty)
-- CT-Ident (substitution portion of CT-Let)
infer g (Var v) =
  let t = Map.findWithDefault TError v g
   in return (t, if t == TError then Set.fromList [CError] else Set.empty)
-- CT-Plus
infer g (Plus x y) = do
  (xType, c1) <- infer g x
  (yType, c2) <- infer g y
  return (TInt, Set.unions [c1, c2, Set.fromList [(CEq xType TInt), (CEq yType TInt)]])

-- CT-Minus
infer g (Minus x y) = do
  (xType, c1) <- infer g x
  (yType, c2) <- infer g y
  return (TInt, Set.unions [c1, c2, Set.fromList [(CEq xType TInt), (CEq yType TInt)]])

-- CT-Eq
infer g (Equal x y) = do
  (xType, c1) <- infer g x
  (yType, c2) <- infer g y
  return (TBool, Set.unions [c1, c2, Set.fromList [CEq xType yType]])

-- CT-ITE
infer g (ITE test ifT ifF) = do
  (testType, c1) <- infer g test
  (ifTType, c2) <- infer g ifT
  (ifFType, c3) <- infer g ifF
  return (ifTType, Set.unions [c1, c2, c3, Set.fromList [CEq ifTType ifFType, CEq testType TBool]])

-- CT-Abs
infer g (Abs x e) = do
  pType <- getFreshTVar
  let gWithX = Map.insert x pType g
  (eType, c) <- infer gWithX e
  return (TArr pType eType, c)

-- CT-App
infer g (App f x) = do
  pType <- getFreshTVar
  rType <- getFreshTVar
  (fType, c1) <- infer g f
  (xType, c2) <- infer g x
  return (rType, Set.unions [c1, c2, Set.fromList [CEq fType (TArr pType rType), CEq xType pType]])

-- CT-Let
infer g (LetIn v e1 e2) = do
  vType <- getFreshTVar
  let gWithV = Map.insert v vType g
  (e1Type, c1) <- infer gWithV e1
  (e2Type, c2) <- infer gWithV e2
  return (e2Type, Set.unions [c1, c2, Set.fromList [CEq vType e1Type]])

inferExpr :: Expr -> (Type, ConstraintSet)
inferExpr expr = let (result, _) = (runState (infer Map.empty expr) 0) in result

toCstrList :: ConstraintSet -> ConstraintList
toCstrList = Set.toList

type Substitution = Map.Map Type Type

applySub :: Substitution -> Type -> Type
applySub subs (TVar i) = Map.findWithDefault (TVar i) (TVar i) subs
applySub subs (TArr pType rType) = TArr (applySub subs pType) (applySub subs rType)
applySub subs t = t

applySubToCstr :: Substitution -> Constraint -> Constraint
applySubToCstr subs (CEq t1 t2) = CEq (applySub subs t1) (applySub subs t2)
applySubToCstr subs CError = CError

applySubToCstrList :: Substitution -> ConstraintList -> ConstraintList
applySubToCstrList subs = map (applySubToCstr subs)

composeSub :: Substitution -> Substitution -> Substitution
composeSub subs1 subs2 =
  let subs2List = (Map.toList subs2)
      subs1subs2List = map (\(k, a) -> (k, (applySub subs1 a))) subs2List
   in Map.union (Map.fromList subs1subs2List) subs1

tvars :: Type -> Set.Set Type
tvars TInt = Set.empty
tvars TBool = Set.empty
tvars (TVar i) = Set.fromList [TVar i]
tvars (TArr pType rType) = Set.union (tvars pType) (tvars rType)

unify :: ConstraintList -> Maybe Substitution
unify [] = Just Map.empty
unify ((CEq s t) : tail)
  | s == t = unify tail
  | (case s of (TVar _) -> True; _ -> False) && not (Set.member s (tvars t)) = extractSubst s t
  | (case t of (TVar _) -> True; _ -> False) && not (Set.member t (tvars s)) = extractSubst t s
  | otherwise =
      case (s, t) of
        ((TArr s1 s2), (TArr t1 t2)) -> unify ([(CEq s1 t1), (CEq s2 t2)] ++ tail)
        _ -> Nothing
  where
    extractSubst x t = do
      let xtSub = Map.fromList [(x, t)]
      result <- unify (applySubToCstrList xtSub tail)
      return (composeSub result xtSub)
unify ((CError) : tail) = Nothing

typing :: Expr -> Maybe Type
typing expr = do
  let (t, cstr) = inferExpr expr
  subs <- unify (toCstrList cstr)
  return (applySub subs t)

--  return TInt

type RelabelState a = State (Map.Map Int Int) a

relabel :: Type -> Type
relabel t = evalState (go t) Map.empty
  where
    go :: Type -> RelabelState Type
    go TInt = return TInt
    go TBool = return TBool
    go (TVar x) = do
      m <- get
      case Map.lookup x m of
        Just v -> return (TVar v)
        Nothing -> do
          let n = 1 + Map.size m
          put (Map.insert x n m)
          return (TVar n)
    go (TArr t1 t2) = do
      t1' <- go t1
      t2' <- go t2
      return (TArr t1' t2')

readExpr :: String -> Expr
readExpr = read

typeInfer :: Expr -> String
typeInfer expr = maybe "Type Error" (show . relabel) (typing expr)
