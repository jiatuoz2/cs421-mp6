module Infer where

import Common

import Control.Monad.Writer (listen)
import Control.Monad.Except (throwError)
import Data.Map.Strict as H (Map, insert, lookup, empty, fromList, singleton, union)

  {- question 1: fresh instance function -}

freshInst :: PolyTy -> Infer MonoTy
freshInst (Forall qVars tau) = 
  let aux :: [VarId] -> Infer Substitution
      aux [] = return $ fromList []
      aux (v:vs) = 
        do monotype <- freshTau
           substs <- aux vs
           return $ insert v monotype substs
  in do subst <- aux qVars
        return $ apply subst tau

  {- question 2: occurs check -}

occurs :: VarId -> MonoTy -> Bool
occurs i tau = 
  let freevars = freeVars tau
  in elem i freevars

  {- question 3: unification -}

unify :: [Constraint] -> Infer Substitution
unify [] = return substEmpty
unify constraints =
  let (c:cs) = reverse constraints
  in do substs <- unify cs
        (tau1 :~: tau2) <- return $ apply substs c
        case tau1 == tau2 of
          True -> return $ substs
          False -> case (tau1, tau2) of 
                (TyConst _ _, TyVar t2) -> unify $ (tau2 :~: tau1):cs
                (TyConst c1 ss, TyConst c2 ts) | c1 == c2 && length ss == length ts -> unify $ (zipWith (:~:) ss ts) ++ cs
                                               | otherwise -> throwError $ Can'tMatch tau1 tau2
                (TyVar t1, _) | (occurs t1 tau2) -> throwError $ InfiniteType t1 tau2
                              | otherwise -> return $ substCompose (substInit t1 tau2) substs
              
  {- question 4: type inference -}

infer :: TypeEnv -> Exp -> Infer MonoTy
infer env exp = undefined

inferInit :: TypeEnv -> Exp -> Infer PolyTy
inferInit env e = do
  (tau, constraints) <- listen $ infer env e
  substitution <- unify constraints
  return $ quantifyMonoTy $ apply substitution tau

inferDec :: TypeEnv -> Dec -> Infer (TypeEnv, PolyTy)
inferDec env (AnonDec e') = do
  tau <- inferInit env e'
  return (env, tau)
inferDec env (LetDec x e') = do
  tau <- inferInit env (LetExp x e' (VarExp x))
  return (H.insert x tau env, tau)
inferDec env (LetRec f x e') = do
  tau <- inferInit env (LetRecExp f x e' (VarExp f))
  return (H.insert f tau env, tau)