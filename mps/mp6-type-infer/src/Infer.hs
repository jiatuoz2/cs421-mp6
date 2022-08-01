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
infer env (ConstExp c) = freshInst $ constTySig c

infer env (VarExp s) = case H.lookup s env of
  Nothing -> throwError $ LookupError s
  Just a -> freshInst a

infer env (LetExp x e1 e2) = do 
  (tau1, constraints) <- listen $ infer env e1
  subst <- unify constraints
  tau <- return $ apply subst tau1
  tau2 <- infer (insert x (gen env tau) env) e2
  return $ tau2

infer env (BinOpExp binop e1 e2) = do
  (tau1, c1) <- listen $ infer env e1
  (tau2, c2) <- listen $ infer env e2
  tau <- freshInst $ binopTySig binop
  ntau <- freshTau
  constrain (funTy tau1 (funTy tau2 ntau)) tau
  subst <- unify ([(funTy tau1 (funTy tau2 ntau)) :~: tau] ++ c1 ++ c2)
  return $ apply subst ntau

infer env (MonOpExp monop e) = do
  (tau1, c1) <- listen $ infer env e
  tau <- freshInst $ monopTySig monop
  ntau <- freshTau
  constrain (funTy tau1 ntau) tau
  subst <- unify ([(funTy tau1 ntau) :~: tau] ++ c1)
  return $ apply subst ntau

infer env (IfExp e1 e2 e3) = do
  (tau1, c1) <- listen $ infer env e1
  (tau2, c2) <- listen $ infer env e2
  (tau3, c3) <- listen $ infer env e3
  constrain tau1 boolTy
  constrain tau2 tau3
  subst <- unify ([tau1 :~: boolTy, tau2 :~: tau3] ++ c1 ++ c2 ++ c3)
  return $ apply subst tau2 

infer env (FunExp v e) = do
  tau1 <- freshTau
  (tau2, c) <- listen $ infer (insert v (Forall [] tau1) env) e
  subst <- unify c
  return $ funTy (apply subst tau1) tau2

infer env (AppExp f a) = do
  (tau1, c1) <- listen $ infer env f
  (tau2, c2) <- listen $ infer env a
  tau <- freshTau
  constrain tau1 (funTy tau2 tau)
  subst <- unify ([tau1 :~: (funTy tau2 tau)] ++ c1 ++ c2)
  return $ apply subst tau

infer env (LetRecExp f x e1 e) = do
  tau1 <- freshTau
  tau2 <- freshTau
  (tau3, c1) <- listen $ infer ((fromList [(f, Forall [] (funTy tau1 tau2)), (x, Forall [] tau1)]) `H.union` env) e1
  constrain tau2 tau3
  subst1 <- unify ((tau2 :~: tau3):c1)
  ntau <- return $ apply subst1 (funTy tau1 tau2)
  (tau, c2) <- listen $ infer (insert f (gen env ntau) env) e
  subst2 <- unify (c2 ++ c1)
  return $ apply subst2 tau



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