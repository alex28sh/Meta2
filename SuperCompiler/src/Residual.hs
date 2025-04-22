module Residual where

import Data.Foldable (foldrM)
import Control.Monad.State
import Syntax

residualize :: Expr -> (Expr, [Decl])
residualize e = runState (residualize' e (freeVars e)) []

residualize' :: Expr -> [String] -> State [Decl] Expr
residualize' (Free x) fv = return $ Free x
residualize' (Bound i) fv = return $ Bound i
residualize' (Lam x t) fv = do 
    t' <- residualize' (concrete x t) (x : fv)
    return $ Lam x $ abstract t' x
residualize' (Con c ts) fv = do 
    ts' <- foldrM (\t ts -> do 
        t' <- residualize' t fv 
        return $ t' : ts) [] ts
    return $ Con c ts'
residualize' (App t u) fv = do 
    t' <- residualize' t fv
    u' <- residualize' u fv
    return $ App t' u'
residualize' (Fun f) fv = return $ Fun f
residualize' (Case t bs) fv = do
    t' <- residualize' t fv
    bs' <- foldrM (\(PCon name xs, b) bs -> do 
            let xs'' = renameVars fv xs
            b' <- residualize' (foldr concrete b xs'') (xs'' ++ fv)
            return $ (PCon name xs, (foldl abstract b' xs'')) : bs) [] bs
    return $ Case t' bs'
residualize' (Let x t u) fv = do 
    t' <- residualize' t fv
    let x' = renameVar fv x
    u' <- residualize' (concrete x' u) (x' : fv)
    return $ subst t' (abstract u' x')
residualize' (Unfold t u v) fv = do 
    let (Fun f) = redex t 
    d <- get 
    if f `elem` (map (\(Decl name _ _) -> name) d)
    then return t
    else do 
        t' <- residualize' v (f : fv)
        let xs = freeVars t
        modify ((Decl f xs $ foldl abstract t' xs) :)
        return t 
residualize' (Fold t) fv = residualize' t fv