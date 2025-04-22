module Generalization where

import Data.Foldable (foldrM)
import Syntax

matchCase :: [(Pat, Expr)] -> [(Pat, Expr)] -> Bool
matchCase bs bs' = length bs == length bs' && all (\((PCon c xs, t),(PCon c' xs', t')) -> c == c' && length xs == length xs') (zip bs bs')

embed :: Expr -> Expr -> Bool
embed t u = couple t u || dive t u
 
couple :: Expr -> Expr -> Bool
couple (Free x) (Free x') = True
couple (Bound i) (Bound i') = i == i' 
couple (Lam _ t) (Lam _ t') = couple t t'
couple (Con c ts) (Con c' ts') | c == c' = all (uncurry embed) (zip ts ts')
couple (App t u) (App t' u') = couple t t' && embed u u'
couple (Fun f) (Fun f') = f == f'
couple (Case t bs) (Case t' bs') | matchCase bs bs' = embed t t' && all (\((PCon c xs, t),(PCon c' xs', t')) -> embed t t') (zip bs bs')
couple t t' = False

dive :: Expr -> Expr -> Bool
dive t (Lam _ t') = embed (shift 1 t) t'
dive t (Con c ts) = any (embed t) ts
dive t (App t' u) = embed t t' || embed t u
dive t (Case t' bs) = embed t t' || any (\(PCon c xs, t') -> embed (shift (length xs) t) t') bs
dive t t' = False

generalize :: Expr -> Expr -> [String] -> Subst -> Subst -> (Expr, Subst, Subst)
generalize (Free x) (Free x') fv s1 s2 | x == x' = (Free x, s1, s2)
generalize (Fun f) (Fun f') fv s1 s2 | f == f' = (Fun f, s1, s2)
generalize (Con c ts) (Con c' ts') fv s1 s2 | c == c' && length ts == length ts' = 
    let (ts'', s1', s2') = foldl (\(ts, s1, s2) (t, t') -> 
            let (t'', s1'', s2'') = generalize t t' fv s1 s2 in 
                (t'' : ts, s1'', s2'')) ([], s1, s2) (zip ts ts') 
    in (Con c ts'', s1', s2')
generalize (App t u) (App t' u') fv s1 s2 =
    let (t'', s1', s2') = generalize t t' fv s1 s2
        (u'', s1'', s2'') = generalize u u' fv s1' s2'
    in (App t'' u'', s1'', s2'')
generalize (Lam x t) (Lam x' t') fv s1 s2 =
    let x'' = renameVar (fv ++ map fst s1) x 
        (t'', s1', s2') = generalize (concrete x'' t) (concrete x'' t') (x'' : fv) s1 s2  
    in (Lam x'' $ abstract t'' x'', s1', s2')
generalize (Case t bs) (Case t' bs') fv s1 s2 | matchCase bs bs' =
    let (t'', s1', s2') = generalize t t' fv s1 s2  
        (bs'', s1'', s2'') = foldl (\(cur, s1, s2) ((PCon name xs, b), (PCon name' xs', b')) -> 
            let xs'' = renameVars (fv ++ map fst s1) xs
                (t'', s1'', s2'') = generalize (foldr concrete b xs'') (foldr concrete b' xs'') (xs'' ++ fv) s1 s2 
            in ((PCon name xs'', foldl abstract t'' xs) : bs'', s1'', s2'')) ([], s1, s2) (zip bs bs') 
    in (Case t'' bs'', s1'', s2'')
generalize t u fv s1 s2 = 
    case [x | (x, t') <- s1, (x', u') <- s2, x == x', t == t', u == u'] of
        (x:_)  -> (Free x, s1, s2)
        [] ->
            let v = renameVar (fv ++ map fst s1) "v" 
            in (Free v, (v, t) : s1, (v, u) : s2)
            

rename :: [(String, String)] -> Expr -> Expr
rename r (Free x) = case lookup x r of
                       Just x'  -> Free x'
                       Nothing -> Free x
rename r (Bound i) = Bound i
rename r (Lam x t) = Lam x (rename r t)
rename r (Con c ts) = Con c (map (rename r) ts)
rename r (App t u) = App (rename r t) (rename r u)
rename r (Fun f) = Fun f
rename r (Case t bs) = Case (rename r t) (map (\(PCon c xs, t) -> (PCon c xs, rename r t)) bs)
rename r (Let x t u) = Let x (rename r t) (rename r u)
rename r (Unfold t u v) = Unfold (rename r t) (rename r u) (rename r v)
rename r (Fold t) = Fold (rename r t)

renaming :: Expr -> Expr -> Maybe [(String, String)]
renaming t u = renaming' t u []

renaming' :: Expr -> Expr -> [(String, String)] -> Maybe [(String, String)]
renaming' (Free x) (Free x') r = 
    if x `elem` map fst r
    then 
        if (x,x') `elem` r 
        then 
            Just r 
        else 
            Nothing
    else Just ((x, x') : r) 
renaming' (Bound i) (Bound i') r | i == i' = Just r
renaming' (Lam x t) (Lam x' t') r = renaming' t t' r
renaming' (Con c ts) (Con c' ts') r | c == c' = foldrM (\(t, t') r -> renaming' t t' r) r (zip ts ts')
renaming' (App t u) (App t' u') r = renaming' t t' r >>= renaming' u u'
renaming' (Fun f) (Fun f') r | f == f' = Just r
renaming' (Case t bs) (Case t' bs') r | matchCase bs bs' = renaming' t t' r >>= (\r -> foldrM (\((_, t), (_, t')) r -> renaming' t t' r) r (zip bs bs'))
renaming' t u r = Nothing