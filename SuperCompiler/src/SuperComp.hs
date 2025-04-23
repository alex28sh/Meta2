module SuperComp where

import Data.Maybe (fromJust, isJust)
import Data.List (find)
import Generalization
import Residual
import Syntax
import Debug

-- data Context = EmptyCtx
--              | ApplyCtx Context Expr
--              | CaseCtx Context [(String,[String],Expr)]

superComp :: Program -> Program
superComp (Program expr decls) = 
    case superHelper expr EmptyCtx [] decls [] [] of
        Nothing -> error "SuperComp failed"
        Just e -> 
            let (e_res, decls_res) = residualize (trace (show e) e) in Program e_res decls_res

superHelper :: Expr -> Context -> [String] -> [Decl] -> [(Expr, Expr)] -> [Expr] -> Maybe Expr 
superHelper (Free x) ctx fv decls m e =            -- Free begin 
    superHelper' ctx (Free x) fv decls m e 
superHelper (Con c ts) EmptyCtx fv decls m e = do  -- Constructor begin
    ts' <- superArgs ts fv decls m e 
    return $ Con c ts'
superHelper (Con c ts) (ApplyCtx k u) fv decls m e = error "Constructor cannot be applied in this context"
superHelper (Con c ts) (CaseCtx k bs) fv decls m e = do 
    case find (\(PCon c' xs, _) -> c == c' && length xs == length ts) bs of 
        Nothing -> error "Constructor not found in case branches"
        Just (PCon c' xs, b) -> 
            superHelper (foldr subst b ts) k fv decls m e -- Constructor end
superHelper (Lam x t) EmptyCtx fv decls m e = do  -- Lambda begin
    let x' = renameVar fv x
    t' <- superHelper (concrete x' t) EmptyCtx (x' : fv) decls m e
    return $ Lam x' $ abstract t' x'
superHelper (Lam x t) (ApplyCtx k u) fv decls m e = superHelper (subst u t) k fv decls m e
superHelper (Lam x t) (CaseCtx k bs) fv decls m e = error "Lambda cannot be applied in this context" -- Lambda end
superHelper (Case t bs) ctx fv decls m e = superHelper t (CaseCtx ctx bs) fv decls m e -- Case begin
superHelper (App t u) ctx fv decls m e = superHelper t (ApplyCtx ctx u) fv decls m e -- Application begin
superHelper (Fun f) ctx fv decls m e = do         -- Function begin
    let t = place (Fun f) (trace (show ctx) ctx)
    case [rename (fromJust r) u | u@(Unfold _ t' _) <- e, let r = renaming t' t, isJust r] of
        (u : _) -> return u
        [] -> 
            let rs = fold t fv decls m e (Fun f) ctx in trace ("Fold: " ++ show rs) rs                 -- Function end

superArgs :: [Expr] -> [String] -> [Decl] -> [(Expr, Expr)] -> [Expr] -> Maybe [Expr]
superArgs [] fv m d e = return []
superArgs (t:ts) fv m d e = do
    t' <- superHelper t EmptyCtx fv m d e
    ts' <- superArgs ts fv m d (unfolds t'++ e)
    return (t':ts')

superBranches :: [(Pat, Expr)] -> Context -> [String] -> [Decl] -> [(Expr, Expr)] -> [Expr] -> Maybe [(Pat, Expr)]
superBranches [] _ _ _ _ _ = return []
superBranches ((PCon c xs, b):bs) ctx fv decls m e = do 
    let xs' = renameVars fv xs
    b' <- superHelper (foldr concrete b xs') ctx (xs' ++ fv) decls m e
    bs' <- superBranches bs ctx fv decls m (unfolds b' ++ e)
    return $ (PCon c xs, foldl abstract b' xs') : bs'

superHelper' :: Context -> Expr -> [String] -> [Decl] -> [(Expr, Expr)] -> [Expr] -> Maybe Expr
superHelper' EmptyCtx t fv decls m e = return t
superHelper' (ApplyCtx k e') t fv decls m e = do 
    e'' <- superHelper (trace ("Applying: " ++ show (e', t)) e') EmptyCtx fv decls m e
    superHelper' k (App t e'') fv decls m (unfolds e'' ++ e)
superHelper' (CaseCtx k bs) (Free x) fv decls m e = 
    let bs' = map (\(PCon c xs, t) -> (PCon c xs, instantiate [(x, foldl abstract (Con c (map Free xs)) xs)] t)) bs
    in Case (Free x) <$> superBranches bs' k fv decls m e
superHelper' (CaseCtx k bs) t fv decls m e = Case t <$> superBranches bs k fv decls m e


superLet :: [(String, Expr)] -> Expr -> [String] -> [Decl] -> [(Expr, Expr)] -> [Expr] -> Maybe Expr
superLet [] t fv m d e = return t
superLet ((x, t) : s') u fv m d e = do
    t'' <- superHelper t EmptyCtx fv m d e
    u' <- superLet s' u (x : fv) m d (unfolds t'' ++ e)
    return $ Let x t'' (abstract u' x)

fold :: Expr -> [String] -> [Decl] -> [(Expr, Expr)] -> [Expr] -> Expr -> Context -> Maybe Expr
fold t fv decls m e fun ctx = do
    case [(u, t') | (u, t') <- m, couple t' t] of
        ((u, t') : _) -> do 
            let (u', s1, s2) = generalize t (trace ("Found: " ++ show (u, t') ++ show t) t') fv [] []
            rsLet <- superLet s1 (Fold u') fv decls m e
            return $ trace ("Fold: " ++ show rsLet) rsLet
        _ -> do 
            let f = trace "Didn't find" $ renameVar (fv ++ [f | (Unfold t _ _) <- e, let Fun f = redex t]) "f"
            let xs = freeVars t
            let u = foldl (\t x -> App t (Free x)) (Fun f) xs
            inner <- superHelper (unfold fun decls) ctx (xs ++ f : fv) decls ((u, t) : m) e
            return $ Unfold u t inner