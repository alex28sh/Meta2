module Syntax where 

import Data.List (find, nub)

data Expr = 
    Free String
    | Bound Int  
    | Con String [Expr]
    | Fun String
    | App Expr Expr
    | Lam String Expr
    | Case Expr [(Pat, Expr)] 
    | Let String Expr Expr 
    | Unfold Expr Expr Expr -- unfolding 
    | Fold Expr -- replacement node 
    deriving (Show, Eq, Read, Ord)

data Pat = PCon String [String]
    deriving (Show, Eq, Read, Ord)

data Decl = Decl String [String] Expr
    deriving (Show, Eq, Read, Ord)

data Program = Program Expr [Decl]
    deriving (Show, Eq, Read, Ord)

data Context = EmptyCtx
             | ApplyCtx Context Expr
             | CaseCtx Context [(Pat,Expr)] deriving Show

type Subst = [(String, Expr)]

shift :: Int -> Expr -> Expr
shift = shift' 0

shift' :: Int -> Int -> Expr -> Expr
shift' d 0 u = u
shift' d i (Free x) = Free x
shift' d i (Bound i') = if i' >= d then Bound (i'+i) else Bound i'
shift' d i (Lam x t) = Lam x (shift' (d+1) i t)
shift' d i (Con c ts) = Con c (map (shift' d i) ts)
shift' d i (App t u) = App (shift' d i t) (shift' d i u)
shift' d i (Fun f) = Fun f
shift' d i (Case t bs) = Case (shift' d i t) (map (\(p@(PCon _ xs), b) -> (p,shift' (d+length xs) i b)) bs)
shift' d i (Let x t u) = Let x (shift' d i t) (shift' (d+1) i u)
shift' d i (Unfold t u v) = Unfold (shift' d i t) (shift' d i u) (shift' d i v)
shift' d i (Fold t) = Fold (shift' d i t)


renameVar :: [String] -> String -> String
renameVar fv x | x `elem` fv = renameVar fv (x++"'") 
               | otherwise = x

renameVars :: [String] -> [String] -> [String]
renameVars fv vars = 
    let (_, nw) = foldr (\v (fv, cur) -> let v' = renameVar fv v in (v' : fv, v' : cur)) (fv, []) vars 
    in nw 

concrete :: String -> Expr -> Expr
concrete = concrete_helper 0 
    where 
        concrete_helper :: Int -> String -> Expr -> Expr
        concrete_helper _ _ (Free x) = Free x
        concrete_helper d x (Bound i) 
            | i == d = Free x 
            | i > d = Bound (i - 1)
            | otherwise = Bound i
        concrete_helper d x (Lam x' t) = Lam x' (concrete_helper (d+1) x t)
        concrete_helper d x (Con c ts) = Con c (map (concrete_helper d x) ts)
        concrete_helper d x (App t u) = App (concrete_helper d x t) (concrete_helper d x u)
        concrete_helper d x (Fun f) = Fun f
        concrete_helper d x (Case t bs) = Case (concrete_helper d x t) (map (\(p@(PCon _ xs), b) -> (p,concrete_helper (d+length xs) x b)) bs)
        concrete_helper d x (Let y t u) = Let y (concrete_helper d x t) (concrete_helper (d+1) x u)
        concrete_helper d x (Unfold t u v) = Unfold (concrete_helper d x t) (concrete_helper d x u) (concrete_helper d x v)
        concrete_helper d x (Fold t) = Fold (concrete_helper d x t)

abstract :: Expr -> String -> Expr
abstract e d = abstract_helper 0 d e 
    where 
        abstract_helper :: Int -> String -> Expr -> Expr
        abstract_helper d x (Free x') | x == x' = Bound d
                                      | otherwise = Free x'
        abstract_helper d x (Bound i) 
            | i >= d = Bound (i + 1)
            | otherwise = Bound i 
        abstract_helper d x (Lam x' t) = Lam x' (abstract_helper (d+1) x t)
        abstract_helper d x (Con c ts) = Con c (map (abstract_helper d x) ts)
        abstract_helper d x (App t u) = App (abstract_helper d x t) (abstract_helper d x u)
        abstract_helper d x (Fun f) = Fun f
        abstract_helper d x (Case t bs) = Case (abstract_helper d x t) (map (\(p@(PCon _ xs), b) -> (p,abstract_helper (d+length xs) x b)) bs)
        abstract_helper d x (Let y t u) = Let y (abstract_helper d x t) (abstract_helper (d+1) x u)
        abstract_helper d x (Unfold t u v) = Unfold (abstract_helper d x t) (abstract_helper d x u) (abstract_helper d x v)
        abstract_helper d x (Fold t) = Fold (abstract_helper d x t)

freeVars :: Expr -> [String]
freeVars = nub . freeVars'

freeVars' :: Expr -> [String]
freeVars' (Free x) = [x]
freeVars' (Bound _) = []
freeVars' (Con _ ts) = concatMap freeVars' ts
freeVars' (Fun _) = []
freeVars' (App t u) = freeVars' t ++ freeVars' u
freeVars' (Lam x t) = freeVars' t
freeVars' (Case t bs) = freeVars' t ++ concatMap (\(PCon _ xs, b) -> freeVars' b) bs
freeVars' (Let x t u) = freeVars' t ++ freeVars' u
freeVars' (Unfold t u v) = freeVars' v
freeVars' (Fold t) = freeVars' t


subst :: Expr -> Expr -> Expr 
subst = subst' 0 

subst' :: Int -> Expr -> Expr -> Expr
subst' d t (Free x) = Free x
subst' d t (Bound i) 
    | i == d = shift i t 
    | i < d = Bound i
    | otherwise = Bound (i - 1)
subst' d t (Lam x u) = Lam x (subst' (d+1) t u)
subst' d t (Con c ts) = Con c (map (subst' d t) ts)
subst' d t (App u v) = App (subst' d t u) (subst' d t v)
subst' d t (Fun f) = Fun f
subst' d t (Case u bs) = Case (subst' d t u) (map (\(p@(PCon _ xs), b) -> (p, subst' (d+length xs) t b)) bs)
subst' d t (Let x u v) = Let x (subst' d t u) (subst' (d+1) t v)
subst' d t (Unfold u v w) = Unfold (subst' d t u) (subst' d t v) (subst' d t w)
subst' d t (Fold u) = Fold (subst' d t u)

redex :: Expr -> Expr 
redex (App l _) = redex l 
redex (Case t _) = redex t
redex e = e 

place :: Expr -> Context -> Expr
place t EmptyCtx = t
place t (ApplyCtx con u) = place (App t u) con
place t (CaseCtx con bs) = place (Case t bs) con

unfolds :: Expr -> [Expr]
unfolds t@(Unfold _ _ u) = t : unfolds u
unfolds (Fold _) = []
unfolds (Free _) = []
unfolds (Bound _) = []
unfolds (Con _ ts) = concatMap unfolds ts
unfolds (Fun _) = []
unfolds (App t u) = unfolds t ++ unfolds u
unfolds (Lam x t) = let x' = renameVar (freeVars t) x
                       in  unfolds (concrete x' t)
unfolds (Case t bs) = unfolds t ++ concatMap (\(PCon _ xs, b) -> 
    let xs' = renameVars (freeVars t) xs
        in  unfolds (foldr concrete b xs')) bs
unfolds (Let x t u) = let x' = renameVar (freeVars u) x
                      in  unfolds t  ++ unfolds (concrete x' u)


instantiate :: Subst -> Expr -> Expr
instantiate = instantiate' 0

instantiate' :: Int -> Subst -> Expr -> Expr
instantiate' d s (Free x) = case lookup x s of
    Just t  -> shift d t
    Nothing -> Free x
instantiate' d s (Bound i) = Bound i
instantiate' d s (Con c ts) = Con c (map (instantiate' d s) ts)
instantiate' d s (Fun f) = Fun f
instantiate' d s (App t u) = App (instantiate' d s t) (instantiate' d s u)
instantiate' d s (Lam x t) = Lam x (instantiate' (d+1) s t)
instantiate' d s (Case t bs) = Case (instantiate' d s t) (map (\(p@(PCon _ xs), b) -> (p, instantiate' (d+length xs) s b)) bs)
instantiate' d s (Let x t u) = Let x (instantiate' d s t) (instantiate' (d+1) s u)
instantiate' d s (Unfold t u v) = Unfold (instantiate' d s t) (instantiate' d s u) (instantiate' d s v)
instantiate' d s (Fold t) = Fold (instantiate' d s t)


unfold :: Expr -> [Decl] -> Expr
unfold (App t u) d = App (unfold t d) u
unfold (Case t bs) d = Case (unfold t d) bs
unfold (Fun f) d = 
    case find (\(Decl name _ _) -> name == f) d of
        Just (Decl _ xs t) -> foldr Lam t xs
        Nothing -> Fun f
unfold t d = t
