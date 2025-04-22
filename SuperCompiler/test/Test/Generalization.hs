module Test.Generalization where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Generalization as G
import qualified Syntax as S

-- Test case 1 

t1 :: S.Expr
t1 = S.Free "x"

t2 :: S.Expr
t2 = S.Free "x"

t3 :: S.Expr
t3 = S.Free "y"

test_var_gen :: Assertion
test_var_gen = do 
    (G.generalize t1 t2 (concatMap S.freeVars [t1, t2]) [] []) @?= (S.Free "x", [], [])
    (G.generalize t1 t3 (concatMap S.freeVars [t1, t3]) [] []) @?= (S.Free "v", [("v", t1)], [("v", t3)])

-- Test case 2

t1' :: S.Expr
t1' = S.Lam "x" (S.Bound 0)

t2' :: S.Expr
t2' = S.Lam "y" (S.Bound 0)

t3' :: S.Expr
t3' = S.Lam "z" (S.Free "x")

test_lam_gen :: Assertion
test_lam_gen = do 
    (G.generalize t1' t2' (concatMap S.freeVars [t1', t2']) [] []) @?= (S.Lam "x" (S.Bound 0), [], [])
    concatMap S.freeVars [t1', t3'] @?= ["x"]
    (G.generalize t1' t3' (concatMap S.freeVars [t1', t3']) [] []) @?= (S.Lam "x'" (S.Free "v"), [("v", S.Bound 0)], [("v", S.Free "x")])


-- Test case 3

t1'' :: S.Expr
t1'' = S.App (S.App (S.Fun "f") (S.Free "x")) (S.Free "y")

t2'' :: S.Expr
t2'' = S.App (S.App (S.Fun "f") (S.Free "z")) (S.Free "z")

t3'' :: S.Expr
t3'' = S.App (S.Fun "f") (S.Free "z")

test_app_gen :: Assertion
test_app_gen = do 
    (G.generalize t1'' t2'' (concatMap S.freeVars [t1'', t2'']) [] []) @?= (S.App (S.App (S.Fun "f") (S.Free "v")) (S.Free "v'"), [("v'", S.Free "y"), ("v", S.Free "x")], [("v'", S.Free "z"), ("v", S.Free "z")])
    (G.generalize t2'' t3'' (concatMap S.freeVars [t2'', t3'']) [] []) @?= (S.App (S.Free "v") (S.Free "z"), [("v", S.App (S.Fun "f") (S.Free "z"))], [("v", S.Fun "f")])

props :: [TestTree]
props = 
  [testCase "var" test_var_gen, 
   testCase "lam" test_lam_gen, 
   testCase "app" test_app_gen
  ]