module Main (main) where

import Syntax
import SuperComp

examplePrg :: Program
examplePrg = Program 
    (App (Lam "x" (Bound 0)) (Con "C" [Free "y"])) 
        [Decl "f" ["x"] (App (Bound 0) (Con "C" [Free "y"])),
        Decl "g" ["x"] (App (Bound 0) (Con "C" [Free "y"]))]

examplePrg' :: Program
examplePrg' = Program 
    (App (Fun "f") (Lam "x" (Bound 0))) 
        [Decl "f" ["x"] (App (Bound 0) (Con "C" [Free "y"])),
        Decl "g" ["x"] (App (Bound 0) (Con "C" [Free "y"])),
        Decl "h" ["x"] (App (Bound 0) (Con "C" [Free "y"]))]

examplePrg'' :: Program
examplePrg'' = Program 
    (App (App (Fun "append") (App (App (Fun "append") (Con "Nil" [])) (Free "y"))) (Free "z"))
        [Decl "append" ["x", "y"] (Case (Bound 1) [
            (PCon "Nil" [], Bound 0),
            (PCon "Cons" ["x", "xs"], 
                Con "Cons" 
                    [Bound 1, 
                    App (App (Fun "append") (Bound 0)) (Bound 2)
                    ])
            ])
        ]
                          

examplePrg''' :: Program
examplePrg''' = Program 
    (App (App (Fun "append") (App (App (Fun "append") (Free "x")) (Free "y"))) (Free "z"))
        [Decl "append" ["x", "y"] (Case (Bound 1) [
            (PCon "Nil" [], Bound 0),
            (PCon "Cons" ["x", "xs"], 
                Con "Cons" 
                    [Bound 1, 
                    App (App (Fun "append") (Bound 0)) (Bound 2)
                    ])
            ])
        ]


examplePrg_ :: Program
examplePrg_ = Program 
    (App (App (Fun "append") (App (App (Fun "append") (Con "Cons" [Free "a", Con "Nil" []])) (Free "x"))) (Free "z"))
        [Decl "append" ["x", "y"] (Case (Bound 1) [ 
            (PCon "Nil" [], Bound 0),
            (PCon "Cons" ["x", "xs"], 
                Con "Cons" 
                    [Bound 1, 
                    App (App (Fun "append") (Bound 0)) (Bound 2)
                    ])
            ])
        ]

examplePrg'''' :: Program
examplePrg'''' = Program 
    (App (App (Fun "append") (App (App (Fun "append") (Free "x")) (Con "Cons" [Free "a", Con "Nil" []]))) (Free "z"))
        [Decl "append" ["x", "y"] (Case (Bound 1) [ 
            (PCon "Nil" [], Bound 0),
            (PCon "Cons" ["x", "xs"], 
                Con "Cons" 
                    [Bound 1, 
                    App (App (Fun "append") (Bound 0)) (Bound 2)
                    ])
            ])
        ]

nthOpt :: Program
nthOpt = Program 
    (App (App (Fun "nth") (Free "x")) (Con "Succ" [Con "Succ" [Con "Zero" []]]))
        [Decl "nth" ["x", "y"] (Case (Bound 0) [
            (PCon "Zero" [], 
                Case (Bound 1) [
                    (PCon "Cons" ["x", "xs"], Bound 1),
                    (PCon "Nil" [], Con "None" [])
                ]
            ),
            (PCon "Succ" ["n"], 
                Case (Bound 2) [
                    (PCon "Cons" ["x", "xs"], App (App (Fun "nth") (Bound 0)) (Bound 2)),
                    (PCon "Nil" [], Con "None" [])
                ]
            )
        ])
        ]

nthOpt' :: Program
nthOpt' = Program 
    (App (App (Fun "nth") (Con "Cons" [Free "a", Con "Cons" [Free "b", Con "Nil" []]])) (Free "x"))
        [Decl "nth" ["x", "y"] (Case (Bound 0) [
            (PCon "Zero" [], 
                Case (Bound 1) [
                    (PCon "Cons" ["x", "xs"], Bound 1),
                    (PCon "Nil" [], Con "None" [])
                ]
            ),
            (PCon "Succ" ["n"], 
                Case (Bound 2) [
                    (PCon "Cons" ["x", "xs"], App (App (Fun "nth") (Bound 0)) (Bound 2)),
                    (PCon "Nil" [], Con "None" [])
                ]
            )
        ])
        ]

main :: IO ()
main = do 
    print $ sumperComp examplePrg
    print $ sumperComp examplePrg'
    print $ sumperComp examplePrg''
    print $ sumperComp examplePrg'''
    -- print $ sumperComp examplePrg''''
    print $ sumperComp examplePrg_
    print $ sumperComp nthOpt
    print $ sumperComp nthOpt'
