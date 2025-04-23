module Main (main) where

import System.Environment (getArgs)
import Syntax
import SuperComp
import Debug

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

maxmin' :: Program
maxmin' = Program 
    (App (Fun "maxmin") (Con "Cons" [Con "Zero" [], Con "Cons" [Con "Succ" [Con "Succ" [Con "Zero" []]], Con "Cons" [Con "Succ" [Con "Zero" []], Con "Nil" []]]]))
    [
        Decl "max" ["x", "y"] (Case (Bound 0) [
            (PCon "Zero" [], Bound 1),
            (PCon "Succ" ["n"], 
                Case (Bound 1) [
                    (PCon "Zero" [], Bound 0),
                    (PCon "Succ" ["n"], Con "Succ" [App (App (Fun "max") (Bound 0)) (Bound 1)])
                ]
            )
        ]), 
        Decl "min" ["x", "y"] (Case (Bound 0) [
            (PCon "Zero" [], Bound 0),
            (PCon "Succ" ["n"], 
                Case (Bound 1) [
                    (PCon "Zero" [], Bound 0),
                    (PCon "Succ" ["n"], Con "Succ" [App (App (Fun "min") (Bound 0)) (Bound 1)])
                ]
            )
        ]), 
        Decl "maxmin" ["x"] (Case (Bound 0) [
            (PCon "Cons" ["x", "xs"], 
                Case (App (Fun "maxmin") (Bound 0)) [
                    (PCon "Pair" ["x", "y"], Con "Pair" [
                        App (App (Fun "max") (Bound 1)) (Bound 3),
                        App (App (Fun "min") (Bound 0)) (Bound 3)
                    ]),
                    (PCon "None" [], Con "Pair" [Bound 1, Bound 1])
                ]
            ), 
            (PCon "Nil" [], Con "None" [])
        ])
    ]

maxmin'' :: Program
maxmin'' = Program 
    (App (Fun "maxmin") (Free "x"))
    [
        Decl "max" ["x", "y"] (Case (Bound 0) [
            (PCon "Zero" [], Bound 1),
            (PCon "Succ" ["n"], 
                Case (Bound 1) [
                    (PCon "Zero" [], Bound 0),
                    (PCon "Succ" ["n"], Con "Succ" [App (App (Fun "max") (Bound 0)) (Bound 1)])
                ]
            )
        ]), 
        Decl "min" ["x", "y"] (Case (Bound 0) [
            (PCon "Zero" [], Bound 0),
            (PCon "Succ" ["n"], 
                Case (Bound 1) [
                    (PCon "Zero" [], Bound 0),
                    (PCon "Succ" ["n"], Con "Succ" [App (App (Fun "min") (Bound 0)) (Bound 1)])
                ]
            )
        ]), 
        Decl "maxmin" ["x"] (Case (Bound 0) [
            (PCon "Cons" ["x", "xs"], 
                Case (App (Fun "maxmin") (Bound 0)) [
                    (PCon "Pair" ["x", "y"], Con "Pair" [
                        App (App (Fun "max") (Bound 1)) (Bound 3),
                        App (App (Fun "min") (Bound 0)) (Bound 3)
                    ]),
                    (PCon "None" [], Con "Pair" [Bound 1, Bound 1])
                ]
            ), 
            (PCon "Nil" [], Con "None" [])
        ])
    ]

reverse' :: Program
reverse' = Program 
    (App (Fun "reverse") (Free "x"))
        [Decl "append" ["x", "y"] (Case (Bound 1) [ 
            (PCon "Nil" [], Bound 0),
            (PCon "Cons" ["x", "xs"], 
                Con "Cons" 
                    [Bound 1, 
                    App (App (Fun "append") (Bound 0)) (Bound 2)
                    ])
            ]), 
        Decl "reverse" ["x"] (Case (Bound 0) [
            (PCon "Nil" [], Bound 0),
            (PCon "Cons" ["x", "xs"], 
                App (App (Fun "append") (App (Fun "reverse") (Bound 0))) (Con "Cons" [Bound 1, Con "Nil" []])
            )
        ])
    ]


reverse'' :: Program
reverse'' = Program 
    (App (Fun "reverse") (Con "Cons" [Free "x", Con "Cons" [Free "y", Con "Nil" []]]))
        [Decl "append" ["x", "y"] (Case (Bound 1) [ 
            (PCon "Nil" [], Bound 0),
            (PCon "Cons" ["x", "xs"], 
                Con "Cons" 
                    [Bound 1, 
                    App (App (Fun "append") (Bound 0)) (Bound 2)
                    ])
            ]), 
        Decl "reverse" ["x"] (Case (Bound 0) [
            (PCon "Nil" [], Bound 0),
            (PCon "Cons" ["x", "xs"], 
                App (App (Fun "append") (App (Fun "reverse") (Bound 0))) (Con "Cons" [Bound 1, Con "Nil" []])
            )
        ])
    ]

main :: IO ()
main = do
    args <- getArgs
    let debug = "--debug" `elem` args
    setDebug debug
    if debug
        then putStrLn "Debug output enabled"
        else putStrLn "Debug output disabled"
    mapM_ (\(name, x) -> do
        putStrLn $ "\nTesting " ++ name ++ ":"
        print $ superComp x
        putStrLn "") 
        [ ("examplePrg", examplePrg)
        , ("examplePrg'", examplePrg')
        , ("examplePrg''", examplePrg'')
        , ("examplePrg'''", examplePrg''')
        , ("examplePrg''''", examplePrg'''')
        , ("examplePrg_", examplePrg_)
        , ("nthOpt", nthOpt)
        , ("nthOpt'", nthOpt')
        , ("maxmin'", maxmin')
        , ("maxmin''", maxmin'')
        , ("reverse'", reverse')
        , ("reverse''", reverse'')
        ]