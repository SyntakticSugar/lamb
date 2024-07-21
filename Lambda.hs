{-
Module      : Lambda
Description : Basic type definition for untyped lambda calculus.
Maintainer  : rhsculling@pm.com
Stability   : experimental

This module defines a type for working with lambda expressions. 

Some example combinators are provided. 

LambdaTerm is also made into an instance of Show for printing.
-}

module Lambda where

    data LambdaTerm = 
        Var String
        | Abs String LambdaTerm 
        | App LambdaTerm LambdaTerm
        deriving Eq

    -- Example combinators. 
    omega = Abs "x" (App (Var "x") (Var "x"))

    i :: LambdaTerm
    i = Abs "x" (Var "x")

    k :: LambdaTerm
    k = Abs "u" (Abs "v" (Var "u"))

    s :: LambdaTerm
    s = Abs "a" 
        (Abs "b" 
            (Abs "c" 
            (App (App (Var "a") (Var "c"))
                    (App (Var "b") (Var "c")))))

    -- SKI(KIS) -> I [Correct!]
    testTerm :: LambdaTerm
    testTerm = App (App (App s k) i) 
                   (App (App k i) s)

    -- SII (SII) -> SII [Correct! Does not normalise]
    hangTerm :: LambdaTerm 
    hangTerm = App (App (App s i) i) 
                   (App (App s i) i)

    -- String representation. 
    printTerm :: LambdaTerm -> String
    printTerm (Var x)   = x
    printTerm (Abs x y) = "(lam " ++ x ++ ". " ++ (printTerm y) ++ ")"
    printTerm (App x y) = "(" ++ (printTerm x) ++ " " ++ (printTerm y) ++ ")" 

    instance Show LambdaTerm where
        show = printTerm