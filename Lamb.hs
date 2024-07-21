{-
Module      : Lamb
Description : Lambda calculus computer
Maintainer  : rhsculling@pm.com
Stability   : experimental

This module computes beta reduction of untyped lambda terms. 
Using the IO monad, the user is able to see the steps in 
beta reduction. 
-}

module Lamb where
    
    import Lambda
    import Church
    import Alpha
    import Beta

    import System.IO

    printStepsHelper :: LambdaTerm -> IO String
    printStepsHelper t =
        do
            --putStrLn ("Current term: " ++ show t)
            let nextTerm = betaReduction t
            if (normalized t)
                then
                    do
                        putStrLn ("Normalisation complete.")
                        putStrLn ("Upto alpha equivalence this term normalises to: ")
                        return (show (unAlphaReduction t))
                else
                    do
                        --putStrLn ("=  " ++ show nextTerm)
                        printStepsHelper nextTerm
    
    printStepsToTerminal :: LambdaTerm -> IO ()
    printStepsToTerminal t = printStepsHelper t >>= putStrLn

    -- Example to be computed
    example :: LambdaTerm
    example = App suc (App (App add one) two)

    example' :: LambdaTerm
    example' = App neg (App (App disj tt) ff)

    main :: IO ()
    main =
        do
            let exampleTerm = (alphaReduction (App suc mulOfAdd))
            putStrLn ("Normalising the following lambda term: \n " ++ (show exampleTerm))
            printStepsToTerminal exampleTerm