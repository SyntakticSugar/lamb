{-
Module      : Compute
Description : Proc for beta reduction.
Maintainer  : rhsculling@pm.com
Stability   : experimental

    This module contains the code used for 
    computing on lambda terms. 

    Of course, computations may never normalise. 
    User discretion is advised. For example: 

    ghci> compute (App omega omega)

    You'll be waiting awhile!

    (!) At this stage there is no care taken 
        to avoid variable clashes. Next step 
        is to implement alpha reduction so 
        as to ensure no variables clash prior 
        to beginning computation. 

-}

module Beta where

    import Lambda 
    import Church

    substitute :: LambdaTerm -> String -> LambdaTerm -> LambdaTerm
    substitute (Var x) v t      = if (x == v)
                                    then t
                                    else (Var x)
    substitute (App x y) v t    = App (substitute x v t)
                                   (substitute y v t)  
    substitute (Abs x b) v t    = Abs x (substitute b v t)                       
   
    betaReduction :: LambdaTerm -> LambdaTerm
    betaReduction (App (Abs x b) t) = substitute b x t
    betaReduction (Var x)           = Var x
    betaReduction (App x y)         = App (betaReduction x)
                                          (betaReduction y)
    betaReduction (Abs x b)         = Abs x (betaReduction b)

    normalized :: LambdaTerm -> Bool
    normalized (App (Abs x b) t) = False
    normalized (Var x)           = True
    normalized (App x y)         = (normalized x) && (normalized y)
    normalized (Abs x b)         = (normalized b)
    
    compute :: LambdaTerm -> LambdaTerm
    compute t
        | normalized t      = t
        | otherwise         = compute (betaReduction t)