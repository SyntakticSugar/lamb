{-
Module      : Alpha
Description : Implement alpha reduction.
Maintainer  : rhsculling@pm.com
Stability   : experimental

This module implements alpha reduction. 
Given a LambdaTerm to normalise, this module 
first changes all bound variables to unique 
integers. After computation, it returns the 
bound variables to a more familiar set of 
variables: x y z etc. 

This does assume at most 10 bound variables 
in the normalised expression.
-}

module Alpha where

    import Lambda

    -- Variable rename, towards alpha reduction.
    renameVar :: LambdaTerm -> String -> String -> LambdaTerm
    renameVar (App x y) old new = App (renameVar x old new) (renameVar y old new)
    renameVar (Abs x b) old new = if (x == old)
                                    then Abs new (renameVar b old new)
                                    else Abs x (renameVar b old new)
    renameVar (Var x) old new   = if (x == old)
                                    then Var new
                                    else Var x    
    
    alphaHelper :: LambdaTerm -> [Int] -> LambdaTerm
    alphaHelper (Abs x b) vars   = Abs  (show (head vars)) 
                                        (alphaHelper    (renameVar b x (show(head vars))) 
                                                        (tail vars))
    alphaHelper (App x y) vars   = App  (alphaHelper x evenVars)
                                        (alphaHelper y oddVars)
        where
            evenVars = [x | (x, i) <- zip vars [0..], even i]
            oddVars  = [x | (x, i) <- zip vars [0..], odd i]
    alphaHelper (Var x) vars     = Var x
        
    
    alphaReduction :: LambdaTerm -> LambdaTerm
    alphaReduction t = alphaHelper t variables
        where
            variables = [1,2 ..]    

    unalphaHelper :: LambdaTerm -> [String] -> LambdaTerm
    unalphaHelper (Abs x b) vars   = Abs  (head vars)
                                        (unalphaHelper    (renameVar b x (head vars)) 
                                                          (tail vars))
    unalphaHelper (App x y) vars   = App  (unalphaHelper x evenVars)
                                          (unalphaHelper y oddVars)
        where
            evenVars = [x | (x, i) <- zip vars [0..], even i]
            oddVars  = [x | (x, i) <- zip vars [0..], odd i]
    unalphaHelper (Var x) vars     = Var x

    unAlphaReduction :: LambdaTerm -> LambdaTerm 
    unAlphaReduction t = unalphaHelper t prettyVars
        where
            prettyVars = ["x","y","z","u","v","p","q","r","a","b","c"]        
            