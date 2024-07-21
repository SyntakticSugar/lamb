{-
Module      : Church
Description : Church encoded lambda terms
Maintainer  : rhsculling@pm.com
Stability   : experimental

        This module stores examples of LambdaTerms. 
        Examples come from Church encodings of 
        Booleans, Numerals, and Arithmetic. 

        (!) To be added: 
                [ ] :: Y Combinator
-}

module Church where

    import Lambda

    -- Church Booleans
    tt :: LambdaTerm
    tt = Abs "xt"
         (Abs "yt" (Var "xt"))

    ff :: LambdaTerm
    ff = Abs "xf"
                (Abs "yf" (Var "yf"))

    cond :: LambdaTerm 
    cond = Abs "cc"
            (Abs "ct"
                    (Abs "cf"
                            (App (App (Var "cc") (Var "ct"))
                                    (Var "cf"))))

    neg :: LambdaTerm
    neg = Abs "Bool" (App ff tt)

    conj :: LambdaTerm
    conj = Abs "pc"
            (Abs "qc"
            (App (App (Var "pc") (Var "qc")) (Var "pc")))

    disj :: LambdaTerm
    disj = Abs "pd"
            (Abs "qd"
            (App (App (Var "pd") (Var "pd")) (Var "qd")))                

    -- Church Arithmetic
    zero :: LambdaTerm
    zero = Abs "f0"
            (Abs "x0" (Var "x0"))

    one :: LambdaTerm 
    one = Abs "f1"
            (Abs "x1" 
            (App (Var "f1") (Var "x1")))

    two :: LambdaTerm 
    two = Abs "f2"
            (Abs "x2" 
            (App (Var "f2") (App (Var "f2") (Var "x2"))))

    suc :: LambdaTerm
    suc = Abs "n"
            (Abs "u"
            (Abs "v"
                   (App (Var "u")
                           (App (App (Var "n") 
                                   (Var "u"))
                           (Var "v")))))

    add :: LambdaTerm
    add = Abs "m"
            (Abs "n"
            (Abs "u"
                    (Abs "v" (App (App (Var "m") (Var "u"))
                                    (App (App (Var "n") (Var "u"))
                                        (Var "v"))))))

    twoPlusTwo :: LambdaTerm
    twoPlusTwo = App (App add two) two

    mul :: LambdaTerm
    mul = Abs "m"
                (Abs "n"
                        (Abs "u"
                                (Abs "v"
                                        (App (App (Var "m") (App (Var "n") (Var "u")))
                                                (Var "v")))))

    mulOfAdd :: LambdaTerm
    mulOfAdd = App (App mul twoPlusTwo) twoPlusTwo
