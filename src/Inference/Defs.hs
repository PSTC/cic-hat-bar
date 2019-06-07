module Inference.Defs
    ( lengthOfIndParams, lengthOfIndArgs
    , lengthOfConstrParams, lengthOfConstrArgs
    , typeInd, typeConstr
    , typePred, typeBranch
    ) where

import Grammar.All

{- Definitions over inductive constructions 
These are the definitions described in both CIC^ Figure 2 
and CIC^_ Section 2.3.3 just after Typing rules for signatures. 
-}

-- The next four functions correspond to params(I), args(I) (missing), params(c), and args(c) of CIC^ Fig. 2
-- Parameters and arguments are stored as contexts in CIC^ and CIC^_,
-- but we follow the Coq Reference Manual's model of storing that information as inductive/constructor types
-- so we need to deconstruct the (product) types to count how many arguments there are

lengthOfIndParams :: String -> IndTypes -> Int
lengthOfIndParams i d = params $ getInd i d

lengthOfIndArgs :: String -> IndTypes -> Int
lengthOfIndArgs i d =
    let (xts, _)    = flatten $ typeInd i d
    in length xts - lengthOfIndParams i d

lengthOfConstrParams :: String -> IndTypes -> Int
lengthOfConstrParams c d = params $ getIndFromConstr c d

lengthOfConstrArgs :: String -> IndTypes -> Int
lengthOfConstrArgs c d =
    let Beta _ t = getConstr c d
        (xts, _) = flatten t
    in  length xts - lengthOfConstrParams c d

-- The type of the inductive type i if it were treated like a function
-- N.B. We use CIC^'s definition, which abstracts over parameters, and not CIC^_'s, which doesn't
typeInd :: String -> IndTypes -> Term Stage
typeInd i d = sig $ getInd i d

-- In the type T of the constructor C of inductive type I,
-- replace the stage of the arity of T with ŝ, and
-- replace the stage of all other instances of I with s
typeConstr :: String -> Stage -> IndTypes -> Term Stage
typeConstr c s d =
    let i = name $ getIndFromConstr c d
        t = getConstrType c d
    in  traverseType i s t
    where
        traverseType :: String -> Stage -> Term Stage -> Term Stage -- constructor type must be of the form Πx1: T1. ... Πxk: Tk. I^s ps as (CIC^_ Def. 2.11)
        traverseType i s (Prod x t e) = Prod x (setStage i s t) (traverseType i s e)
        traverseType i s (Ind _ _ ps as) = Ind i (Succ s) ps as

        setStage :: String -> Stage -> Term Stage -> Term Stage
        setStage i s (Ind i' Infty ps as) =                         -- inductive types must be annotated with ∞ (CIC^_ Sec. 2.3.3, Clause I5)
            let ps' = map (setStage i s) ps                         -- inductive type never appears in the arguments (CIC^_ Def. 2.10, bullets 2 and 4.1)
            in  if i == i' then Ind i s ps' as else Ind i' Infty ps' as
        setStage i s (Prod x t e) = Prod x t (setStage i s e)       -- inductive type never appears in t (CIC^_ Def. 2.10, bullet 3)
        setStage _ _ t = t                                          -- inductive type never appears elsewhere (CIC^_ Def. 2.10, bullet 1)

-- Given an inductive type name i, a stage s, a list of parameters ps,
-- a variable name x, a sort w, and a global context d,
-- construct a product with the arguments of i and x of type i with stage (succ s)
-- and with w as the body in which x in used
-- where the parameters of i are bound to ps
-- i.e. ΠΔas. Πx: (I ŝ ps as). w
typePred :: String -> Stage -> [Term Stage] -> String -> Term Stage -> IndTypes -> Term Stage
typePred i s ps x w d =
    let I _ n sig _ = getInd i d
        (xts, _) = flatten sig
        pdom = getIndParamDom i d
        as   = map Var $ getIndArgDom i d
        body = Prod x (Ind i (Succ s) ps as) w
        prod = unflatten (drop n xts, body)
    in  bindAll prod $ zip pdom ps

-- Given a constructor name c, a stage s, a list of parameters ps,
-- and a global context d,
-- construct a product with the arguments of c
-- and with p applied to the indices of the inductive type of c and applied to c as the body
-- where the parameters of c are bound to ps
-- i.e. ΠΔas. P is c
typeBranch :: String -> Stage -> Term Stage -> [Term Stage] -> IndTypes -> Term Stage
typeBranch c s p ps d =
    let n = lengthOfConstrParams c d
        sig = typeConstr c s d
        (xts, Ind _ _ _ is) = flatten sig
        pdom = getConstrParamDom c d
        as   = map Var $ getConstrArgDom c d
        body = apply p is [Constr c (map (fmap (const Bare)) ps) as]
        prod = unflatten (drop n xts, body)
    in  bindAll prod $ zip pdom ps
