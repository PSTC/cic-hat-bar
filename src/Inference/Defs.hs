module Inference.Defs
    ( lengthOfIndParams
    , lengthOfIndArgs
    , lengthOfConstrParams
    , lengthOfConstrArgs
    , typeInd
    , typeConstr
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
    let I _ _ sig _ = getInd i d
        (xts, _)    = flatten sig
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
typeInd i d = let I _ _ sig _ = getInd i d in sig

-- In the type T of the constructor C of inductive type I,
-- replace the stage of the arity of T with ŝ, and
-- replace the stage of all other instances of I with s
-- It's not entirely clear where I can occur,
-- or why occurrences of I within other inductive types need a stage,
-- e.g. in Tree (List A) -> List A, for instance
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

-- I /think/ this is the type of a case expression
-- applied to parameters ps but abstracted over the input inductive type's arguments
-- Since our case expressions contain its entire type, I think we just need to bind ps
-- then set the stage of the input inductive type to ŝ
typePred :: String -> Stage -> [Term Stage] -> Term Stage -> IndTypes -> Term Stage
typePred i s ps w d = Set -- TODO

-- I /think/ this is the type of a case branch
-- applied to parameters ps but abstracted over constructor arguments
-- Since our case branches are functions over parameters and arguments,
-- I think we just need to get the type of that function applied to ps
-- then set the stage of the inductive type to s
typeBranch :: String -> Stage -> Term Stage -> [Term Stage] -> Term Stage
typeBranch c s p ps = Set -- TODO
