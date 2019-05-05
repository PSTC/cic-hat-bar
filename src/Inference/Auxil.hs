module Inference.Auxil
    ( rules
    , whnf
    , (⪯)
    ) where

import Grammar.All

{- Auxiliary functions 
These are the auxiliary functions described in CIC^ Section 5 just after Definition 12
and used within the infer and check algorithms.
-}

-- Axioms state the types of the sorts in our universe, which by the Coq Reference Manual are
--      Prop:   Type 1
--      Set:    Type 1
--      Type j: Type (j + 1)
-- We begin at Type 1 (as in the Manual) rather than Type 0 (as in CIC^_)
-- This is implemented directly inside infer because we don't differentiate between sorts and terms

-- Rules state the type of products Πx: T. U depending on the types of T and U
-- which is described by Prod-Prop, Prod-Set, and Prod-Type in the Manual
rules :: Term Stage -> Term Stage -> Term Stage
rules _         Prop    = Prop
rules (Type i)  Set     = Type i
rules _         Set     = Set
rules (Type i) (Type j) = Type (max i j)
rules _        (Type j) = Type j

-- whnf "computes the weak head normal form of an expression" [CIC^]
-- which performs reductions from the outside in
-- until it reaches an unapplied abstraction/product or a constructor
-- fixpoints alone are not touched, but case expressions are reduced
whnf :: Term Stage -> Term Stage
whnf = id -- TODO

-- This is "an auxiliary function that generates constraints from subtyping judgements" [CIC^]
-- which could be computed using Lemma 2: "If A and A' are normalizing, then A ⪯ A' iff NF(A) ≤ NF(A')"
-- No further detail is given, but it looks like the algorithm will have to be constructed from Definition 6 (Subtyping)
(⪯) :: Term Stage -> Term Stage -> Constraints
_ ⪯ _ = [] -- TODO

-- Shift "replaces all stage annotations s in recursive positions by ŝ; in addition,
-- Shift returns the set V* of replaced variables." [CIC^]
shift :: Term Stage -> Term Position -> (StageVars, Term Stage)
shift u upos = (initStageVars, Set) -- TODO