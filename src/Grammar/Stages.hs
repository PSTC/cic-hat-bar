module Grammar.Stages
    ( Bare(..)
    , Position(..)
    , Stage(..)
    , StageVars
    , initStageVars, nextStage, incStageVars
    , Constraints, Constraint(..)
    ) where

import Data.IntSet (IntSet, empty, insert)

data Bare = Bare deriving Eq                            -- ε
data Position = Epsilon | Position deriving Eq          -- ε, *
data Stage = S String | Succ Stage | Infty deriving Eq  -- s, Ŝ, ∞

instance Show Bare where
    show Bare = "°"

instance Show Position where
    show Epsilon = "°"
    show Position = "*"

instance Show Stage where
    show (S x) = "^S" ++ x
    show (Succ (S x)) = "^Ŝ" ++ x
    show Infty = "∞"


-- For keeping track of used stage variables
-- This could use a State monad
type StageVars = (IntSet, Int)

initStageVars :: StageVars
initStageVars = (empty, 0)

nextStage :: StageVars -> Stage
nextStage (_, i) = S $ show i

incStageVars :: StageVars -> StageVars
incStageVars (sv, i) = (insert i sv, i + 1)


-- For collecting constraints between stages
type Constraints = [Constraint]
data Constraint = Constraint Stage Stage deriving Show  -- s ⊑ r
