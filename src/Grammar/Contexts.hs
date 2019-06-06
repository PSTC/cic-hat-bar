module Grammar.Contexts
    ( Context, Declaration(..)
    , IndTypes, Inductive(..)
    , getType
    , getInd, getIndFromConstr, getIndSort
    , getConstr, getConstrType, getConstrNames
    , getIndParamDom, getIndArgDom, getConstrParamDom, getConstrArgDom
    , getFreeVariable
    ) where

import Grammar.Terms
import Grammar.Stages
import Data.Maybe (fromJust)
import Data.List (find)

-- A local context (Γ) as defined in the chapter on CIC in the Coq Reference Manual
-- Declarations are called Beta and Zeta because they disappear after beta-/zeta-reduction
type Context = [Declaration]
data Declaration
    = Beta String (Term Stage)              -- (x : T[a])
    | Zeta String (Term Stage) (Term Stage) -- (x = T[a] : T[a])

-- A global context (Δ) as defined in the chapter on CIC in the Coq Reference Manual,
-- except there's only inductive definitions in them
type IndTypes = [Inductive]
data Inductive = I {
        -- The name I of the inductive type being defined
        -- N.B. Only one type is defined at a time, as opposed to the Manual,
        --      which defines mutually inductive types together
        name        :: String,
        -- The number of polymorphic parameters
        params      :: Int,
        -- The type signature of I if we consider it as a function
        -- N.B. The signature is an abstraction on both the parameters and the arguments of I
        signature   :: Term Stage,
        -- The type signatures of the names and constructors C of I
        -- These will ALWAYS be Betas (it doesn't make sense for them to be Zetas!)
        -- N.B. The signatures are abstractions on both the parameters and the arguments of C
        --      This is NOT the same as defined in the Manual, which excludes the parameters
        constrs     :: Context
    }   -- Ind[n](I: T[a] := Γ)


-- TODO: There needs to be a function that checks the validity of inductive definitions
-- based on the conditions I1 through I9 as described in CIC^_ (based on the seven clauses in CIC^ Sec. 4)
isValid :: Inductive -> Bool
isValid _ = True

{- Examples

The naturals are defined as
    Ind[0](Nat: Set := {
        O: Nat,
        S: Nat -> Nat
    })

Polymorphic lists are defined as
    Ind[1](List: Set -> Set := {
        Nil:  (A: Set) -> List A,
        Cons: (A: Set) -> A -> List A -> List A
    })

The finite sets are defined as
    Ind[0](Fin: Nat -> Set := {
        FZ: (k: Nat) -> Fin (S k),
        FS: (k: Nat) -> Fin k -> Fin (S k)
    })

Sized vectors are defined as
    Ind[1](Vec: Set -> Nat -> Set := {
        VNil:  (A: Set) -> Vec A O
        VCons: (A: Set) -> (n: Nat) -> A -> Vec A n -> Vec A (S n)
    })

Notice that the constructor types explicitly abstract over polymorphic parameters and dependent arguments.

-}

{- Extraction from contexts -}

-- Given a local context g and a variable name x, get the type of x in the context
getType :: String -> Context -> Term Stage
getType x g =
    case fromJust $ find (isBound x) g of
        Beta _   t -> t
        Zeta _ _ t -> t
    where
        isBound x (Beta x'   _) = x == x'
        isBound x (Zeta x' _ _) = x == x'

-- Given a global context and an inductive type name i, get the corresponding inductive definition
getInd :: String -> IndTypes -> Inductive
getInd i = fromJust . find (isInductive i)
    where isInductive i ind = i == name ind

-- Given a global context and a constructor name c, get the inductive definition it belongs to
getIndFromConstr :: String -> IndTypes -> Inductive
getIndFromConstr c = fromJust . find (isIndConstr c)
    where
        isIndConstr c ind = any (isConstr c) (constrs ind)
        isConstr c (Beta c' _) = c == c'
        isConstr _ _ = False

-- Given a global context d and an inductive type name i, get the sort of the fully-applied inductive type
getIndSort :: String -> IndTypes -> Term Stage
getIndSort i = snd . flatten . signature . getInd i

-- Given a global context d and a constructor name c, get the corresponding constructor declaration
getConstr :: String -> IndTypes -> Declaration
getConstr c d =
    let cs = constrs $ getIndFromConstr c d
    in  fromJust $ find (isConstr c) cs
    where
        isConstr c (Beta c' _) = c == c'
        isConstr _ _ = False

-- Given a global context d and a constructor name c, get the type of the corresponding constructor
getConstrType :: String -> IndTypes -> Term Stage
getConstrType c d =
    let Beta _ t = getConstr c d
    in  t

-- Given a global context d and an inductive type name i, get the names of the constructors of i
getConstrNames :: String -> IndTypes -> [String]
getConstrNames i d =
    let I _ _ _ cs = getInd i d
    in  map (\(Beta c _) -> c) cs

-- Given a global context d and an inductive type name i, get the parameter names (i.e. its domain)
getIndParamDom :: String -> IndTypes -> [String]
getIndParamDom i d =
    let I _ n sig _ = getInd i d
    in  take n $ dom sig

-- Given a global context d and an inductive type name i, get the argument names (i.e. its domain)
getIndArgDom :: String -> IndTypes -> [String]
getIndArgDom i d =
    let I _ n sig _ = getInd i d
    in  drop n $ dom sig

-- Given a global context d and a constructor name c, get the parameter names (i.e. its domain)
getConstrParamDom :: String -> IndTypes -> [String]
getConstrParamDom c d =
    let I _ n _ _ = getIndFromConstr c d
    in  take n . dom $ getConstrType c d

-- Given a global context d and a constructor name c, get the argument names (i.e. its domain)
getConstrArgDom :: String -> IndTypes -> [String]
getConstrArgDom c d =
    let I _ n _ _ = getIndFromConstr c d
    in  drop n . dom $ getConstrType c d

-- Given a local context and a list of terms, get an arbitrary variable name that is free within the context
-- TODO: This always returns "_"!!
getFreeVariable :: Context -> [Term a] -> String
getFreeVariable _ _ = "_"
