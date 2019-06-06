{-#LANGUAGE DeriveFunctor #-}

module Grammar.Terms
    ( Term(..)
    , bind
    , apply, unapply
    , flatten
    ) where

import Grammar.Stages
import Data.List (intercalate, foldl')

data Term a
    = Var String
    | Prop | Set | Type Int                     -- ∀ 1 <= i <= j. Prop <= Set <= Type i <= Type j
    | Abs String (Term Bare) (Term a)           -- λx: T°. T[a]
    | App (Term a) (Term a)                     -- T[a] T[a]
    | Let String (Term a) (Term Bare) (Term a)  -- let x = T[a]: T° in T[a]
    | Prod String (Term a) (Term a)             -- Πx: T[a]. T[a]
    | Ind String a [Term a] [Term a]            -- I^s T[a]... T[a]...  (parameters and indices)
    | Constr String [Term Bare] [Term a]        -- C T°... T[a]...      (parameters and arguments)
    | Case (Term a) (Term Bare) [Term a]        -- case T[a] of T[a]... : T°
    | Fix Int String (Term Position) (Term a)   -- fix_n x: T* = T[a]
    deriving (Eq, Functor)

instance Show a => Show (Term a) where
    show t = case t of
        Var x           -> x
        Prop            -> "Prop"
        Set             -> "Set"
        Type j          -> "Type " ++ show j
        Abs x t e       -> "(λ" ++ x ++ ": " ++ show t ++ ". " ++ show e ++ ")"
        App e1 e2       -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
        Let x e1 t e2   -> "(let " ++ x ++ " = " ++ show e1 ++ ": " ++ show t ++ " in " ++ show e2 ++ ")"
        Prod x t1 t2    -> "(Π" ++ x ++ ": " ++ show t1 ++ ". " ++ show t2 ++ ")"
        Ind i s ps as   -> unwords . filter (not . null) $ [i ++ show s, unwords (map show ps), unwords (map show as)]
        Constr c ps as  -> unwords . filter (not . null) $ [c, unwords (map show ps), unwords (map show as)]
        Case e t es     -> "(case " ++ show e ++ " of " ++ intercalate " | " (map show es) ++ ": " ++ show t ++ ")"
        Fix n x t e     -> "(fix_" ++ show n ++ " " ++ show x ++ ": " ++ show t ++ " = " ++ show e ++ ")"

-- Perform beta-/zeta-reduction by binding t{x/e}
bind :: Term Stage -> String -> Term Stage -> Term Stage
bind t x e = bindStage t
    where
        bindStage   = binder bindStage e
        bindEps     = binder bindEps   (fmap (const Epsilon)  e)
        bindPos     = binder bindPos   (fmap (const Position) e)
        bindBare    = binder bindBare  (fmap (const Bare)     e)

        bindFix :: Int -> Term Position -> Term Position
        bindFix 1 (Prod x t1 t2) = Prod x (bindPos t1) (bindEps t2)
        bindFix n (Prod x t1 t2) = Prod x (bindEps t1) (bindFix (n - 1) t2)

        binder :: (Term a -> Term a) -> Term a -> Term a -> Term a
        binder bind' e t = case t of
            Var x' ->
                if x == x' then e else Var x'
            Abs x' t' e' ->
                if x == x'
                then Abs x' (bindBare t') e'
                else Abs x' (bindBare t') (bind' e')
            Prod x' t1 t2 ->
                if x == x'
                then Prod x' (bind' t1) t2
                else Prod x' (bind' t1) (bind' t2)
            Let x' e1 t' e2 ->
                if x == x'
                then Let x' (bind' e1) (bindBare t') e2
                else Let x' (bind' e1) (bindBare t') (bind' e2)
            Fix n x' t' e' ->
                if x == x'
                then Fix n x' (bindFix n t') e'
                else Fix n x' (bindFix n t') (bind' e')
            App e1 e2      -> App (bind' e1) (bind' e2)
            Ind i s ps as  -> Ind i s (map bind' ps) (map bind' as)
            Constr c ps as -> Constr c (map bindBare ps) (map bind' as)
            Case e' t' es  -> Case (bind' e') (bindBare t') (map bind' es)
            _              -> t

{- Transformations of terms -}

-- Turn (x ps as) into actual application (App (App x p1...) as...)
apply :: Term a -> [Term a] -> [Term a] -> Term a
apply x ps as = foldl' (\abs p -> App abs p) x (ps ++ as)

-- Turn application (App (App x p1...) as...) into (x ps as)
-- n is the number of arguments and NOT the number of parameters
unapply :: Term a -> Int -> (Term a, [Term a], [Term a])
unapply (App abs p) 0 =
    let (x, ps, _) = unapply abs 0
    in  (x, ps ++ [p], [])
unapply (App abs a) n =
    let (x, ps, as) = unapply abs (n - 1)
    in  (x, ps, as ++ [a])
unapply x 0 = (x, [], [])

-- Turm product (Prod x1 t1 (... (Prod xn tn body))) into flat list ([(x1, t1), ...], body)
flatten :: Term a -> ([(String, Term a)], Term a)
flatten (Prod x t b) =
    let (xts, body) = flatten b
    in  ((x, t) : xts, body)
flatten b = ([], b)

{-
-- like a map, except f g h can decide what to do on select constructors
-- need three different functions to handle the three kinds of terms
-- TODO: I think this is some sort of Traversable pattern?
visit :: (Term a -> Term a) -> (Term Position -> Term Position) -> (Term Bare -> Term Bare) -> Term a -> Term a
visit f g h t = case t of
    Abs x t e       -> Abs x (h t) (f e)
    App e1 e2       -> App (f e1) (f e2)
    Let x e1 t e2   -> Let x (f e1) (h t) (f e2)
    Prod x t1 t2    -> Prod x (f t1) (f t2)
    Ind i s ps as   -> Ind i s (map f ps) (map f as)
    Constr c ps as  -> Constr c (map h ps) (map f as)
    Case e t es     -> Case (f e) (h t) (map f es)
    Fix n x t e     -> Fix n x (g t) (f e)
    _               -> t
-}
