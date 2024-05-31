module LC (Term(..), tnf) where

data Term = Var String
          | App Term Term
          | Lam String Term
          deriving (Show, Eq)

-- Function to substitute a variable with a term in another term
subst :: String -> Term -> Term -> Term
subst x s (Var y)        | x == y    = s
                         | otherwise = Var y
subst x s (App t1 t2)                = App (subst x s t1) (subst x s t2)
subst x s (Lam y t)      | x == y    = Lam y t
                         | otherwise = Lam y (subst x s t)

-- Function to perform beta reduction if possible
betaReduce :: Term -> Maybe Term
betaReduce (App (Lam x t1) t2) = Just $ subst x t2 t1
betaReduce _ = Nothing

-- Function to normalize a term with a limited number of resources
normalize :: Int -> Term -> Maybe Term
normalize 0 _ = Nothing
normalize _ t@(Var _) = Just t
normalize r (App t1 t2) = case betaReduce (App t1 t2) of
                            Just t' -> normalize (r - 1) t'
                            Nothing -> App <$> normalize (r `div` 2) t1 <*> normalize (r `div` 2) t2
normalize r (Lam x t) = Lam x <$> normalize (r - 1) t

-- Function to compute the normal form of a term with limited resources
tnf :: Int -> Term -> Maybe Term
tnf r t = normalize r t