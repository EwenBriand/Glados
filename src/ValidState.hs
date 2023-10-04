module ValidState
  ( ValidState (..),
    fromValidState,
    ValidState.return,
    (ValidState.>>=),
  )
where

-- ValidState is a monade that behaves exaclty like Maybe,
-- but it also has a function that allows to add an error message to the monade.
-- it is used to propagate errors in the parsing of the AST or its evaluation.

data ValidState a
  = Valid a
  | Invalid String
  deriving (Show, Ord)

return :: a -> ValidState a
return = Valid

(>>=) :: ValidState a -> (a -> ValidState b) -> ValidState b
(Valid x) >>= f = f x
(Invalid s) >>= _ = Invalid s

instance Functor ValidState where
  fmap f (Valid x) = Valid (f x)
  fmap _ (Invalid s) = Invalid s

instance Applicative ValidState where
  pure = Valid
  (Valid f) <*> (Valid x) = Valid (f x)
  (Invalid s) <*> _ = Invalid s
  _ <*> (Invalid s) = Invalid s

instance Eq a => Eq (ValidState a) where
  (Valid _) == (Invalid _) = False
  (Invalid _) == (Valid _) = False
  (Valid a) == (Valid b) = a == b
  (Invalid s1) == (Invalid s2) = s1 == s2 || s1 == "TestError" || s2 == "TestError"

instance Monad ValidState where
  return = Valid
  (Valid x) >>= f = f x
  (Invalid s) >>= _ = Invalid s

fromValidState :: a -> ValidState a -> a
fromValidState _ (Valid x) = x
fromValidState d (Invalid _) = d
