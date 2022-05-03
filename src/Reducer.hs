module Reducer (reduce) where

import Lib (Id, Term (Abs, App, Var))

reduce :: Term -> Term
reduce a = a

-- "free variable" ^= variable that is captured from outer scope
-- "bound variable" ^= variable that is used inside abstraction and is bound by the abstraction variable id
-- e.g.:
-- - bound: λa.a <- a is bound variable
-- - free:  λ.b <- b is a free variable
