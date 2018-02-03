
module Week8.Party where

import Week8.Employee
import Data.Monoid
import Data.Tree

-- exercise 1 -----------------
glCons :: Employee -> GuestList -> GuestList
glCons e (GL list fun) = GL (e : list) (empFun e + fun)

--
instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

-- return the GuestList that has more fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 > f2 then gl1 else gl2

-- exercise 2 fold data tree ----------------
treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f b (Node a ts) =
    let newB = f b a in
    foldl (treeFold f) newB ts

-- to test >  treeFold (\b a -> b + empFun a) 0 testCompany