-- Do not make changes to this file.
-- If you get a type error here, it means the grader won't be able to inspect your datatype.
-- Make sure your custom datatypes derive Generic and CustomData.
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GradeLib.GradeAST () where

import Calendar
import DateTime
import GHC.Generics
import GradeLib.CustomASTData

data Repl = Repl

deriving instance Generic DateTime

deriving instance Generic Calendar

deriving instance Generic Event

deriving instance Generic Token

instance Inspect Calendar

instance Inspect DateTime where
  inspect = const Repl

instance Inspect Event

instance Inspect Token

class (Eq a) => Inspect a where
  inspect :: a -> Repl
  default inspect :: (Generic a, Inspect' (Rep a)) => a -> Repl
  inspect x = inspect' (from x)

instance {-# OVERLAPPING #-} Inspect String where
  inspect = const Repl

instance {-# OVERLAPPABLE #-} (Inspect a) => Inspect [a]

instance {-# OVERLAPPABLE #-} (Eq a, CustomData a, Inspect' (Rep a)) => Inspect a

instance (Inspect a) => Inspect (Maybe a)

instance (Inspect a, Inspect b) => Inspect (Either a b)

instance (Inspect a, Inspect b) => Inspect (a, b)

instance (Inspect a, Inspect b, Inspect c) => Inspect (a, b, c)

instance (Inspect a, Inspect b, Inspect c, Inspect d) => Inspect (a, b, c, d)

instance (Inspect a, Inspect b, Inspect c, Inspect d, Inspect e) => Inspect (a, b, c, d, e)

instance (Inspect a, Inspect b, Inspect c, Inspect d, Inspect e, Inspect f) => Inspect (a, b, c, d, e, f)

instance (Inspect a, Inspect b, Inspect c, Inspect d, Inspect e, Inspect f, Inspect g) => Inspect (a, b, c, d, e, f, g)

instance (Inspect a, Inspect b, Inspect c, Inspect d, Inspect e, Inspect f, Inspect g, Inspect h) => Inspect (a, b, c, d, e, f, g, h)

class Inspect' f where
  inspect' :: f p -> Repl

instance Inspect' V1 where
  inspect' x = case x of {}

instance Inspect' U1 where
  inspect' U1 = Repl

instance (Inspect' f, Inspect' g) => Inspect' (f :+: g) where
  inspect' (L1 x) = inspect' x
  inspect' (R1 x) = inspect' x

instance (Inspect' f, Inspect' g) => Inspect' (f :*: g) where
  inspect' (x :*: y) = case (inspect' x, inspect' y) of (Repl, Repl) -> Repl

instance (Inspect c) => Inspect' (K1 i c) where
  inspect' (K1 x) = inspect x

instance (Inspect' f) => Inspect' (M1 i t f) where
  inspect' (M1 x) = inspect' x
