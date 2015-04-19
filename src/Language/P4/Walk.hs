{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.P4.Walk
  ( Walkable(..)
  ) where

import Language.P4.AST

class Walkable a b where
  -- | @walk f x@ walks the structure @x@ (bottom up) and replaces every
  -- occurrence of an @a@ with the result of applying @f@ to it.
  walk :: (a -> a) -> b -> b
  -- | @query f x@ walks the structure @x@ (bottom up) and applies @f@
  -- to every @a@, appending the results.
  query :: Monoid c => (a -> c) -> b -> c

instance Walkable a b => Walkable a [b] where
  walk f  = map (walk f)
  query f xs = mconcat $ map (query f) xs

instance Walkable Program Program where
  walk f = f
  query f = f

instance Walkable Decl Program where
  walk f (Program xs)  = Program (walk f xs)
  query f (Program xs) = query f xs

instance Walkable FieldListEntry Program where
  walk f (Program xs) = Program (walk f xs)
  query f (Program xs) = query f xs

instance Walkable ReturnValueType Program where
  walk f (Program xs) = Program (walk f xs)
  query f (Program xs) = query f xs

instance Walkable Decl Decl where
  walk f = f
  query f = f

instance Walkable FieldListEntry Decl where
  walk f (FieldListDecl n xs) = FieldListDecl n (walk f xs)
  walk _ x = x

  query f (FieldListDecl _ xs) = query f xs
  query _ _ = mempty

instance Walkable ReturnValueType Decl where
  walk f (ParserFunctionDecl a (ParserFunctionBody b (RsReturnValueType rvt))) =
    ParserFunctionDecl a (ParserFunctionBody b (RsReturnValueType (walk f rvt)))
  walk _ x = x

  query f (ParserFunctionDecl _ (ParserFunctionBody _ (RsReturnValueType rvt))) = query f rvt
  query _ _ = mempty

instance Walkable FieldListEntry FieldListEntry where
  walk f = f
  query f = f

instance Walkable ReturnValueType ReturnValueType where
  walk f = f
  query f = f

instance Walkable BoolExpr BoolExpr where
  walk f = f
  query f = f

instance Walkable Exp Exp where
  walk f = f
  query f = f
