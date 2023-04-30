module Lambda
  ( Lambda (..),
    Operation (..),
    Constant (..),
    subsitition,
    isValue,
    notValue,
    isVar,
    notVar,
    isConst,
    notConst,
    yvCombinator
  )
where

import Data.List (delete, find)
import Data.Maybe (fromJust)
import Data.Text.Prettyprint.Doc
import Prettyprinter.Render.Text

type Binding = (String, Lambda)

data Lambda
  = Var !String
  | Abstract !String !Lambda
  | Apply !Lambda !Lambda
  | Prim !Operation ![Lambda]
  | Const !Constant
  | If !Lambda !Lambda !Lambda
  | Let !Binding !Lambda
  | Letrec !Binding !Lambda
  | Mutate !String !Lambda
  | Sequence !Lambda !Lambda
  deriving(Show)

data Operation = Write | Add1 | Sub1 | IsZero | Add2 | Sub2 | Mul | GT | LT | EQ | NE | LE | GE deriving (Show)

instance Pretty Operation where
  pretty o = pretty $ case o of 
                  Add2 -> "+"
                  Sub2 -> "-"
                  Mul -> "*"
                  Lambda.LT -> "<"
                  Lambda.GT -> ">"
                  LE -> "<="
                  GE -> ">="
                  Lambda.EQ -> "=="
                  NE -> "!="

instance Pretty Lambda where
  pretty (Var s) = pretty s
  pretty (Abstract x m) = pretty "λ" <> pretty x <> pretty "." <> group (nest 2 $ line' <> pretty m)
  pretty (Apply m1 m2) = pretty "(" <> group (nest 2 (pretty m1 <+> pretty m2)) <> pretty ")"
  pretty (Prim o ms) =
      case ms of
          [l, r] -> pretty "(" <> pretty l <+> pretty o <+> pretty r <> pretty ")"
          _ -> pretty (show o) <+> pretty "(" <> hsep (pretty <$> ms) <> pretty ")"
  pretty (Const c) = pretty c
  pretty (If c m1 m2) = 
      group $ align (pretty "if" <+> pretty c <> nest 2 (
      line <> pretty "then" <+> pretty m1
      <> line <> pretty "else" <+> pretty m2))
  pretty (Let (name,expr) body) = 
      group $ align (pretty "let" <+> pretty name <+> pretty "=" <+> pretty expr <> line <> pretty "in" <+> pretty body)
  pretty (Letrec (name,expr) body) =
      group (pretty "letrec" <+> pretty name <+> pretty "=" <+> pretty expr <> line <> pretty "in" <+> pretty body)
  pretty (Mutate name expr) = 
      pretty name <+> pretty ":=" <+> pretty expr
  pretty (Sequence m1 m2) = 
      align $ pretty m1 <> pretty ";" <> line <> pretty m2

data Constant
  = Integer !Int
  | Boolean !Bool
  | Address !Int
  | Unit
  deriving (Show)

instance Pretty Constant where
  pretty x = case x of
    Integer i -> pretty i
    Boolean b -> pretty b
    Address a -> pretty "^" <+> pretty a
    Unit -> pretty "unit"

yvCombinator :: Lambda
yvCombinator = let t = Abstract "g" (Apply (Var "f") (Abstract "x" (Apply (Apply (Var "g") (Var "g")) (Var "x"))))
                in Abstract "f" (Abstract "x" (Apply (Apply t t) (Var "x")))


-- validate ANF
isANF :: Lambda -> Bool
isANF (Var _) = True
isANF (Abstract _ body) = isANF body
isANF (Apply m1 m2) = isANF m1 && isANF m2
isANF (Prim _ ms) = all isANF ms
isANF (Const _) = True
isANF (If v m1 m2) = isANF v && isANF m1 && isANF m2
isANF (Let (_, m1) m2) = isANF m1 && isANF m2
isANF (Letrec (_, expr) body) = isANF expr && isANF body

-- free variables of expression
free :: Lambda -> [String]
free (Var x) = [x]
free (Abstract v x) = delete v (free x)
free (Apply xs x) = free xs ++ free x
free (Const _) = []
free (Prim _ _) = []
free (If v1 m2 m3) = free v1 ++ free m2 ++ free m3
free (Let (v, expr) body) = delete v (free expr) ++ free body
free (Letrec (name, expr) body) = delete name (free expr) ++ free body
-- free (Letrec bs body) = concatMap (free . snd) bs ++ free body

fresh :: [String] -> String
fresh xs = fromJust $ find (`elem` xs) variables
  where
    variables = [p : show (i :: Int) | p <- ['a' .. 'z'], i <- [0 ..]]

subsitition :: Lambda -> String -> Lambda -> Lambda
subsitition (Var x1) x2 m
  | x1 == x2 = m
  | x1 /= x2 = Var x1
subsitition (Abstract x1 m1) x2 m2
  | x1 == x2 = Abstract x1 m1
  | x1 /= x2 =
      let x3 = fresh (free m2 ++ delete x1 (free m1))
       in Abstract x3 (subsitition (subsitition m1 x1 (Var x3)) x2 m2)
subsitition (Apply m1 m2) x m3 = Apply (subsitition m1 x m3) (subsitition m2 x m3)
subsitition (Prim o vs) x m = Prim o (fmap (\t -> subsitition t x m) vs)
subsitition (Const c) _ _ = Const c
subsitition (If v m1 m2) x m3 = If (subsitition v x m3) (subsitition m1 x m3) (subsitition m2 x m3)
subsitition (Let (name, expr) body) x m
  | name == x = Let (name, expr) (subsitition body x m)
  | name /= x = Let (name, subsitition expr x m) (subsitition body x m)
subsitition (Letrec bind body) x m = Letrec (f bind) (subsitition body x m)
  where
    f (name, expr)
      | name == x = (name, expr)
      | otherwise = (name, subsitition expr x m)
subsitition _ _ _ = error "unexpected input"

-- subsitition (Let )
--   | If Lambda Lambda Lambda
--   | Let String Lambda Lambda
--   | Letrec [Binding] Lambda
-- -- substitution  M[X <- V]
-- substitude :: Lambda -> String -> Lambda -> Lambda
-- substitude (Var t) x r = if t == x then r else Var t
-- substitude (Apply p q) x r = Apply (substitude p x r) (substitude q x r)
-- substitude (Abstract v e) x r
--   | v == x = Abstract v (substitude e x r)
--   | v /= x =
--       if v `elem` free r
--         then substitude (convert (v ++ "'") (Abstract v e)) x r
--         else Abstract v $ substitude e x r
-- substitude (Prim o xs) x r = Prim o (fmap (\m -> substitude m x r) xs)
-- substitude (Const x) _ _ = Const x
-- substitude (If v m1 m2) x r = If (substitude v x r) (substitude m1 x r) (substitude m2 x r)

-- substitude _ _ _ = error "unexpected term"

-- -- alpha-conversion
-- convert :: String -> Lambda -> Lambda
-- convert v' (Abstract v e) = Abstract v' (cvtbound v v' e)
--   where
--     cvtbound a b (Var v) = Var $ if v == a then b else v
--     cvtbound a b (Abstract v e) = Abstract v $ if v == a then e else cvtbound a b e
--     cvtbound a b (Apply l r) = Apply (cvtbound a b l) (cvtbound a b r)
--     cvtbound a b (Const c) = Const c
--     cvtbound a b (Prim o vs) = Prim o (cvtbound a b <$> vs)
--     cvtbound a b (If v m1 m2) = If (cvtbound a b v) (cvtbound a b m1) (cvtbound a b m2)
--     cvtbound a b (Let s m1 m2) = Let s (cvtbound a b m1) (cvtbound a b m2)
-- convert _ _ = error "unexpected term"

isValue, notValue, isVar, notVar :: Lambda -> Bool
isValue x = case x of
  Var _ -> True
  Const _ -> True
  Abstract _ _ -> True
  _ -> False
notValue = not . isValue

isVar (Var _) = True
isVar _ = False

notVar = not . isVar

isConst (Const _) = True
isConst _ = False

notConst = not . isConst