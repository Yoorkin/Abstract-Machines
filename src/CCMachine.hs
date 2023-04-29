module CCMachine (eval) where

import Builtin
import Debug.Trace (traceShowId)
import Lambda

data Context = Env Context Lambda | Empty deriving (Show)

type State = (Lambda, Context)

hole :: Lambda
hole = Var "[]"

isBasic :: Lambda -> Bool
isBasic x = case x of
  Const _ -> True
  _ -> False

isHole :: Lambda -> Bool
isHole x =
  case x of
    Var "[]" -> True
    _ -> False

trans :: State -> IO State
trans (Apply m n, e) | notValue m = return (m, Env e (Apply hole n)) -- cc1
trans (Apply v m, e) | isValue v && notValue m = return (m, Env e (Apply v hole)) -- cc2
trans (Apply (Abstract x m) v, e) = return (subsitition m x v, e) -- ccβv
trans (Prim o bs, e) | all isBasic bs = (,) <$> invoke o bs <*> return e -- ccδ
trans (Prim o xs, e) = return $ introHole [] xs -- cc3
  where
    introHole vs (x : xs)
      | notValue x = (x, Env e (Prim o (vs ++ [hole] ++ xs)))
      | isValue x = introHole (vs ++ [x]) xs
    introHole _ _ = error "unexpected state in cc3"
trans (v, Env e (Apply u h)) | isValue v && isHole h = return (Apply u v, e) -- cc4
trans (v, Env e (Apply h n)) | isValue v && isHole h = return (Apply v n, e) -- cc5
trans (v, Env e (Prim o xs)) | isValue v = let xs' = elimHole xs in return (Prim o xs', e) -- cc6
  where
    elimHole (x : xs)
      | isHole x = v : xs
      | otherwise = x : elimHole xs
    elimHole _ = error "unexpected state in cc6"
trans (If m1 m2 m3, e) | notValue m1 = return (m1, Env e (If hole m2 m3)) -- intro if
trans (If (Const (Boolean True)) m _, e) = return (m, e) -- eval if-true
trans (If (Const (Boolean False)) _ m, e) = return (m, e) -- eval if-false
trans (v, Env e (If h m1 m2)) | isValue v && isHole h = return (If v m1 m2, e) -- elim if

trans s = error $ "unexpected state: " ++ show s

eval' :: State -> IO State
eval' s =
  case s of
    (Const _, Empty) -> return s
    (Abstract _ _, Empty) -> return s
    _ -> do
      s' <- trans s
      eval' (traceShowId s')

eval :: Lambda -> IO Lambda
eval x = fst <$> eval' (x, Empty)
