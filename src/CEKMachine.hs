module CEKMachine (eval) where

import Builtin
import Data.HashMap.Strict (HashMap, empty, insert, null, (!), fromList)
import Debug.Trace (traceShowId)
import Lambda
import Prelude hiding (null)

data Env = Env !(HashMap String Closure) deriving (Show)

type Closure = (Lambda, Env)

type Val = (Lambda, Env)

emptyEnv :: Env
emptyEnv = Env empty

isEmptyEnv (Env e) = null e

appendEnv :: Env -> String -> Closure -> Env
appendEnv (Env e) x c = Env $ insert x c e

lookEnv :: Env -> String -> Closure
lookEnv (Env e) x = e ! x

data Kont
  = Mt
  | Fun !Val !Kont
  | Arg !Closure !Kont
  | Opd ![Val] !Operation ![Closure] !Kont
  | Branch !Closure !Closure !Kont
  | KLet !String !Closure !Kont
  | KLetrec 
  deriving (Show)

type State = (Closure, Kont)

trans :: State -> IO State
trans ((Var x, e), k) = trans (lookEnv e x, k) -- cek7

trans ((Apply m n, e), k) = trans ((m, e), Arg (n, e) k) -- cek1

trans ((v, e), Fun (Abstract x1 m, e') k)
  | isValue v = -- && notVar v =
      trans ((m, appendEnv e' x1 (v, e)), k) -- cek3

trans ((v, e), Arg (n, e') k)
  | isValue v = -- && notVar v =
      trans ((n, e'), Fun (v, e) k) -- cek4

trans ((If m1 m2 m3, e), k) =
      trans ((m1, e), Branch (m2, e) (m3, e) k) -- intro branch

trans ((Const (Boolean cond), e), Branch c1 c2 k) =
      trans (if cond then c1 else c2, k) -- elim branch

trans ((Let (x, expr) body, e), k) = 
      trans ((expr,e), KLet x (body, e) k)  -- intro let scope

trans ((v,e), KLet x (body, e') k) | isValue v = 
      trans ((body, appendEnv e' x (v,e)), k)   -- elim let scope

trans ((Prim o (m : ns), e), k) =
  let ns' = zip ns (repeat e)
   in trans ((m, e), Opd [] o ns' k) -- cek2

trans s@((Const _, _), Mt) = return s 

trans s@((Abstract _ _, _), Mt) = return s

trans ((b@(Const _), e), Opd cs o [] k) =
  do
    v <- invoke o (reverse (b : (fmap fst cs)))
    trans ((v, emptyEnv), k) -- cek5

trans ((v, e), Opd vs o (c : cs) k)
  | isValue v =  -- && notVar v =
      trans (c, Opd ((v, e) : vs) o cs k) -- cek6

trans s = error $ "\n\nunexpected state:" ++ show s
-- trans ((Letrec bs body, e@(Env mp)), k) = let e' = Eev (union (fromList bs) )

isConstOrAbstract (Const _) = True
isConstOrAbstract (Abstract _ _) = True
isConstOrAbstract _ = False

-- eval' :: State -> IO State
-- eval' s@((v, _), Mt) | isConstOrAbstract v = return s
-- eval' s = do
--   s' <- trans s
--   eval' s' --(traceShowId s')

eval :: Lambda -> IO Lambda
eval m = (fst . fst) <$> trans ((m, emptyEnv), Mt)