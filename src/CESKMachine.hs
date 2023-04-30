module CESKMachine (eval) where

import Builtin
import Data.IORef
import Data.HashMap.Strict (HashMap, empty, insert, null, (!), fromList, size, lookup, size, empty)
import Debug.Trace (traceShowId)
import Lambda
import Prelude hiding (null, lookup, empty)
import qualified Data.Vector.Mutable as MV
import Foreign.C (eMULTIHOP)


data Env = Env !(HashMap String Closure) deriving (Show)

type Closure = (Lambda, Env)

type Val = (Lambda, Env)

data Store = Store { vec :: MV.IOVector Closure, used :: Int }

instance Show Store where
    show _ = ""

emptyEnv :: Env
emptyEnv = Env empty

isEmptyEnv (Env e) = null e

appendEnv :: Env -> String -> Closure -> Env
appendEnv (Env e) x c = Env $ insert x c e

lookEnv :: Env -> String -> Closure
lookEnv (Env e) x = e 
! x

emptyStore :: IO Store
-- emptyStore :: MV.MVector Int (Lambda, Env)
emptyStore = do
    vec <- MV.generate 100 (const (Const Unit, emptyEnv))
    return Store { vec = vec, used = 0 }

setStore :: (Env, Store) -> String -> Closure -> IO (Env, Store)
setStore (Env e,s) name c =
    case lookup name e of
        Just (Const (Address addr), _) ->
            do MV.write (vec s) addr c
               return (Env e, s)
        _ -> let freshAddr = used s
                 closure = (Const $ Address freshAddr, emptyEnv)
              in do 
                let ls = vec s
                let idx = used s + 1
                ls <- if idx == MV.length ls then MV.grow ls (idx * 2) else return ls
                MV.unsafeWrite ls freshAddr c
                return (appendEnv (Env e) name closure, Store { vec = ls, used = idx })

getStore :: Store -> Int -> IO Closure
getStore s = MV.unsafeRead (vec s)


data Kont
  = Mt
  | Fun !Val !Kont
  | Arg !Closure !Kont
  | Opd ![Val] !Operation ![Closure] !Kont
  | Branch !Closure !Closure !Kont
  | KLet !String !Closure !Kont
  | KMut !String !Kont
  | KSeq !Closure !Kont


instance Show Kont where
    show Mt = "Mt"
    show (Fun (v,_) k) = "Fun " ++ show v ++ " -> " ++ show k
    show (Arg (m,_) k) = "Arg " ++ show m ++ " ->" ++ show k
    show (Opd vs o ns k) = "Opd " ++ show vs ++ " " ++ show o ++ " " ++ show (fst <$> ns) ++ " -> " ++ show k
    show (Branch (m1,_) (m2,_) k) = "Branch " ++ show m1 ++ " " ++ show m2 ++ " -> " ++ show k
    show (KLet s (m,_) k) = "KLet " ++ s ++ show m ++ " -> " ++ show k
    show (KMut s k) = "KMut " ++ show s ++ " -> " ++ show k
    show (KSeq (m,_) k) = "KSeq " ++ show m ++ " -> " ++ show k


type State = (Closure, Store, Kont)

trans :: State -> IO State

trans ((Letrec (name,expr) body, e), s, k) = do 
    (e1,s1) <- setStore (e,s) name (Const Unit, emptyEnv)
    (e2,s2) <- setStore (e1,s1) name (expr, e1)
    trans ((body, e2), s2, k)

trans ((Sequence m1 m2, e), s, k) =
    trans ((m1, e), s, KSeq (m2, e) k)

trans ((Const Unit, e), s, KSeq (m, _) k) =
    trans ((m, e), s, k)

trans ((v,e), s, KMut name k) | isValue v = do
    (e', s') <- setStore (e,s) name (v,e)
    trans ((Const Unit, e'), s', k)

trans ((Mutate name expr, e), s, k) =
    trans ((expr, e), s, KMut name k)

trans ((Var x, e), s, k) = do 
    c <- case lookEnv e x of
            (Const (Address addr), _) -> getStore s addr
            var -> return var
    trans (c, s, k) -- cek7

trans ((Apply m n, e), s, k) = trans ((m, e), s, Arg (n, e) k) -- cek1

trans ((v, e), s, Fun (Abstract x1 m, e') k)
  | isValue v = -- && notVar v =
      trans ((m, appendEnv e' x1 (v, e)), s, k) -- cek3

trans ((v, e), s, Arg (n, e') k)
  | isValue v = -- && notVar v =
      trans ((n, e'), s, Fun (v, e) k) -- cek4

trans ((If m1 m2 m3, e), s, k) =
      trans ((m1, e), s, Branch (m2, e) (m3, e) k) -- intro branch

trans ((Const (Boolean cond), e), s, Branch c1 c2 k) =
      trans (if cond then c1 else c2, s, k) -- elim branch

trans ((Let (x, expr) body, e), s, k) =
      trans ((expr,e), s, KLet x (body, e) k)  -- intro let scope

trans ((v,e), s, KLet x (body, e') k) | isValue v =
      trans ((body, appendEnv e' x (v,e)), s, k)   -- elim let scope

trans ((Prim o (m : ns), e), s, k) =
  let ns' = zip ns (repeat e)
   in trans ((m, e), s, Opd [] o ns' k) -- cek2

trans s@((Const _, _), _, Mt) = return s

trans s@((Abstract _ _, _), _, Mt) = return s

trans ((b@(Const _), e), s, Opd cs o [] k) =
  do
    v <- invoke o (reverse (b : (fmap fst cs)))
    trans ((v, emptyEnv), s, k) -- cek5

trans ((v, e), s, Opd vs o (c : cs) k)
  | isValue v =  -- && notVar v =
      trans (c, s, Opd ((v, e) : vs) o cs k) -- cek6

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
eval m = do 
    store <- emptyStore
    let s = ((m, emptyEnv), store, Mt) in f <$> trans s
        where f ((x, _), _, _) = x