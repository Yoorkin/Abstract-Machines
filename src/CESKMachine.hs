module CESKMachine (eval) where

import Builtin
import Data.IORef
import Data.HashMap.Strict (HashMap, empty, insert, null, (!), toList, fromList, size, lookup, size, empty)
import Debug.Trace (traceShowId)
import Lambda
import Prelude hiding (null, lookup, empty)
import qualified Data.Vector.Mutable as MV
import Foreign.C (eMULTIHOP)


data Env = Env !(HashMap String Closure)

instance Show Env where
    show (Env x) = let pairs = fmap (\(n,(l,_)) -> (n,l)) (toList x) in show pairs

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
lookEnv (Env e) x = e ! x

storeAddress :: State -> String -> Maybe Addr
storeAddress State { control = (_, e) } name =
    case lookEnv e name of
        (Const (Address addr), _) -> Just addr
        _ -> Nothing

emptyStore :: IO Store
-- emptyStore :: MV.MVector Int (Lambda, Env)
emptyStore = do
    vec <- MV.generate 100 (const (Const Unit, emptyEnv))
    return Store { vec = vec, used = 0 }

-- setStore :: (Env, Store) -> String -> Closure -> IO (Env, Store)
-- setStore (Env e,s) name c =
--     case lookup name e of
--         Just (Const (Address addr), _) ->
--             do MV.write (vec s) addr c
--                return (Env e, s)
--         _ -> let freshAddr = used s
--                  closure = (Const $ Address freshAddr, emptyEnv)
--               in do 
--                 let ls = vec s
--                 let idx = used s + 1
--                 ls <- if idx == MV.length ls then MV.grow ls (idx * 2) else return ls
--                 MV.unsafeWrite ls freshAddr c
--                 return (appendEnv (Env e) name closure, Store { vec = ls, used = idx })

-- getStore :: Store -> Int -> IO Closure
-- getStore s = MV.unsafeRead (vec s)


newStore :: State -> String -> IO (State, Addr)
newStore State { control = (m, Env e), store = s, kont = k } name =
     let addr = used s + 1
         e' = insert name (Const (Address addr), emptyEnv) e
      in do
            let ls = vec s
            vec' <- if addr == MV.length ls then MV.grow ls (addr * 2) else return ls
            return (State { control = (m, Env e'), store = Store { vec = vec', used = addr }, kont = k}, addr)

type Addr = Int

setStore :: State -> Addr -> Closure -> IO State 
setStore state@State { store } addr v = do
    MV.unsafeWrite (vec store) addr v
    return state

getStore :: State -> Addr -> IO Closure
getStore State { store } = MV.unsafeRead (vec store)

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
    show (Arg (m,_) k) = "Arg " ++ show m ++ " -> " ++ show k
    show (Opd vs o ns k) = "Opd " ++ show vs ++ " " ++ show o ++ " " ++ show (fst <$> ns) ++ " -> " ++ show k
    show (Branch (m1,_) (m2,_) k) = "Branch " ++ show m1 ++ " " ++ show m2 ++ " -> " ++ show k
    show (KLet s (m,_) k) = "KLet " ++ s ++ show m ++ " -> " ++ show k
    show (KMut s k) = "KMut " ++ show s ++ " -> " ++ show k
    show (KSeq (m,_) k) = "KSeq " ++ show m ++ " -> " ++ show k


data State = State { control :: Closure, store :: Store, kont :: Kont }

controlEnv State { control = (_, e) } = e

instance Show State where
    show State { control = (c,e), store, kont } =
        "# lambda: \n" ++ show c ++ "\n" ++
        "# environment: \n" ++ show e ++ "\n" ++
        "# continuation: \n" ++ show kont ++ "\n" ++
        "\n" 

trans :: State -> IO State

trans state = case state of

    -- letrec
    State { control = (Letrec (name, func) body, e), store, kont } -> do 
        (state, addr) <- newStore state name
        state <- setStore state addr (func, controlEnv state)
        return state { control = (body, controlEnv state), kont }

    -- intro KSeq
    State { control = (Sequence m1 m2, e), kont } ->
        return state { control = (m1, e), kont = KSeq (m2, e) kont }

    -- elim KSeq
    State { control = (Const Unit, e), kont = KSeq (m, _) kont } ->
        return state { control = (m, e), kont }

    -- elim KMut
    State { control = (v,e), kont = KMut name kont } | isValue v -> 
        case storeAddress state name of
            Just addr -> do
                state <- setStore state addr (v,e)
                return state { control = (Const Unit, e), kont }
            Nothing -> error "invalid input"

    -- intro KMut
    State { control = (Mutate name expr, e), kont } ->
        return state { control = (expr, e), kont = KMut name kont }

    -- cek7  lookup variable in environment and store
    State { control = (Var x, e) } -> do 
        control <- case lookEnv e x of
                (Const (Address addr), _) -> getStore state addr
                m -> return m
        return state { control } 

    -- cek1
    State { control = (Apply m n, e), kont } ->
        return state { control = (m, e), kont = Arg (n, e) kont } 

    -- cek3
    State { control = (v, e), kont = Fun (Abstract x1 m, e') kont }  | isValue v -> -- && notVar v =
        return state { control = (m, appendEnv e' x1 (v, e)), kont } 

    -- cek4
    State { control = (v, e), kont = Arg (n, e') kont } | isValue v -> -- && notVar v =
        return state { control = (n, e'), kont = Fun (v, e) kont } 

    -- intro branch
    State { control = (If m1 m2 m3, e), kont } ->
      return state { control = (m1, e), kont = Branch (m2, e) (m3, e) kont } 

    -- elim branch
    State { control = (Const (Boolean cond), _), kont = Branch c1 c2 kont } ->
      return state { control = if cond then c1 else c2, kont } 

    -- intro let scope
    State { control = (Let (x, expr) body, e), kont } ->
      return state { control = (expr,e), kont = KLet x (body, e) kont }  

    -- elim let scope
    State { control = (v,e), kont = KLet x (body, e') kont } | isValue v ->
      return state { control = (body, appendEnv e' x (v,e)), kont }    

    -- cek2
    State { control = (Prim o (m : ns), e), kont } ->
        let ns' = zip ns (repeat e)
        in return state { control = (m, e), kont = Opd [] o ns' kont } 

    -- cek5
    State { control = (b@(Const _), e), kont = Opd cs o [] kont } -> do
        v <- invoke o (reverse (b : (fmap fst cs)))
        return state { control = (v, e), kont } 

    -- cek6
    State { control = (v, e), kont = Opd vs o (c : cs) kont } | isValue v ->  -- && notVar v =
        return state { control = c, kont = Opd ((v, e) : vs) o cs kont } 

    _ -> error $ "\n\nunexpected state:" ++ show state

isConstOrAbstract (Const _) = True
isConstOrAbstract (Abstract _ _) = True
isConstOrAbstract _ = False

eval' :: State -> IO State
eval' s@State { control = (v, _), kont = Mt } | isConstOrAbstract v = return s
eval' s = do
  s' <- trans s
--   eval' (traceShowId s')
  eval' s' --(traceShowId s')

eval :: Lambda -> IO Lambda
eval m = do 
    store <- emptyStore
    let s = State { control = (m, emptyEnv), store, kont = Mt } in f <$> eval' s
        where f State { control = (x, _) } = x