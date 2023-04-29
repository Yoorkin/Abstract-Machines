module Builtin (invoke) where
import Prelude hiding (EQ,GT,LT)
import Lambda

invoke :: Operation -> [Lambda] -> IO Lambda
invoke o1 [Const b] =
  Const <$> b'
  where
    b' = case o1 of
      Write ->
        case b of
          Integer x -> do
            print x
            return Unit
          Boolean x -> do
            print x
            return Unit
          Unit -> return Unit
      Add1 -> return b
      Sub1 ->
        case b of
          Integer x -> return $ Integer (-x)
          _ -> error "invalid args for sub1"
      _ -> error "undefined prim"
invoke o2 [Const b1, Const b2] =
  return $ Const b'
  where
    b' =
      case o2 of
        Add2 ->
          case (b1, b2) of
            (Integer a, Integer b) -> Integer (a + b)
            _ -> error "invalid args for add2"
        Sub2 ->
          case (b1, b2) of
            (Integer a, Integer b) -> Integer (a - b)
            _ -> error "invalid args for sub2"
        Mul ->
          case (b1, b2) of
            (Integer a, Integer b) -> Integer (a * b)
            _ -> error "invalid args for mul"
        GT ->
          case (b1, b2) of
            (Integer a, Integer b) -> Boolean (a > b)
            _ -> error "invalid args for GT"
        GE ->
          case (b1, b2) of
            (Integer a, Integer b) -> Boolean (a >= b)
            _ -> error "invalid args for GE"
        EQ ->
          case (b1, b2) of
            (Integer a, Integer b) -> Boolean (a == b)
            _ -> error "invalid args for EQ"
        NE ->
          case (b1, b2) of
            (Integer a, Integer b) -> Boolean (a /= b)
            _ -> error "invalid args for NE"
        LE ->
          case (b1, b2) of
            (Integer a, Integer b) -> Boolean (a <= b)
            _ -> error "invalid args for LE"
        LT ->
          case (b1, b2) of
            (Integer a, Integer b) -> Boolean (a < b)
            _ -> error "invalid args for LT"
        _ -> error "undefined prim"
invoke _ args = error $ "o" ++ show (length args) ++ " prim undefined"
