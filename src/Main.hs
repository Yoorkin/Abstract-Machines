module Main (main) where
import Lambda
import CESKMachine
import Pass
import Debug.Trace
import Data.Text.Prettyprint.Doc
import Prettyprinter.Util

testSeq = Sequence (Mutate "a" (Const $ Integer 1919810)) (Var "a")

testRec = Letrec ("id", Abstract "x" (Var "x")) (Apply (Var "id") (Const $ Integer 111)) 

recFib = Letrec ("fib", Abstract "x"
                  (If (Prim Lambda.LT [Var "x", Const $ Integer 2])
                    (Var "x")
                    (Prim Add2 [
                        Apply (Var "fib") (Prim Sub2 [Var "x", Const $ Integer 1]),
                        Apply (Var "fib") (Prim Sub2 [Var "x", Const $ Integer 2])
                      ]
                    )
                  )) $
    Prim Write [Apply (Var "fib") (Const (Integer 9))]

main :: IO ()
main = do
    -- eval lambda
    let input = recFib
    putStrLn "lambda:"
    putDocW 40 (pretty input)
    putStrLn "\nside effects:"
    r <- eval input
    putStrLn "eval result:"
    print r 
    return ()
  where
    fib = Let ("fib", Apply yvCombinator
                        (Abstract "fib" (Abstract "x"
                          (If (Prim Lambda.LT [Var "x", Const $ Integer 2])
                            (Var "x")
                            (Prim Add2 [
                                Apply (Var "fib") (Prim Sub2 [Var "x", Const $ Integer 1]),
                                Apply (Var "fib") (Prim Sub2 [Var "x", Const $ Integer 2])
                              ]
                            )
                          )))) $
            Prim Write [Apply (Var "fib") (Const (Integer 35))]
    lambda = Prim Write [
              Let ("number", Const (Integer 114514)) $
                Let ("subX", Abstract "x" (Prim Sub2 [Var "number", Var "x"])) $
                  Let ("addX", Abstract "x" (Prim Add2 [Var "number", Var "x"])) $
                    Sequence (Prim Write [Const Unit])
                    (If (Prim Lambda.GT [Const (Integer 5), Const (Integer 6)])
                        (Apply (Var "subX") (Const $ Integer 1))
                        (Apply (Var "addX") (Const $ Integer 2))
                    )
             ]
            -- write (
            --    let number = 114514 in
            --      let subX = (x) => number - x in
            --        let addX = (x) => number + x in
            --          if 5 > 6 then subX 1 else addX 2
            -- )

    -- lambda = Prim Write [Apply (Abstract "x"
    --                           (If (Prim Lambda.GT [Const (Integer 5), Const (Integer 6)]) 
    --                             (Prim Sub2
    --                               [Var "x", Const (Integer 1)]
    --                             )
    --                             (Prim Add2
    --                               [Var "x", Const (Integer 1)]
    --                             )
    --                           )
    --                       )
    --                   (Const (Integer 114515))]
    -- (write ((λx.x-1) 114515))

