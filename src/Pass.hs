module Pass(translate)where

import Lambda

translate :: Lambda -> Lambda 
-- translate (Letrec (name, abs) body) = (Let (name, Const Unit) (Sequence (Mutate name abs) body))
translate x = x