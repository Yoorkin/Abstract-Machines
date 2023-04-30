module CParser where
import Lambda
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Data.Void

data CTerm = CPrimType String
          | CFunDecl CTerm String [(CTerm,String)] CTerm
          | CVarDecl CTerm String
          | CReturn CTerm
          | CBlock [CTerm]
          | CAssign String CTerm
          | CVar String
          | CInt Int
          | CBool Bool
          | CApply String [CTerm]
          | CIf CTerm CTerm (Maybe CTerm)
          | CWhile CTerm CTerm
          | CProg [CTerm]
          deriving (Show)

type Parser a = Parsec Void String a

int :: Parser Int
int = read <$> some digitChar 

ident :: Parser String
ident = (:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_')

ctype :: Parser CTerm
ctype = string "int" *> return (CPrimType "int")
    <|> string "bool" *> return (CPrimType "bool")
    <|> string "void" *> return (CPrimType "unit")
    <|> string "string" *> return (CPrimType "string")
    <|> string "char" *> return (CPrimType "char")

parens = between (string "(") (string ")")

prog = CProg <$> many (space *> (try fundecl <|> vardecl)) <* eof

vardecl, fundecl, ifstmt, whilestmt, expr, stmt :: Parser CTerm
vardecl = CVarDecl <$> ctype <*> (space *> ident)

fundecl = CFunDecl <$> ctype <*> (space *> ident) <*> (space *> parens (arg `sepBy` char ',')) <*> (space *> block)
    where arg = (,) <$> ctype <*> (space *> ident) 

ifstmt = CIf <$> (string "if" *> space *> (parens expr)) <*> (space *> block) 
            <*> optional (space *> string "else" *> space *> block) 

whilestmt = CWhile <$> (string "while" *> (parens expr)) <*> (space *> block)

stmt = ifstmt <|> whilestmt <|> vardecl <|> returnstmt <|> expr

returnstmt = CReturn <$> (string "return" *> expr)

expr = try (do 
    lhs <- addingExpr
    space
    op <- (string "<" <|> string ">" <|> string "<=" <|> string ">="
        <|> string "==" <|> string "!=" <|> string "=")
    space
    rhs <- expr
    return (CApply op [lhs, rhs])) <|> addingExpr
    
addingExpr = try (do 
    lhs <- mulExpr
    space
    op  <- (string "+" <|> string "-")
    space
    rhs <- addingExpr
    return (CApply op [lhs, rhs])) <|> mulExpr

mulExpr = try (do 
    lhs <- assignExpr
    space
    op  <- (string "*")
    space
    rhs <- mulExpr
    return (CApply op [lhs, rhs])) <|> assignExpr

assignExpr = try (do
    lhs <- ident
    space
    string "="
    space
    rhs <- expr
    return (CAssign lhs rhs)) <|> term

term = 
    try (CApply <$> ident <*> (parens ( expr `sepBy` char ',' )) )
    <|> CVar <$> ident 
    <|> CInt <$> int
    <|> string "true" *> return (CBool True)
    <|> string "false" *> return (CBool False)
    <|> parens expr

block :: Parser CTerm
block = CBlock <$> (between (char '{' <* space) (char '}') $ many (space *> stmt <* space <* string ";" <* space))


compile :: CTerm -> Lambda
compile (CProg xs) = compDecl xs Nothing

initVal (CPrimType typ) = 
    case typ of
        "int" -> Const $ Integer 0
        "bool" -> Const $ Boolean False
        "void" -> Const Unit
        -- "string" -> Const $ LString ""
        -- "char" -> Const $ LChar '0' 

appendSeq :: Lambda -> Lambda -> Lambda
appendSeq (Sequence a b) t = Sequence a (appendSeq b t)
appendSeq a t = Sequence a t

compDecl :: [CTerm] -> Maybe CTerm -> Lambda
compExpr :: CTerm -> Lambda


compDecl (x:xs) main = 
    case x of
        m@(CFunDecl _ "main" _ _) -> compDecl xs (Just m)
        (CVarDecl typ name) -> Let (name, initVal typ) (compDecl xs main)
        (CFunDecl ret_typ name params (CBlock stmts)) ->
                    let funBody = compStmt stmts (initVal ret_typ)
                        process_param [] = funBody
                        process_param ((_, name):xs) = Abstract name (process_param xs)
                        func = process_param params
                    in Letrec (name, func) (compDecl xs main) 

compDecl [] (Just (CFunDecl _ "main" _ (CBlock stmts))) = compStmt stmts (Const $ Integer 0)


compStmt :: [CTerm] -> Lambda -> Lambda
compStmt [] lastStmt = lastStmt
compStmt (x:xs) lastStmt = 
    case x of
        (CVarDecl typ name) -> 
            Let (name, initVal typ) (compStmt xs lastStmt)
        (CWhile cond (CBlock expr)) ->
            let jump = (Apply (Var "loop1") (Const Unit))
                loop = (If (compExpr cond) (compStmt expr jump) (Const Unit))
             in Letrec ("loop1", (Abstract "u" loop)) 
                            (Sequence jump (compStmt xs lastStmt))
        (CIf cond (CBlock ifso) Nothing) -> 
            let ifLam = If (compExpr cond) (compStmt ifso (Const Unit)) (Const Unit)
             in Sequence ifLam (compStmt xs lastStmt)
        (CBlock stmts) -> Sequence (compStmt stmts (Const Unit)) (compStmt xs lastStmt)
        (CReturn expr) -> compExpr expr
        _ -> Sequence (compExpr x) (compStmt xs lastStmt)

compExpr x = 
    case x of 
        (CAssign name expr) -> Mutate name (compExpr expr)
        (CVar name) -> Var name
        (CInt v) -> Const $ Integer v
        (CBool v) -> Const $ Boolean v
        (CApply f args) ->
            case (f,args) of
                ("+",[l,r]) -> Prim Add2 [compExpr l, compExpr r]
                ("-",[l,r]) -> Prim Sub2 [compExpr l, compExpr r]
                ("*",[l,r]) -> Prim Mul [compExpr l, compExpr r]
                (">",[l,r]) -> Prim Lambda.GT [compExpr l, compExpr r]
                ("<",[l,r]) -> Prim Lambda.LT [compExpr l, compExpr r]
                ("<=",[l,r]) -> Prim LE [compExpr l, compExpr r]
                (">=",[l,r]) -> Prim GE [compExpr l, compExpr r]
                ("==",[l,r]) -> Prim Lambda.EQ [compExpr l, compExpr r]
                ("!=",[l,r]) -> Prim NE [compExpr l, compExpr r]
                ("+",[r]) -> Prim Add1 [compExpr r]
                ("-",[r]) -> Prim Sub1 [compExpr r]
                (f,args) -> 
                    let apply acc (x:xs) = apply (Apply acc (compExpr x)) xs
                        apply acc [] = acc 
                     in apply (Var f) args


test = case parse prog "" ( "int i " ++ 
                            "int foo(int x){" ++ 
                            "   while(i > 100){ " ++ 
                                   "i = i + 1 + 2 * 3;" ++ 
                                   "foo(20);" ++ 
                                "};" ++ 
                                "i = abc(100);" ++ 
                            "} " ++ 
                            "int main(){ " ++ 
                                "foo(5);"  ++ 
                            "}") of
         Left bundle -> Left $ show (errorBundlePretty bundle)
         Right xs -> Right $ compile xs


-- CPrimType String
--           | CFunDecl CTerm String [(CTerm,String)] CTerm
--           | CVarDecl CTerm String
--           | CReturn CTerm
--           | CBlock [CTerm]
--           | CAssign String CTerm
--           | CVar String
--           | CInt Int
--           | CBool Bool
--           | CApply String [CTerm]
--           | CIf CTerm CTerm (Maybe CTerm)
--           | CWhile CTerm CTerm
--           | CProg [CTerm]

