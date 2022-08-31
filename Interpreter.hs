{-
Peace Okoko
12/08/2021
Programming Languages
 -}
module Main where
import TypeChecker
import Grammar
import Lexer

type OpEnv = [(String,AST)]

-- (remove_var x sigma) computes sigma_x.

remove_var:: String -> OpEnv -> OpEnv
remove_var x [] = []
remove_var x ((y,_):env) | x == y =
    remove_var x env
remove_var x ((y,ast):env) =
    (y,ast) : remove_var x env

-- Substitute an AST for a variable in an AST.

subst_var:: String -> AST -> AST -> AST
subst_var _ _ (Boolean b) = Boolean b
subst_var _ _ (Integer n) = Integer n
subst_var x e (Variable v) | x == v = e
subst_var x e (Variable v) = Variable v
subst_var x e (Plus ast1 ast2) =
    Plus (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Minus ast1 ast2) =
    Minus (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Times ast1 ast2) =
    Times (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Rem ast1 ast2) =
    Rem (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Quot ast1 ast2) =
    Quot (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (And ast1 ast2) =
    And (subst_var x e ast1) (subst_var x e ast2)
--subst_var x e (Not ast) =
--    Not (subst_var x e ast)
subst_var x e (Or ast1 ast2) =
    Or (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Equals ast1 ast2) =
    Equals (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Lt ast1 ast2) =
    Lt (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Gt ast1 ast2) =
    Gt (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (App ast1 ast2) =
    App (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (If ast1 ast2 ast3) =
    If (subst_var x e ast1) (subst_var x e ast2) (subst_var x e ast3)
subst_var x e (Let y ast1 ast2) | y == x =
    Let y (subst_var x e ast1) ast2
subst_var x e (Let y ast1 ast2) =
    Let y (subst_var x e ast1) (subst_var x e ast2)
subst_var x e (Lambda y ast t1 t2) | x == y =
    Lambda y ast t1 t2
subst_var x e (Lambda y ast t1 t2) =
    Lambda y (subst_var x e ast) t1 t2

-- Treat an environment as a variable substitution.

subst:: OpEnv -> AST -> AST
subst [] ast = ast
subst ((x,e):env) ast =
    subst env (subst_var x e ast)

-- Please complete the definition of interpreter.

interpreter:: AST -> OpEnv -> AST
interpreter (Boolean b) _ = Boolean b
interpreter (Integer n) _ = Integer n
interpreter (Variable v) env =
    let
        e = TypeChecker.lookup v env
    in
        interpreter e env
--Doing integer would not allow other expressions to be taken in
--We are defining the interpreter
interpreter (Plus e1 e2) env=
--converts--int------Intepreter--AST-environ  
    let Integer n1 = interpreter e1 env
        Integer n2 = interpreter e2 env
    in  
        let n=n1+n2
        in Integer n
interpreter (Minus e1 e2) env=
--converts--int------Intepreter--AST-environ  
    let Integer n1 = interpreter e1 env
        Integer n2 = interpreter e2 env
    in  
        let n=n1-n2
        in Integer n
interpreter (Times e1 e2) env=
--converts--int------Intepreter--AST-environ  
    let Integer n1 = interpreter e1 env
        Integer n2 = interpreter e2 env
    in  
        let n=n1*n2
        in Integer n
interpreter (Quot e1 e2) env=
--converts--int------Intepreter--AST-environ  
    let Integer n1 = interpreter e1 env
        Integer n2 = interpreter e2 env
    in  
        --Normal function has two arguments
        let n=quot n1 n2
        in Integer n
interpreter (Rem e1 e2) env=
--converts--int------Intepreter--AST-environ  
    let Integer n1 = interpreter e1 env
        Integer n2 = interpreter e2 env
    in  
        let n=rem n1 n2
        in Integer n
interpreter (Or e1 e2) env=
--converts--int------Intepreter--AST-environ  
    let Boolean n1 = interpreter e1 env
        
    in  
      if n1== True
          then 
            Boolean True
       else
        let Boolean n2 = interpreter e2 env
          in
            Boolean n2
interpreter (And e1 e2) env=
--converts--int------Intepreter--AST-environ  
    let Boolean n1 = interpreter e1 env
        
    in  
      if n1== False
          then 
            Boolean False
       else
        let Boolean n2 = interpreter e2 env
          in
            Boolean n2
interpreter (Equals e1 e2) env=
--converts--int------Intepreter--AST-environ  
    let n1 = interpreter e1 env
        n2 = interpreter e2 env
        
    in  
      if n1 /= n2
          then 
            Boolean False
       else
            Boolean True
interpreter (Lt e1 e2) env=
--converts--int------Intepreter--AST-environ  
    let n1 = interpreter e1 env
        n2 = interpreter e2 env
        compareAST (Boolean b1) (Boolean b2) = b1 < b2
        compareAST (Integer b1) (Integer b2) = b1 < b2
        
    in  
      if compareAST n1 n2
          then 
            Boolean True
       else
            Boolean False
interpreter (Gt e1 e2) env=
--converts--int------Intepreter--AST-environ  
    let n1 = interpreter e1 env
        n2 = interpreter e2 env
        compareAST (Boolean b1) (Boolean b2) = b1 > b2
        compareAST (Integer b1) (Integer b2) = b1 > b2
        
    in  
      if compareAST n1 n2
          then 
            Boolean True
       else
            Boolean False
interpreter (If e1 e2 e3) env=
--converts--int------Intepreter--AST-environ  
    let Boolean n1 = interpreter e1 env
        
    in  
      if n1== True
          then 
            interpreter e2 env
       else
            interpreter e3 env
--Functions
--Lambda
--Stuff not shadowed substitute
--Stuff is shadowed leave it there
interpreter (Lambda x e s t) env= 
    --snatched x away
         let env1= remove_var x env 
    --Substitution of env1 []
         in let e1 = subst env1 e
    --Return new one
         in Lambda x e1 s t
interpreter (Let x e1 e2) env=
    --Substitute because it might be in a new environment 
         let env1=  (x, subst env e1) : env
         in
             let n2=interpreter e2 env1 
             in n2
interpreter (App e1 e2) env=
     let Lambda x e3 s t= interpreter e1 env
        in
        let env1=  (x, subst env e2) : env
            in
                let n2=interpreter e2 env1 
                in n2




main = do
  s <- getContents
  let ast = parseHasquelito (scanTokens s)
  let t = typeChecker ast []
  let val = interpreter ast []
  print (ast,t,val)  
