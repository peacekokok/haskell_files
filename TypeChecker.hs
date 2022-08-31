--Peace Okoko
--Lab 6
--Design of Programming Languages.
module TypeChecker where
import Grammar
import Lexer

-- Type alias for type environments.

type TypeEnv = [(String,TypeExp)]

-- Function for retrieving types of variables form the environment.

lookup::String -> [(String,a)] -> a
lookup s  []                    = error ("Type " ++ s ++ " not defined in the current environment")
lookup s1 ((s2,t):l) | s1 == s2 = t
lookup s  (_:l)                 = TypeChecker.lookup s l

-- Please complete the definition of typeChecker for the rest of the abstract syntax.

typeChecker:: AST -> TypeEnv -> TypeExp
typeChecker (Boolean _) _ = BoolType
typeChecker (Integer _) _ = IntType
typeChecker (Variable s) env = TypeChecker.lookup s env
--Operations, capital names = constructor lowercase=variable
--Plus
typeChecker (Plus e1 e2) env =
    if typeChecker e1 env == IntType
        && typeChecker e2 env == IntType
        then IntType
        else error "what did you do?"
--Minus 
typeChecker (Minus e1 e2) env =
    if typeChecker e1 env == IntType
        && typeChecker e2 env == IntType
        then IntType
        else error "what did you do?"
--Times
typeChecker (Times e1 e2) env =
    if typeChecker e1 env == IntType
        && typeChecker e2 env == IntType
        then IntType
        else error "what did you do?"
--Quot
typeChecker (Quot e1 e2) env =
    if typeChecker e1 env == IntType
        && typeChecker e2 env == IntType
        then IntType
        else error "what did you do?"
--Rem
typeChecker (Rem e1 e2) env =
    if typeChecker e1 env == IntType
        && typeChecker e2 env == IntType
        then IntType
        else error "what did you do?"
--And
typeChecker (And e1 e2) env =
    if typeChecker e1 env == BoolType
        && typeChecker e2 env == BoolType
        then BoolType
        else error "what did you do?"
--Or
typeChecker (Or e1 e2) env =
    if typeChecker e1 env == BoolType
        && typeChecker e2 env == BoolType
        then BoolType
        else error "what did you do?"
--Equals
typeChecker (Equals e1 e2) env = 
    if eqPrimType (typeChecker e1 env) (typeChecker e2 env)
        then BoolType
        else error "what did you do?"
--greater
typeChecker (Gt e1 e2) env = 
    if eqPrimType (typeChecker e1 env) (typeChecker e2 env)
        then BoolType
        else error "what did you do?"
--less
typeChecker (Lt e1 e2) env = 
    if eqPrimType (typeChecker e1 env) (typeChecker e2 env)
        then BoolType
        else error "what did you do?"
--Let
typeChecker (Let x e1 e2) env=
    let
        s = typeChecker e1 env
        t = typeChecker e2 env1
        env1=(x,s) :env
    in t
--if 
typeChecker (If e1 e2 e3) env=
    if typeChecker e1 env==BoolType &&
       typeChecker e2 env==typeChecker e3 env
       then typeChecker e2 env
       else error "I cannot believe you made that mistake"
--lambda
typeChecker(Lambda x e s t) env=
    let env1= (x,s): env
--If is in in
    in 
--Already are given t so check to see if it is equals
    if typeChecker e env1==t
        then (Arrow s t)
        else error "Shut up"
--Function Application
typeChecker(App e1 e2) env=
    case typeChecker e1 env of
--s= source type t=target type 
        Arrow s t -> 
            if typeChecker e2 env==s
                then t
                else error "Life is a mystery everyone must alone"
        _ -> error "What do you mean, it's not an arrow?"

    
    




eqPrimType :: TypeExp -> TypeExp -> Bool
eqPrimType (BoolType) (BoolType) = True
eqPrimType (IntType) (BoolType) = False
eqPrimType (BoolType) (IntType) = False
eqPrimType (IntType) (IntType) = True
--Cannot compare functions (arrow constructor)
eqPrimType _ _ = False