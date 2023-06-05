module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp div _         (IntVal 0) = ExnVal "Division by 0"
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"


--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  env = IntVal i
eval (BoolExp b) env = BoolVal b

--- ### Variables

eval (VarExp s) env = case H.lookup s env of
                        Just val -> val
                        Nothing  -> ExnVal "No match in env"


--- ### Arithmetic

eval (IntOpExp op e1 e2) env = do
    IntVal v1 <- eval e1 env
    IntVal v2 <- eval e2 env
    case (v1, v2) of
        (IntVal i1, IntVal i2) ->
            if op == "/" && i2 == 0
            then return (ExnVal "Division by 0")
            else case H.lookup op intOps of
                    Just f  -> return (IntVal (f i1 i2))
                    Nothing -> return (ExnVal "Unknown operator")
        _ -> return (ExnVal "Cannot lift")
        
--- ### Boolean and Comparison Operators

eval :: Exp -> Env -> Maybe Val
eval (BoolOpExp op e1 e2) env = do
    BoolVal v1 <- eval e1 env
    BoolVal v2 <- eval e2 env
    case lookup op [ ("and", (&&)), ("or", (||)) ] of
        Just operator -> return $ BoolVal (operator v1 v2)
        Nothing -> Nothing

eval (CompOpExp op e1 e2) env = do
    IntVal v1 <- eval e1 env
    IntVal v2 <- eval e2 env
    case lookup op [ ("==", (==)), ("/=", (/=)), ("<", (<)), (">", (>)), ("<=", (<=)), (">=", (>=)) ] of
        Just operator -> return $ BoolVal (operator v1 v2)
        Nothing -> Nothing

--- ### If Expressions

eval (IfExp e1 e2 e3) env = 
    let v1 = eval e1 env
    in case v1 of
        BoolVal True -> eval e2 env
        BoolVal False -> eval e3 env
        _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = 
    let v1 = eval e1 env
        argVals = map (`eval` env) args
    in case v1 of
        CloVal params body closureEnv ->
            let newEnv = H.union (H.fromList (zip params argVals)) closureEnv
            in eval body newEnv
        _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env =
    let newBindings = map (\(s, e) -> (s, eval e env)) pairs
        newEnv = H.union (H.fromList newBindings) env
    in eval body newEnv

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env =
    let val = eval e env
        newEnv = H.insert var val env
    in ("", penv, newEnv)

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (s:stmts)) penv env =
    let (out1, newPEnv, newEnv) = exec s penv env
        (out2, finalPEnv, finalEnv) = exec (SeqStmt stmts) newPEnv newEnv
    in (out1 ++ out2, finalPEnv, finalEnv)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = case eval e1 env of
    BoolVal True  -> exec s1 penv env
    BoolVal False -> exec s2 penv env
    _             -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = 
    let penv' = H.insert name p penv 
    in ("", penv', env)

exec (CallStmt name args) penv env =
    case H.lookup name penv of
        Nothing -> ("Procedure " ++ name ++ " undefined", penv, env)
        Just (ProcedureStmt _ params body) -> 
            exec (body) penv (H.union (H.fromList (zip params (map (\z -> eval z env) args))) (env))
