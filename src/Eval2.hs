module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of
                Just n -> Right n
                _      -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)

stepComm Skip s                 = Right (Skip :!: s)

stepComm (Let v e) s            = case evalExp e s of
                                    Right n  -> let s' = update v n s
                                                in Right (Skip :!: s')
                                    Left err -> Left err

stepComm (Seq Skip c1) s        = Right (c1 :!: s)

stepComm (Seq c0 c1) s          = case stepComm c0 s of
                                    Right (c0' :!: s') -> Right (Seq c0' c1 :!: s')
                                    Left err           -> Left err

stepComm (IfThenElse p c0 c1) s = case evalExp p s of
                                    Right b  -> if b then Right (c0 :!: s)
                                                     else Right (c1 :!: s)
                                    Left err -> Left err

stepComm r@(While b c) s       = let ite = IfThenElse b Skip r
                                  in Right (Seq c ite :!: s) -- esta mal para el primer caso

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error a 

evalExp (Const n) s     = Right n

evalExp (Var x) s       = case lookfor x s of
                            Right n  -> Right n
                            _        -> Left UndefVar

evalExp (UMinus e) s    = case evalExp e s of
                            Right n  -> Right (- n)
                            Left err -> Left err

evalExp (Plus e0 e1) s  = case evalExp e0 s of
                            Right n0 ->
                                case evalExp e1 s of
                                    Right n1 -> Right (n0 + n1)
                                    Left err -> Left err
                            Left err -> Left err

evalExp (Minus e0 e1) s = case evalExp e0 s of
                            Right n0 ->
                                case evalExp e1 s of
                                    Right n1 -> Right (n0 - n1)
                                    Left err -> Left err
                            Left err -> Left err

evalExp (Times e0 e1) s = case evalExp e0 s of
                            Right n0 ->
                                case evalExp e1 s of
                                    Right n1 -> Right (n0 * n1)
                                    Left err -> Left err
                            Left err -> Left err

evalExp (Div e0 e1) s   = case evalExp e0 s of
                            Right n0 ->
                                case evalExp e1 s of
                                    Right n1 ->
                                        if n1 == 0
                                            then Left DivByZero 
                                            else Right (div n0 n1)
                                    Left err -> Left err
                            Left err -> Left err
{-
evalExp (ECond p0 e0 e1) s = case evalExp p0 of
                              Right p1 ->
                                if p1
                                  then Right (evalExp e0 s)
                                  else Right (evalEXP e1 s)
                              Left err -> Left err
-}

evalExp BTrue s       = Right True

evalExp BFalse s      = Right False

evalExp (Lt e0 e1) s  = case evalExp e0 s of
                            Right n0 ->
                                case evalExp e1 s of
                                    Right n1 -> Right (n0 < n1)
                                    Left err -> Left err
                            Left err -> Left err

evalExp (Gt e0 e1) s  = case evalExp e0 s of
                            Right n0 ->
                                case evalExp e1 s of
                                    Right n1 -> Right (n0 > n1)
                                    Left err -> Left err
                            Left err -> Left err

evalExp (And p0 p1) s = case evalExp p0 s of
                            Right b0 ->
                                case evalExp p1 s of
                                    Right b1 -> Right (b0 && b1)
                                    Left err -> Left err
                            Left err -> Left err

evalExp (Or p0 p1) s  = case evalExp p0 s of
                            Right b0 ->
                                case evalExp p1 s of
                                    Right b1 -> Right (b0 || b1)
                                    Left err -> Left err
                            Left err -> Left err

evalExp (Not p) s     = case evalExp p s of
                            Right b  -> Right (not b)
                            Left err -> Left err

evalExp (Eq e0 e1) s  = case evalExp e0 s of
                            Right n0 ->
                                case evalExp e1 s of
                                    Right n1 -> Right (n0 == n1)
                                    Left err -> Left err
                            Left err -> Left err

evalExp (NEq e0 e1) s = case evalExp e0 s of
                            Right n0 ->
                                case evalExp e1 s of
                                    Right n1 -> Right (n0 /= n1)
                                    Left err -> Left err
                            Left err -> Left err
