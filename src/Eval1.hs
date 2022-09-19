module Eval1
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
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s                 = Skip :!: s

stepComm (Let v e) s            = let n   = evalExp e s
                                      s' = update v n s
                                  in (Skip :!: s')

stepComm (Seq Skip c1) s        = c1 :!: s

stepComm (Seq c0 c1) s          = let (c0' :!: s') = stepComm c0 s
                                  in (Seq c0' c1 :!: s')

stepComm (IfThenElse p c0 c1) s | b         = c0 :!: s
                                | otherwise = c1 :!: s
                                where b = evalExp p s

stepComm r@(While b c) s       = let ite = IfThenElse b (Seq c r) Skip
                                 in (ite :!: s)             

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> a 

evalExp (Const n) s     = n

evalExp (Var x) s       = lookfor x s

evalExp (UMinus e) s    = let n = evalExp e s
                          in (-n)

evalExp (Plus e0 e1) s  = let n0 = evalExp e0 s
                              n1 = evalExp e1 s
                          in (n0 + n1)

evalExp (Minus e0 e1) s = let n0 = evalExp e0 s
                              n1 = evalExp e1 s
                          in (n0 - n1)

evalExp (Times e0 e1) s = let n0 = evalExp e0 s
                              n1 = evalExp e1 s
                          in (n0 * n1)

evalExp (Div e0 e1) s   = let n0 = evalExp e0 s
                              n1 = evalExp e1 s
                          in (div n0 n1)

evalExp (ECond p0 e0 e1) s = let p1 = evalExp p0 s
                                 n = if p1 then evalExp e0 s 
                                            else evalExp e1 s
                             in n

evalExp BTrue s       = True

evalExp BFalse s      = False

evalExp (Lt e0 e1) s  = let n0  = evalExp e0 s
                            n1  = evalExp e1 s
                        in (n0 < n1)

evalExp (Gt e0 e1) s  = let n0  = evalExp e0 s
                            n1  = evalExp e1 s
                        in (n0 > n1)

evalExp (And p0 p1) s = let b0  = evalExp p0 s
                            b1  = evalExp p1 s
                        in (b0 && b1)

evalExp (Or p0 p1) s  = let b0  = evalExp p0 s
                            b1  = evalExp p1 s
                        in (b0 || b1)

evalExp (Not p) s     = let b = evalExp p s
                        in (not b)

evalExp (Eq e0 e1) s  = let n0  = evalExp e0 s
                            n1  = evalExp e1 s
                        in (n0 == n1)

evalExp (NEq e0 e1) s = let n0  = evalExp e0 s
                            n1  = evalExp e1 s
                        in (n0 /= n1)
