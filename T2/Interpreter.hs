{- Programacao Funcional - Trabalho 2 - 
   Prazo de entrega: 19/02/2023 por Tarefa no Aprender 3
   O trabalho deve ser feito individualmente

  ** ENUNCIADO DO TRABALHO:   
 Evolua o interpretador abaixo para prover avaliacao preguicosa (lazy evaluation).
 Ao fim do codigo abaixo, ha alguns casos de testes. 
 Sugere-se observar as dicas neste arquivo e no AbsLI.hs.
 
 Criterios de avaliacao: 
   1) testCaseSuiteResults deve ser computavel e ser True 
   2) para nota maior ou igual a 9, deve-se usar SYB.

-}

module Interpreter where

import AbsLI
import Prelude 
import Data.Maybe
import Memo


type Context k v = [(k,v)]                                
type RContext = (VContext, FContext)
type VContext = Context Ident Integer
type FContext = Context Ident Function

-- remove variables from the expression
removeVar :: VContext -> Exp -> Exp
removeVar context x = case x of
  EAdd expR expL    -> EAdd (removeVar context expR) (removeVar context expL)
  ESub expR expL    -> ESub (removeVar context expR) (removeVar context expL)
  EMul expR expL    -> EMul (removeVar context expR) (removeVar context expL)
  EDiv expR expL    -> EDiv (removeVar context expR) (removeVar context expL)
  EInt n            -> EInt n

  EVar name         -> removeVar context ( fromJust (lookupMemo name context))

  EIf e1 e2 e3      -> EIf (removeVar context e1) (removeVar context e2) (removeVar context e3)

  Call func params  -> Call func (map (removeVar context) params)



evalP :: Program -> Integer
evalP (Prog fs) = resp where
  ( resp, _ ) = eval ([],(updatecF [] fs)) (Call (Ident "main") [])   

eval :: RContext -> Exp -> ( Integer, RContext )
eval context x = case x of
  EAdd expL expR  -> ( val, newCont ) where
    ( valL, contL ) = eval context expL
    ( valR, contR ) = eval contL expR
    val = valL + valR
    newCont = contR
  ESub expL expR  -> ( val, newCont ) where
    ( valL, contL ) = eval context expL
    ( valR, contR ) = eval contL expR
    val = valL - valR
    newCont = contR
  EMul expL expR  -> ( val, newCont ) where
    ( valL, contL ) = eval context expL
    ( valR, contR ) = eval contL expR
    val = valL * valR
    newCont = contR
  EDiv expL expR  -> ( val, newCont ) where
    ( valL, contL ) = eval context expL
    ( valR, contR ) = eval contL expR
    val = valL `div` valR
    newCont = contR
  EInt n         -> ( n, context )


  -- dica: considere alteracao na alternativa abaixo
  EVar id        -> (val, newCont) where
    varExp = fromJust (lookupMemo id (fst context))
    (val, exprCont) = eval context varExp
    (vContext, fContext) = exprCont
    newVarCont = updateMemo vContext id (EInt val)
    newCont = (newVarCont, fContext)

  EIf e1 e2 e3   -> (val, newCont) where
    (cond, condCont) = (eval context e1)
    (val, newCont) = if (cond /= 0)
      then (eval condCont e2)
      else (eval condCont e3)


  -- dica: considere alteracao na alternativa abaixo                  
  Call func params -> (val, newCont) where
    ( prevVCont, prevFCont ) = context
    Fun _ funcParams funcExp = fromJust ( lookupMemo func prevFCont )
    paramBindings = zip funcParams ( map ( removeVar prevVCont ) params)
    ( val, newCont ) = eval ( paramBindings, prevFCont ) funcExp
  

updatecF :: FContext -> [Function] -> FContext
updatecF ctx [] = ctx
updatecF ctx (f@(Fun id _ _):fs) = updatecF (updateMemo ctx id f) fs


{-
  main () {
    fat (5)
  }
  
  fat (n) {
    if (n) 
      then n * fat (n - 1) 
    else 1
  }
-}

fat = 
  Prog [
    Fun (Ident "main") [] (Call (Ident "fat") [EInt 5]),

    Fun (Ident "fat") [Ident "n"] (
      EIf (EVar (Ident "n"))
        (EMul (EVar (Ident "n"))
        (Call (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)]))
      (EInt 1)
    )
  ]

testCaseFat = evalP fat == 120


{-
 main () {
   fib (8)
}
 fib (n) {
   if (n) then 
      if (n - 1) 
        then fib (n - 1) + fib (n - 2) 
        else 1 
    else 1
}
-}

fibo =  
  Prog [
    Fun (Ident "main") [] (
      Call (Ident "fib") [EInt 8]
    ),
    Fun (Ident "fib") [Ident "n"] (
      EIf (EVar (Ident "n")) 
        (EIf (ESub (EVar (Ident "n")) (EInt 1)) 
          (EAdd (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 1)]) 
          (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 2)])) 
        (EInt 1)) 
      (EInt 1)
    )
  ]

testCaseFibo = evalP fibo == 34

-- testCaseSuiteResults deve ser true 
testCaseSuiteResults  = testCaseFat && testCaseFibo

