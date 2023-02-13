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
import Data.Maybe
import Memo
import Prelude
import Debug.Trace

-- import qualified Data.Type.Bool as valor

type Context k v = [(k, v)]

type RContext = (VContext, FContext)

-- type VContext = Context Ident Integer
type VContext = Context Ident Exp

type FContext = Context Ident Function

evalP :: Program -> Integer
evalP (Prog fs) = result where
  (result, _) = evalC ([], (updatecF [] fs)) (Call (Ident "main") [])

{-
eval :: RContext -> Exp -> Integer
eval context x = case x of
  EAdd exp0 exp -> eval context exp0 + eval context exp
  ESub exp0 exp -> eval context exp0 - eval context exp
  EMul exp0 exp -> eval context exp0 * eval context exp
  EDiv exp0 exp -> eval context exp0 `div` eval context exp
  EInt n -> n
  -- dica: considere alteracao na alternativa abaixo
  -- IDEIA: mudar context de variável pra salvar expression e não valor.(&&)
  --   aí aqui embaixo a gente ia ter que avaliar a expressão antes de retornar
  EVar id -> fromJust (lookupMemo id (fst context))
  EIf e1 e2 e3 ->
    if (eval context e1 /= 0)
      then (eval context e2)
      else (eval context e3)
  -- dica: considere alteracao na alternativa abaixo
  -- IDEIA: e aqui da mesma forma a gente tem que mudar função pra receber expressão não valores
  --   e a gente aqui só passa a expressão, e a função avalia ela quando/se necessário. 
  Call id lexp -> eval (paramBindings, contextFunctions) exp
    where
      Fun _ decls exp = fromJust (lookupMemo id (snd context))
      paramBindings = zip decls (map (eval context) lexp)
      contextFunctions = snd context
-}

identStr :: Ident -> String
identStr (Ident val) = val

removeContext :: VContext -> Exp -> Exp
removeContext context x = let noContext = removeContext context in case x of
  EAdd expL expR -> EAdd (noContext expL) (noContext expR)
  ESub expL expR -> ESub (noContext expL) (noContext expR)
  EDiv expL expR -> EDiv (noContext expL) (noContext expR)
  EMul expL expR -> EMul (noContext expL) (noContext expR)
  Call funcId paramExps -> Call funcId (map noContext paramExps)
  EIf expCond expThen expElse -> EIf (noContext expCond) (noContext expThen) (noContext expElse)
  EInt val -> EInt val
  EVar varName -> noContext (fromJust (lookupMemo varName context))

evalC :: RContext -> Exp -> (Integer, RContext)
evalC context x = case x of
  EAdd expL expR -> (value, newContext) where
    (valueL, contextL) = evalC context expL 
    (valueR, contextR) = evalC contextL expR
    value = valueL + valueR
    newContext = contextR
  ESub expL expR -> (value, newContext) where
    (valueL, contextL) = evalC context expL 
    (valueR, contextR) = evalC contextL expR
    value = valueL - valueR
    newContext = contextR
  EMul expL expR -> (value, newContext) where
    (valueL, contextL) = evalC context expL 
    (valueR, contextR) = evalC contextL expR
    value = valueL * valueR
    newContext = contextR
  EDiv expL expR -> (value, newContext) where
    (valueL, contextL) = evalC context expL 
    (valueR, contextR) = evalC contextL expR
    value = valueL `div` valueR
    newContext = contextR
  EInt n -> (n, context)
  EVar id -> (value, newContext) where
    varExp = fromJust (lookupMemo id (fst context))
    (value, exprContext) = evalC context varExp
    (vContext, fContext) = exprContext
    newVarContext = updateMemo vContext id (EInt value)
    newContext = (newVarContext, fContext)
  Call funcId paramExps -> (value, newContext) where
    (oldVContext, oldFContext) = context
    Fun _ funcParams funcExp = fromJust (lookupMemo funcId oldFContext)
    paramBindings = zip funcParams (map (removeContext oldVContext) paramExps)
    (value, newContext) = evalC (paramBindings, oldFContext) funcExp 
  EIf eCond eThen eElse -> (value, newContext) where
    (condVal, condContext) = evalC context eCond
    (value, newContext) = if (condVal /= 0)
      then (evalC condContext eThen)
      else (evalC condContext eElse)

updatecF :: FContext -> [Function] -> FContext
updatecF ctx [] = ctx
updatecF ctx (f@(Fun id _ _) : fs) = updatecF (updateMemo ctx id f) fs

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
  Prog
    [ Fun (Ident "main") [] (Call (Ident "fat") [EInt 5]),
      Fun
        (Ident "fat")
        [Ident "n"]
        ( EIf
            (EVar (Ident "n"))
            ( EMul
                (EVar (Ident "n"))
                (Call (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)])
            )
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
  Prog
    [ Fun (Ident "main") [] (Call (Ident "fib") [EInt 8]),
      Fun
        (Ident "fib")
        [Ident "n"]
        ( EIf
            (EVar (Ident "n"))
            ( EIf
                (ESub (EVar (Ident "n")) (EInt 1))
                ( EAdd
                    (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 1)])
                    (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 2)])
                )
                (EInt 1)
            )
            (EInt 1)
        )
    ]

testCaseFibo = evalP fibo == 34

-- testCaseSuiteResults deve ser true
testCaseSuiteResults = testCaseFat && testCaseFibo
