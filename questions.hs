{-
* Trabalho 1. Entrega: até dia 29/01/2023 às 23:59h por Atividade no Aprender 3
  (detalhes a serem fornecidos pelo professor)

* O trabalho deve ser feito individualmente. 

* O trabalho tem 3 questões e vale 10 pontos.

* Questão 1(4,0 pontos). Evolua a implementação da função evalM para lidar com outros tipos 
  de expressões, de forma a tratar não apenas expressões de soma, mas também expressões de 
  Subtração, multiplicação, e divisão. Observe as dicas fornecidas em comentário. 
  Evite fazer replicação de código.


* Questão 2 (2,0 pontos).  Adapte a função evalM para computar a função de Fibonacci de 
  forma memoizada (fibM).


* Questão 3 (4,0 pontos). A função custoEvalM avalia a complexidade da avaliação memoizada, 
retornando a fração número de operações aritméticas realizadas na versão memoizada em relação
à versão não memoizada (número menor do que 1 significa ganho na memoização) e um inteiro 
indicando o tamanho da memória. Para tanto, a função custoEvalM depende das funções
eval' e evalM', que são extensões das funções eval e evalM, respectivamente, computando o 
custo das operações. Faça o mesmo para definir a função custoFiboM, também retornando o
custo da memória. 

-}
          

import Data.Maybe

data Expr = 
  Lit Integer 
  | UnExp UnOp Expr 
  | BinExp BinOp Expr Expr deriving (Eq)

data UnOp =
  Fib deriving (Eq)

data BinOp = 
  Add |
  Sub |
  Mul |
  Div deriving (Eq) 

calcBinOp :: BinOp -> Integer -> Integer -> Integer
calcBinOp Add a b = a + b
calcBinOp Sub a b = a - b
calcBinOp Mul a b = a * b
calcBinOp Div a b = a `div` b

calcUnOp :: UnOp -> Integer -> Integer
calcUnOp Fib a = result where (result, _) = fibM a emptyMemo

-- preserve as deficoes de left e right
left :: Expr -> Expr
left (BinExp _ e1 _) = e1
right :: Expr -> Expr
right (BinExp _ _ e2) = e2 

-- dica: lembre de acrescentar novos casos abaixo para novos tipos de expressoes
instance Show Expr where
  show (Lit n) = show n
  show (BinExp Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (BinExp Sub e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
  show (BinExp Mul e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
  show (BinExp Div e1 e2) = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"
  show (UnExp Fib e) = "Fibonacci(" ++ show e ++ ")"

-- preserve as deficoes de Sizeable e suas instancias
-- para refletir: por que usamos essas definicoes ?
class Sizeable t where
  size :: t -> Integer

instance Sizeable Integer where
  size _ = 1

instance Sizeable Expr where
  size (Lit n) = 1
  size (BinExp _ e1 e2) = 1 + size e1 + size e2
  size (UnExp _ e) = 1 + size e

instance (Sizeable k, Sizeable v) => Sizeable (k,v) where
  size (x,y) = size x + size y 

instance Sizeable e  => Sizeable [e] where
  size list = foldr (+) 0 (map size list) 

-- dica: lembre de acrescentar novos casos abaixo para novos tipos de expressoes
eval :: Expr -> Integer         
eval exp = case exp of
  Lit n -> n
  (BinExp op e1 e2) -> calcBinOp op (eval e1) (eval e2) 
  (UnExp op e) -> calcUnOp op (eval e)
        
-- preserve as definicoes da memoria
emptyMemo = []
lookupMemo :: Eq k => k -> [(k,v)] -> Maybe v
lookupMemo _  [] = Nothing
lookupMemo key ((k,v):kvs)
  | k == key = Just v
  | otherwise = lookupMemo key kvs 
  
updateMemo ::  Eq k => [(k,v)] -> k  -> v -> [(k,v)]
updateMemo [] key value = [(key,value)]
updateMemo ((k,v):kvs) key newValue
  | k == key = (k,newValue):kvs
  | otherwise = (k,v):updateMemo kvs key newValue

useMemo :: Eq k => (k -> [(k, v)] -> (v, [(k, v)])) -> (k -> [(k, v)] -> (v, [(k, v)]))
useMemo foo = \key memo -> case lookupMemo key memo of
  Just valor -> (valor, memo)
  Nothing -> (result, finalMemo) where
    (result, tempMemo) = foo key memo
    finalMemo = updateMemo tempMemo key result

type MemoExprInt = [(Expr,Integer)]

evalM :: Expr -> MemoExprInt -> (Integer, MemoExprInt)
evalM (Lit n) m = (n,m)
evalM e m = useMemo (\exp memo -> case exp of
    BinExp op a b -> (result, bMemo) where
      (aResult, aMemo) = evalM a memo
      (bResult, bMemo) = evalM b aMemo
      result = calcBinOp op aResult bResult
    UnExp op a -> (result, aMemo) where
      (aResult, aMemo) = evalM a memo
      result = calcUnOp op aResult
  ) e m

-- funcao de Fibonacci original
fib :: Integer -> Integer 
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- versao da funcao de Fibonacci memoizada 
type MemoIntInt = [(Integer,Integer)]
fibM :: Integer -> MemoIntInt -> (Integer,MemoIntInt)
fibM 0 memo = (0, memo)
fibM 1 memo = (1, memo)
fibM n m = useMemo (\num memo ->
      let
        (fibNumMinus2, memo_1) = fibM (num-2) memo
        (fibNumMinus1, memo_2) = fibM (num-1) memo_1        
        result = fibNumMinus1 + fibNumMinus2
      in (result, memo_2)
  ) n m

type CustoMemoria = Integer
type CustoOperacoes = Integer
type FracaoOperacoes = Float
custoEvalM :: Expr -> MemoExprInt -> (FracaoOperacoes,CustoMemoria)
custoEvalM exp memo = let
  (_,c) = eval' exp 
  (_,memo',cM) = evalM' exp memo
  in ((fromIntegral cM) / (fromIntegral c), size memo')


evalM' :: Expr -> MemoExprInt -> (Integer, MemoExprInt, CustoOperacoes)
evalM'  exp@(Lit n) m = (n,m,0)
evalM'  exp@(UnExp op innerExp) memo = case lookupMemo exp memo  of
  Just v -> (v, memo, 0)
  Nothing -> (result, newMemo, custo) where
    (expValue, exprMemo, custoExp) = evalM' innerExp memo
    custo = custoExp + 1
    result = calcUnOp op expValue
    newMemo = updateMemo exprMemo exp expValue

evalM'  exp@(BinExp op expL expR)  memo     = (valor, memoF,custo) 
  where  
    (valor, memoF,custo) = case lookupMemo exp memo  of
      Just v -> (v, memo,0)
      Nothing -> let valor' = calcBinOp op expLV expRV in
        (valor', 
        updateMemo memo'' exp valor',
        1 + custo' + custo'')                                         
    (expLV,memo',custo') = evalM' (left exp) memo
    (expRV,memo'',custo'')= evalM' (right exp) memo'

eval' :: Expr -> (Integer,CustoOperacoes)
eval' (Lit n) = (n, 0)
eval' (UnExp op innerExp) = (valor, custo) where
  (innerVal, innerCusto) = eval' innerExp
  valor = calcUnOp op innerVal
  custo = 1 + innerCusto
eval' (BinExp op lExp rExp) = (valor, custo) where
  (lValor, lCusto) = eval' lExp       
  (rValor, rCusto) = eval' rExp
  valor = calcBinOp op lValor rValor
  custo = 1 + lCusto + rCusto

custoFibo :: Integer -> Integer
custoFibo 0 = 0
custoFibo 1 = 0
custoFibo n = custoFibo (n-1) + custoFibo (n-2) + 1

-- 0 e 1 tem valores _hardcoded_, então o custo é 0. para qualquer outro n tem que calcular fib (n-1), fib(n-2) e fazer a soma. fazendo o cálculo de fib(n-1) todas as contas de fib(n-2) ficam memoizadas, então o custo é igual a 1 (fib(n-1) + fib(n-2)) + o custo de calcular fib(n-1).
-- O custo para qualquer n > 1 então é sempre n-1 
custoExecFiboM :: Integer -> Integer
custoExecFiboM 0 = 0
custoExecFiboM 1 = 0
custoExecFiboM n = 1 + custoExecFiboM (n-1)

-- de forma similar ao custo de execução, o custo de memória de fib(n) é igual ao custo de memória de fib(n-1) + custo de memória de fib(n-2) + 1, mas como todos os indices de memória de fib(n-2) também são compartilhados por fib(n-1), fib(n-2) não adiciona nenhum custo de memória, e o custo final é igual a custo(fib(n-1)) + 1. 
-- O custo para qualquer n > 1 então é sempre n-1 
custoMemoFiboM :: Integer -> Integer
custoMemoFiboM 0 = 0
custoMemoFiboM 1 = 0
custoMemoFiboM n = 1 + custoMemoFiboM (n-1)

custoFiboM :: Integer -> MemoIntInt -> (FracaoOperacoes,CustoMemoria)      
custoFiboM n m = ((fromIntegral (custoExecFiboM n)) / (fromIntegral (custoFibo n)), custoMemoFiboM n)
