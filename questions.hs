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
  | BinExp Op Expr Expr deriving (Eq)
-- dica: acrescente outros tipos de expressos abaixo: Add, Mul, Div             
data Op = 
    Add 
  | Sub 
  | Mul 
  | Div deriving (Eq) 



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

-- preserve as deficoes de Sizeable e suas instancias
-- para refletir: por que usamos essas definicoes ?
class Sizeable t where
 size :: t -> Integer

instance Sizeable Integer where
  size _ = 1

instance Sizeable Expr where
  size (Lit n) = 1
  size (BinExp _ e1 e2) = 1 + size e1 + size e2

instance (Sizeable k, Sizeable v) => Sizeable (k,v) where
  size (x,y) = size x + size y 

instance Sizeable e  => Sizeable [e] where
  size list = foldr (+) 0 (map size list) 



-- dica: lembre de acrescentar novos casos abaixo para novos tipos de expressoes
eval :: Expr -> Integer         
eval exp = case exp of
  Lit n -> n
  (BinExp Add e1 e2) -> eval e1 + eval e2 
  (BinExp Sub e1 e2) -> eval e1 - eval e2
  (BinExp Mul e1 e2) -> eval e1 * eval e2
  (BinExp Div e1 e2) -> eval e1 `div` eval e2
             
             
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


type MemoExprInt = [(Expr,Integer)]

evalM :: Expr -> MemoExprInt -> (Integer, MemoExprInt)
evalM  exp@(Lit n) m = (n,m)
evalM  exp  memo     = (valor, memoF) 
  where  
    (valor, memoF) = case lookupMemo exp memo  of
      Just v -> (v, memo)
      Nothing -> let valor' = (op exp) expLV expRV in
        (valor', updateMemo memo'' exp valor')                                         
    (expLV,memo') = evalM (left exp) memo
    (expRV,memo'')= evalM (right exp) memo'  
    -- dica: acrescente outros tipos de expressos abaixo: Add, Mul, Div             					   
    op e = case e of
      (BinExp Add _ _) -> (+)
      (BinExp Sub _ _) -> (-)
      (BinExp Mul _ _) -> (*)
      (BinExp Div _ _) -> (/)




-- exemplos de expressoes   
-- fique a vontade para criar outras com diferentes operadores                             
e1 = Lit 1
e2 = Lit 2
e12 =  BinExp Add e1 e2     
e122 = BinExp Add e12 e2       
memo = snd(evalM e122 [])   
e1122 = BinExp Add (Lit 1) e122
e22 = BinExp Add (e1122) (e122)              

sub1 = BinExp Sub e1 e2
mul1 = BinExp Mul e2 e2
      
-- funcao de Fibonacci original
fib :: Integer -> Integer 
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- versao da funcao de Fibonacci memoizada 
type MemoIntInt = [(Integer,Integer)]
fibM :: Integer -> MemoIntInt -> (Integer,MemoIntInt)
fibM = undefined


type CustoMemoria = Integer
type CustoOperacoes = Integer
type FracaoOperacoes = Float
custoEvalM :: Expr -> MemoExprInt -> (FracaoOperacoes,CustoMemoria)
custoEvalM exp memo =  let (_,c)   = eval' exp 
                           (_,memo',cM) = evalM' exp memo in
                        ((fromIntegral cM) / (fromIntegral c), size memo')


evalM' :: Expr -> MemoExprInt -> (Integer, MemoExprInt, CustoOperacoes)
evalM'  exp@(Lit n) m = (n,m,0)
evalM'  exp  memo     = (valor, memoF,custo) 
  where  
    (valor, memoF,custo) = case lookupMemo exp memo  of
      Just v -> (v, memo,0)
      Nothing -> let valor' = (op exp) expLV expRV in
        (valor', 
        updateMemo memo'' exp valor',
        1 + custo' + custo'')                                         
    (expLV,memo',custo') = evalM' (left exp) memo
    (expRV,memo'',custo'')= evalM' (right exp) memo'  
    op e = case e of
      (BinExp Add _ _) -> (+)

eval' :: Expr -> (Integer,CustoOperacoes)         
eval' exp = case exp of
    Lit n -> (n,0)
    _ -> (valor,  1 + custo' + custo'')
  where 
    valor = (op exp) expLV expRV
    (expLV,custo') = eval' (left exp) 
    (expRV,custo'')= eval' (right exp)
    op e = case e of
      (BinExp Add _ _) -> (+)
                      



custoFiboM :: Integer -> MemoIntInt -> (FracaoOperacoes,CustoMemoria)      
custoFiboM = undefined
