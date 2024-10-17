import Data.List

-- Exemplo tabuleiro questão 1
valoresKojun :: TabuleiroKojun
valoresKojun = [
             [2,0,0,0,1,0],
             [0,0,0,3,0,0],
             [0,3,0,0,5,3],
             [0,0,0,0,0,0],
             [0,0,3,0,4,2],
             [0,0,0,0,0,0]
             ]

regioesKojun :: TabuleiroKojun
regioesKojun = [
               [1,1,2,2,2,3],
               [4,4,4,4,4,3],
               [5,6,6,6,4,7],
               [5,5,5,6,7,7],
               [8,8,10,0,0,0],
               [9,9,10,10,0,0]
               ]

-- Definições de tipos
type Celula = Int
type Linha a = [a]
type Tabuleiro a = [Linha a]
type TabuleiroKojun = Tabuleiro Celula
type Possibilidades = [Celula]

-- Agrupa células por bloco
agruparPorRegiao :: Eq m => Tabuleiro m -> TabuleiroKojun -> Tabuleiro m
agruparPorRegiao valores regioes = [filtrarPorRegiao r valoresComRegioes | r <- regioesUnicas]
  where
    -- Combina valores e regiões em uma lista de pares
    valoresComRegioes = concat (zipWith zip valores regioes)
    -- Obtém as regiões únicas
    regioesUnicas = nub (map snd valoresComRegioes)
    -- Filtra os valores por região
    filtrarPorRegiao r lista = map fst $ filter ((== r) . snd) lista

-- Reconstrói colunas originais a partir de uma lista de regiões
reconstruirColunasOriginais :: [Linha l] -> Int -> [Linha l]
reconstruirColunasOriginais regioes n = dividirLista n (concat regioes)
  where
    -- Divide uma lista em sublistas de tamanho n
    dividirLista n = takeWhile (not . null) . map (take n) . iterate (drop n)

-- Funções específicas para Kojun

-- Calcula o tamanho de uma região específica
tamanhoRegiao :: Eq m => m -> Tabuleiro m -> Int
tamanhoRegiao _ [] = 0
tamanhoRegiao id regioes = sum [contar id linha | linha <- regioes]
  where 
    -- Conta quantas vezes um elemento aparece em uma lista
    contar x = length . filter (==x)

-- Obtém os valores presentes em uma região específica
valoresNaRegiao :: Eq m => Tabuleiro m -> TabuleiroKojun -> Int -> [m]
valoresNaRegiao valores regioes id = map fst $ filter ((==id) . snd) valoresComRegioes
  where
    valoresComRegioes = concat (zipWith zip valores regioes)

-- Remove possibilidades de uma lista de possibilidades
removerPossibilidades :: Possibilidades -> Possibilidades -> Possibilidades
xs `removerPossibilidades` ys = if unicoElemento xs then xs else xs \\ ys

-- Verifica se uma lista contém apenas um elemento
unicoElemento :: [a] -> Bool
unicoElemento [_] = True
unicoElemento _ = False

-- Obtém a dimensão (número de colunas) de um tabuleiro
obterDimensao :: Tabuleiro m -> Int
obterDimensao tab = length (head tab)

-- Obtém as linhas de um tabuleiro
obterLinhas :: Tabuleiro m -> [Linha m]
obterLinhas = id

-- Obtém as colunas de um tabuleiro
obterColunas :: Tabuleiro m -> [Linha m]
obterColunas = transpose

-- Gera as possibilidades iniciais para cada célula do tabuleiro Kojun
gerarPossibilidadesKojun :: TabuleiroKojun -> TabuleiroKojun -> Tabuleiro Possibilidades
gerarPossibilidadesKojun valores regioes = map (map gerar) (zipWith zip valores regioes)
  where 
    -- Gera as possibilidades para uma célula
    gerar (v, r) = if v == 0 then [1..(tamanhoRegiao r regioes)] `removerPossibilidades` (valoresNaRegiao valores regioes r) else [v]

-- Agrupa células por coluna e região
agruparPorColunaKojun :: Eq m => Tabuleiro m -> TabuleiroKojun -> [Linha m]
agruparPorColunaKojun valores regioes = zipWith zip (obterColunas valores) (obterColunas regioes) >>= map (map fst) . groupBy (\x y -> snd x == snd y)

-- Reduz as possibilidades para cada célula do tabuleiro Kojun
reduzirPossibilidadesKojun :: Tabuleiro Possibilidades -> TabuleiroKojun -> Tabuleiro Possibilidades
reduzirPossibilidadesKojun valores regioes = obterColunas $ reconstruirColunasOriginais (map reduzirPorLista (agruparPorColunaKojun valores regioes)) (obterDimensao valores)

-- Reduz as possibilidades em uma lista de possibilidades
reduzirPorLista :: Linha Possibilidades -> Linha Possibilidades
reduzirPorLista xss = [xs `removerPossibilidades` unicos | xs <- xss]
  where 
    -- Obtém os elementos únicos em uma lista de possibilidades
    unicos = concat (filter unicoElemento xss)

-- Busca a solução para o tabuleiro Kojun
buscarSolucaoKojun :: Tabuleiro Possibilidades -> TabuleiroKojun -> [TabuleiroKojun]
buscarSolucaoKojun valores regioes
  | naoTemSolucao valores regioes = []
  | all (all unicoElemento) valores = [map concat valores]
  | otherwise = [sol | valores' <- expandirPossibilidades valores, sol <- buscarSolucaoKojun (reduzirPossibilidadesKojun valores' regioes) regioes]

-- Verifica se não há solução para o tabuleiro Kojun
naoTemSolucao :: Tabuleiro Possibilidades -> TabuleiroKojun -> Bool
naoTemSolucao valores regioes = vazio valores || not (valido valores regioes)
  where
    -- Verifica se há células vazias
    vazio m = any (any null) m

-- Verifica se o tabuleiro é válido
valido :: Tabuleiro Possibilidades -> TabuleiroKojun -> Bool
valido valores regioes = all vizinhosValidos (obterColunas valores) &&
            all vizinhosValidos (obterLinhas valores) &&
            all linhaValida (agruparPorRegiao valores regioes) &&
            all colunaDecrescente (agruparPorColunaKojun valores regioes)

-- Verifica se os vizinhos são válidos
vizinhosValidos :: Eq a => Linha [a] -> Bool
vizinhosValidos [] = True
vizinhosValidos [_] = True
vizinhosValidos (a:b:bs)
  | length a <= 1 && length b <= 1 = if a == b then False else vizinhosValidos (b:bs)
  | otherwise = vizinhosValidos (b:bs)

-- Verifica se uma linha é válida
linhaValida :: Eq a => Linha [a] -> Bool
linhaValida [] = True
linhaValida (x:xs) = if length x <= 1 then x `notElem` xs && linhaValida xs else linhaValida xs

-- Verifica se uma coluna é decrescente
colunaDecrescente :: Ord a => Linha [a] -> Bool
colunaDecrescente [] = True
colunaDecrescente [_] = True
colunaDecrescente (a:b:bs)
  | length a <= 1 && length b <= 1 = if a < b then False else colunaDecrescente (b:bs)
  | otherwise = colunaDecrescente (b:bs)

-- Expande as possibilidades para encontrar a solução
expandirPossibilidades :: Tabuleiro Possibilidades -> [Tabuleiro Possibilidades]
expandirPossibilidades m = [linhas1 ++ [linha1 ++ [c] : linha2] ++ linhas2 | c <- cs]
  where
    (linhas1,linha:linhas2) = break (not . all unicoElemento) m
    (linha1,cs:linha2) = break (not . unicoElemento) linha

-- Resolve o tabuleiro Kojun
resolverKojun :: TabuleiroKojun -> TabuleiroKojun -> TabuleiroKojun
resolverKojun valores regioes = head (buscarSolucaoKojun (reduzirPossibilidadesKojun (gerarPossibilidadesKojun valores regioes) regioes) regioes)

main = do
  mapM_ print (resolverKojun valoresKojun regioesKojun)