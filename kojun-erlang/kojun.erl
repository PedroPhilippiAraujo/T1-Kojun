% hello world program
-module(kojun).
-export([start/0]).

% Função auxiliar para substituir valor em uma lista na posição especificada
atualizaColuna(Lista, Indice, Valor) ->
  Lista_enumerada = lists:zip(lists:seq(1, length(Lista)), Lista),
  [if I == Indice -> Valor; true -> Val end || {I, Val} <- Lista_enumerada].

% Atualiza a matriz na posição {X, Y} com o novo valor
atualizaTabuleiro(Tab, {X, Y}, Valor_novo) ->
  Linha = lists:nth(X, Tab),
  Nova_Linha = atualizaColuna(Linha, Y, Valor_novo),
  atualizaColuna(Tab, X, Nova_Linha).

% Retorna a quantidade de células com o valor da região presente na coordenada {X, Y}
tamanhoRegiao(Reg, {X, Y}) ->
  Value = valorCoordenada(Reg, {X, Y}),
  length([C || R <- Reg, C <- R, C =:= Value]).

% Retorna o valor presente na coordenada {X, Y} na matriz
valorCoordenada(Matriz, {X, Y}) ->
  lists:nth(Y, lists:nth(X, Matriz)).

% Retorna lista de coordenadas cujo valor na matriz de valores seja igual a 0
coordenadasZero(Tab) ->
  Lim = length(Tab),
  Coordenadas = [{Row, Col} || Row <- lists:seq(1, Lim), Col <- lists:seq(1, Lim)],
  lists:filter(fun({X, Y}) -> valorCoordenada(Tab, {X, Y}) == 0 end, Coordenadas).

% Retorna uma lista de valores já presentes na região
valoresRegiao(Tab, Reg, R) ->
  Coordenadas = coordenadasRegiao(Reg, R),
  [valorCoordenada(Tab, {I, J}) || {I, J} <- Coordenadas].

% Retorna a lista de coordenadas que possuem o ID de região informado
coordenadasRegiao(Reg, Reg_id) ->
  Coordenadas = [{X, Y} || X <- lists:seq(1, length(Reg)), Y <- lists:seq(1, length(lists:nth(1, Reg)))],
  lists:filter(fun({X, Y}) -> valorCoordenada(Reg, {X, Y}) == Reg_id end, Coordenadas).

% Retorna uma lista de valores adjacentes da coordenada informada
celulasAdjacentes(Tab, {X, Y}) ->
  Acima = {X - 1, Y},
  Baixo = {X + 1, Y},
  Esquerda = {X, Y - 1},
  Direita = {X, Y + 1},
  Up = if X > 1 -> valorCoordenada(Tab, Acima); true -> 0 end,
  Down = if X < length(Tab) -> valorCoordenada(Tab, Baixo); true -> 0 end,
  Right = if Y < length(Tab) -> valorCoordenada(Tab, Direita); true -> 0 end,
  Left = if Y > 1 -> valorCoordenada(Tab, Esquerda); true -> 0 end,
  [Up, Down, Left, Right].

% Filtra lista para só possuir valores maiores que o valor da célula abaixo da coordenada informada
restricaoVerticalCima(Tab, Reg, {X, Y}, Prefiltro) ->
  if X == length(Tab) -> Prefiltro;
     true ->
       Baixo = {X + 1, Y},
       Regiao_id = valorCoordenada(Reg, {X, Y}),
       Regiao_id_abaixo = valorCoordenada(Reg, Baixo),
       Valor_abaixo = valorCoordenada(Tab, Baixo),
       if Regiao_id_abaixo /= Regiao_id -> Prefiltro;
        true -> lists:filter(fun(A) -> A > Valor_abaixo end, Prefiltro)
       end
  end.

% Filtra lista para só possuir valores menores que o valor da célula acima da coordenada informada
restricaoVerticalBaixo(Tab, Reg, {X, Y}, Prefiltro) ->
  if X == 1 -> Prefiltro;
     true ->
       Acima = {X - 1, Y},
       Regiao_id = valorCoordenada(Reg, {X, Y}),
       Regiao_id_acima = valorCoordenada(Reg, Acima),
       Valor_acima = valorCoordenada(Tab, Acima),
       if Regiao_id_acima /= Regiao_id orelse Valor_acima == 0 -> Prefiltro;
        true -> lists:filter(fun(A) -> A < Valor_acima end, Prefiltro)
       end
  end.

% Função unificadora para evitar ter que fazer duas chamadas na função restricoesPossibilidades
filtraRestricoesVerticals(Tab, Reg, {X, Y}, Prefiltro) ->
  FiltradoVertical1 = restricaoVerticalCima(Tab, Reg, {X, Y}, Prefiltro),
  restricaoVerticalBaixo(Tab, Reg, {X, Y}, FiltradoVertical1).

% Verifica se a célula é a menor solitária na região
menorSolitarioReg(Reg, {X, Y}) ->
  Regiao_id = valorCoordenada(Reg, {X, Y}),
  Regiao_id_abaixo = if X == length(Reg) -> -1; true -> valorCoordenada(Reg, {X + 1, Y}) end,
  Linha = lists:nth(X, Reg),
  Num_mesma_regiao_linha = length(lists:filter(fun(A) -> A == Regiao_id end, Linha)),
  Num_mesma_regiao_linha == 1 andalso Regiao_id /= Regiao_id_abaixo.

% Verifica se a célula é a maior solitária na região
maiorSolitarioReg(Reg, {X, Y}) ->
  Regiao_id = valorCoordenada(Reg, {X, Y}),
  Regiao_id_acima = if X == 1 -> -1; true -> valorCoordenada(Reg, {X - 1, Y}) end,
  Linha = lists:nth(X, Reg),
  Num_mesma_regiao_linha = length(lists:filter(fun(A) -> A == Regiao_id end, Linha)),
  Num_mesma_regiao_linha == 1 andalso Regiao_id /= Regiao_id_acima.

% Filtra a lista de possibilidades de acordo com as regras de menor e maior solitário
filtraSolitarioRegiao(Reg, {X, Y}, Prefiltro) ->
  Tamanho_Prefiltro = length(Prefiltro),
  IsLowestLonely = menorSolitarioReg(Reg, {X, Y}),
  IsHighestLonely = maiorSolitarioReg(Reg, {X, Y}),
  if Tamanho_Prefiltro > 1 andalso IsLowestLonely ->
       Valor_max = lists:max(Prefiltro),
       lists:subtract(Prefiltro, [Valor_max]);
     Tamanho_Prefiltro > 1 andalso IsHighestLonely ->
       Valor_min = lists:min(Prefiltro),
       lists:subtract(Prefiltro, [Valor_min]);
     true -> Prefiltro
  end.

% Retorna uma lista de todas as possibilidades para uma dada coordenada
restricaoPossibilidades(Tab, Reg, {X, Y}) ->
  Reg_id = valorCoordenada(Reg, {X, Y}),
  PossibilidadesBasicas = lists:seq(1, tamanhoRegiao(Reg, {X, Y})),
  PossibilidadesFiltrada1 = lists:subtract(PossibilidadesBasicas, valoresRegiao(Tab, Reg, Reg_id)),
  PossibilidadesFiltrada2 = lists:subtract(PossibilidadesFiltrada1, celulasAdjacentes(Tab, {X, Y})),
  PossibilidadesFiltrada3 = filtraRestricoesVerticals(Tab, Reg, {X, Y}, PossibilidadesFiltrada2),
  filtraSolitarioRegiao(Reg, {X, Y}, PossibilidadesFiltrada3).

% Chama valoresUnicos e realiza a atualização da matriz de valores caso existam valores únicos
possibilidadesUnicas(Tab, Possibilidades_Posicoes) ->
  Coords_e_valores_unicos = valoresUnicos(Possibilidades_Posicoes),
  if Coords_e_valores_unicos == [] -> Tab;
     true -> atualizaValoresComUnicos(Tab, Coords_e_valores_unicos)
  end.

% Função chamada pelo pre-solucionador para inicializar o contador da chamada recursiva
encontrarUnicaPossibilidade(Tab, Reg) ->
  Comeco_Count = lists:max(lists:max(Reg)),
  encontrarUnicaPossibilidade(Tab, Reg, Comeco_Count).

% Vasculha o tabuleiro em busca de algum valor que só aparece em um lugar da região
encontrarUnicaPossibilidade(Tab, Reg, Count) ->
  Possibilidades = valoresPossiveisDaRegiao(Tab, Reg, Count),
  Novo_tab = possibilidadesUnicas(Tab, Possibilidades),
  if Count == 1 -> Novo_tab;
     true -> encontrarUnicaPossibilidade(Novo_tab, Reg, Count - 1)
  end.

% Função auxiliar e recursiva para atualizar a tabela com valores únicos
atualizaValoresComUnicos(Tab, [{{X, Y}, Valor} | Resto]) ->
  Novo_tab = atualizaTabuleiro(Tab, {X, Y}, hd(Valor)),
  if Resto == [] -> Novo_tab;
     true -> atualizaValoresComUnicos(Novo_tab, Resto)
  end.

% Verifica se na lista de tuplas coordenadas, possibilidades existe algum valor de possibilidade que só apareça em uma coordenada
valoresUnicos(Lista_Coords_Poss) ->
  Lista_Possibilidades = lists:flatten([Poss || {_, Poss} <- Lista_Coords_Poss]),
  Lista_Unicos = lists:filter(fun(A) -> length(lists:filter(fun(B) -> B == A end, Lista_Possibilidades)) == 1 end, Lista_Possibilidades),
  coordsValoresUnicos(Lista_Coords_Poss, Lista_Unicos).

% Função auxiliar à valoresUnicos
coordsValoresUnicos(Lista_Coords_Poss, Lista_Unicos) ->
  Lista = lists:map(fun({{X, Y}, List}) ->
    Unicos = lists:filter(fun(Value) -> lists:member(Value, List) end, Lista_Unicos),
    if length(Unicos) == 1 -> {{X, Y}, Unicos}; true -> {} end
  end, Lista_Coords_Poss),
  lists:filter(fun(A) -> A /= {} end, Lista).

% Recebe uma lista com todas as coordenadas com valor == 0 e retorna uma matriz de possibilidades onde cada linha é a possibilidade de uma das coordenadas
possibilidadesVazios(_, _, [], Resultado) -> Resultado;
possibilidadesVazios(Tab, Reg, [{X, Y} | Resto], Resultado) ->
  Possibilidades_atual = restricaoPossibilidades(Tab, Reg, {X, Y}),
  possibilidadesVazios(Tab, Reg, Resto, Resultado ++ [Possibilidades_atual]).

% Função principal do Backtracking
testaPossibilidades(Tab, _, _, []) -> Tab;
testaPossibilidades(Tab, Reg, {X, Y}, [Possibilidade | Resto]) ->
  Novo_tab = atualizaTabuleiro(Tab, {X, Y}, Possibilidade),
  Coords_zero = coordenadasZero(Tab),
  Poss_vazios = possibilidadesVazios(Tab, Reg, Coords_zero, []),
  Novo_Coords_zero = coordenadasZero(Novo_tab),
  Novo_Poss_vazios = possibilidadesVazios(Novo_tab, Reg, Novo_Coords_zero, []),
  Tamanho_Novo_Poss = length(Novo_Poss_vazios),
  Tamanho_Poss = length(Poss_vazios),
  if Tamanho_Novo_Poss < (Tamanho_Poss - 1) ->
       testaPossibilidades(Tab, Reg, {X, Y}, Resto);
     true -> solucionador(Novo_tab, Reg)
  end.

% Retorna uma lista de tuplas de coordenadas e valores possíveis para dadas coordenadas de uma região
valoresPossiveisDaRegiao(Tab, Reg, Reg_id) ->
  Coordenadas = coordenadasRegiao(Reg, Reg_id),
  Coords_zeros = lists:filter(fun({X, Y}) -> valorCoordenada(Tab, {X, Y}) == 0 end, Coordenadas),
  [{Coord, restricaoPossibilidades(Tab, Reg, Coord)} || Coord <- Coords_zeros].

% Função auxiliar de solucionador. Aplica todas as regras e deduções implementadas
preSolucionador(Tab, Reg, [{X, Y} | Resto]) ->
  Novo_tab1 = encontrarUnicaPossibilidade(Tab, Reg),
  Possibilidades = restricaoPossibilidades(Novo_tab1, Reg, {X, Y}),
  Novo_Tab2 = if length(Possibilidades) == 1 -> atualizaTabuleiro(Novo_tab1, {X, Y}, hd(Possibilidades)); true -> Novo_tab1 end,
  if Resto == [] -> Novo_Tab2; true -> preSolucionador(Novo_Tab2, Reg, Resto) end.

% Função principal para solucionar o tabuleiro
solucionador(Tab, Reg) ->
  Coords_zero = coordenadasZero(Tab),
  if length(Coords_zero) == 0 -> Tab;
     true ->
       Novo_tab = preSolucionador(Tab, Reg, Coords_zero),
       if Novo_tab == Tab ->
          Novo_Coords_zero = coordenadasZero(Novo_tab),
          if length(Novo_Coords_zero) == 0 -> Novo_tab;
           true ->
             Novo_poss = restricaoPossibilidades(Tab, Reg, hd(Novo_Coords_zero)),
             testaPossibilidades(Tab, Reg, hd(Novo_Coords_zero), Novo_poss)
          end;
        true -> solucionador(Novo_tab, Reg)
       end
  end.

% Função para printar a matriz final
printTabuleiro(Matriz) ->
  Num_zeros = length(coordenadasZero(Matriz)),
  if Num_zeros > 0 -> io:format("TABULEIRO MAL FORMULADO. N° SOLUCOES /= 1 ");
     true -> lists:foreach(fun(Row) -> io:format("~p~n", [Row]) end, Matriz)
  end.

start() ->
  ValoresKojun = [
             [2,0,0,0,1,0],
             [0,0,0,3,0,0],
             [0,3,0,0,5,3],
             [0,0,0,0,0,0],
             [0,0,3,0,4,2],
             [0,0,0,0,0,0]
             ],

  RegioesKojun = [
               [1,1,2,2,2,3],
               [4,4,4,4,4,3],
               [5,6,6,6,4,7],
               [5,5,5,6,7,7],
               [8,8,10,0,0,0],
               [9,9,10,10,0,0]
               ],

  printTabuleiro(solucionador(ValoresKojun, RegioesKojun)).
