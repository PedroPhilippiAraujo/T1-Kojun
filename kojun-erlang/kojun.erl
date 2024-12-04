-module(kojun).
-export([start/0]).

% Atualiza a matriz na posição {X, Y} com o novo valor
atualizar_valor_na_matriz(Matriz, {X, Y}, NovoValor) ->
  Linha = lists:nth(X, Matriz),
  NovaLinha = substituir_valor_na_lista(Linha, Y, NovoValor),
  substituir_valor_na_lista(Matriz, X, NovaLinha).

% Função auxiliar para substituir um valor em uma lista na posição especificada
substituir_valor_na_lista(Lista, Indice, Valor) ->
  ListaEnumerada = lists:zip(lists:seq(1, length(Lista)), Lista),
  [if I == Indice -> Valor; true -> Val end || {I, Val} <- ListaEnumerada].

% Retorna a quantidade de células disponíveis na região para um dado valor
contar_celulas_na_regiao(Regiao, {X, Y}) ->
  Valor = obter_valor_na_matriz(Regiao, {X, Y}),
  length([C || R <- Regiao, C <- R, C =:= Valor]).

% Retorna o valor presente na coordenada {X, Y} na matriz
obter_valor_na_matriz(Matriz, {X, Y}) ->
  lists:nth(Y, lists:nth(X, Matriz)).

% Retorna a lista de coordenadas vazias na matriz a ser resolvida
coordenadas_vazias_na_matriz(Matriz) ->
  Limite = length(Matriz),
  Coordenadas = [{Linha, Coluna} || Linha <- lists:seq(1, Limite), Coluna <- lists:seq(1, Limite)],
  lists:filter(fun({X, Y}) -> obter_valor_na_matriz(Matriz, {X, Y}) == 0 end, Coordenadas).

% Retorna uma lista dos valores que já estão preenchidos na região
valores_presentes_na_regiao(Matriz, Regiao, IdRegiao) ->
  Coordenadas = coordenadas_da_regiao(Regiao, IdRegiao),
  [obter_valor_na_matriz(Matriz, {I, J}) || {I, J} <- Coordenadas].

% Retorna a lista de coordenadas de uma região com base no valor da região
coordenadas_da_regiao(Regiao, IdRegiao) ->
  Coordenadas = [{X, Y} || X <- lists:seq(1, length(Regiao)), Y <- lists:seq(1, length(lists:nth(1, Regiao)))],
  lists:filter(fun({X, Y}) -> obter_valor_na_matriz(Regiao, {X, Y}) == IdRegiao end, Coordenadas).

% Retorna as celulas adjacentes à coordenada {X, Y}
valores_adjacentes_na_matriz(Matriz, {X, Y}) ->
  Superior = {X - 1, Y},
  Inferior = {X + 1, Y},
  Esquerda = {X, Y - 1},
  Direita = {X, Y + 1},
  ValorSuperior = if X > 1 -> obter_valor_na_matriz(Matriz, Superior); true -> 0 end,
  ValorInferior = if X < length(Matriz) -> obter_valor_na_matriz(Matriz, Inferior); true -> 0 end,
  ValorDireita = if Y < length(Matriz) -> obter_valor_na_matriz(Matriz, Direita); true -> 0 end,
  ValorEsquerda = if Y > 1 -> obter_valor_na_matriz(Matriz, Esquerda); true -> 0 end,
  [ValorSuperior, ValorInferior, ValorEsquerda, ValorDireita].

% Filtra para que a lista possua apenas valores maiores que os passados em {X, Y}
filtrar_restricao_vertical_superior(Matriz, Regiao, {X, Y}, Prefiltro) ->
  if X == length(Matriz) -> Prefiltro;
     true ->
       Inferior = {X + 1, Y},
       IdRegiao = obter_valor_na_matriz(Regiao, {X, Y}),
       IdRegiaoInferior = obter_valor_na_matriz(Regiao, Inferior),
       ValorInferior = obter_valor_na_matriz(Matriz, Inferior),
       if IdRegiaoInferior /= IdRegiao -> Prefiltro;
        true -> lists:filter(fun(A) -> A > ValorInferior end, Prefiltro)
       end
  end.

%  Filtra para que a lista possua apenas valores menores que os passados em {X, Y}
filtrar_restricao_vertical_inferior(Matriz, Regiao, {X, Y}, Prefiltro) ->
  if X == 1 -> Prefiltro;
     true ->
       Superior = {X - 1, Y},
       IdRegiao = obter_valor_na_matriz(Regiao, {X, Y}),
       IdRegiaoSuperior = obter_valor_na_matriz(Regiao, Superior),
       ValorSuperior = obter_valor_na_matriz(Matriz, Superior),
       if IdRegiaoSuperior /= IdRegiao orelse ValorSuperior == 0 -> Prefiltro;
        true -> lists:filter(fun(A) -> A < ValorSuperior end, Prefiltro)
       end
  end.

% Resolve as restrições verticais com base nas funções anteriores
filtrar_restricoes_verticais(Matriz, Regiao, {X, Y}, Prefiltro) ->
  FiltradoVertical1 = filtrar_restricao_vertical_superior(Matriz, Regiao, {X, Y}, Prefiltro),
  filtrar_restricao_vertical_inferior(Matriz, Regiao, {X, Y}, FiltradoVertical1).

% Verifica se célula é o menor valor único de uma região
verificar_menor_unico_na_regiao(Regiao, {X, Y}) ->
  IdRegiao = obter_valor_na_matriz(Regiao, {X, Y}),
  IdRegiaoInferior = if X == length(Regiao) -> -1; true -> obter_valor_na_matriz(Regiao, {X + 1, Y}) end,
  Linha = lists:nth(X, Regiao),
  NumMesmaRegiaoLinha = length(lists:filter(fun(A) -> A == IdRegiao end, Linha)),
  NumMesmaRegiaoLinha == 1 andalso IdRegiao /= IdRegiaoInferior.

% Verifica se a célula é o maior valor unico de uma região
verificar_maior_unico_na_regiao(Regiao, {X, Y}) ->
  IdRegiao = obter_valor_na_matriz(Regiao, {X, Y}),
  IdRegiaoSuperior = if X == 1 -> -1; true -> obter_valor_na_matriz(Regiao, {X - 1, Y}) end,
  Linha = lists:nth(X, Regiao),
  NumMesmaRegiaoLinha = length(lists:filter(fun(A) -> A == IdRegiao end, Linha)),
  NumMesmaRegiaoLinha == 1 andalso IdRegiao /= IdRegiaoSuperior.

% Filtra a lista de possibilidades de acordo com as regras de menor e maior solitário
filtrar_unicos_na_regiao(Regiao, {X, Y}, Prefiltro) ->
  TamanhoPrefiltro = length(Prefiltro),
  MenorUnico = verificar_menor_unico_na_regiao(Regiao, {X, Y}),
  MaiorUnico = verificar_maior_unico_na_regiao(Regiao, {X, Y}),
  if TamanhoPrefiltro > 1 andalso MenorUnico ->
       ValorMax = lists:max(Prefiltro),
       lists:subtract(Prefiltro, [ValorMax]);
     TamanhoPrefiltro > 1 andalso MaiorUnico ->
       ValorMin = lists:min(Prefiltro),
       lists:subtract(Prefiltro, [ValorMin]);
     true -> Prefiltro
  end.

% Retorna uma lista com os valores possíveis para a coordenada {X, Y}
obter_possibilidades_para_coordenada(Matriz, Regiao, {X, Y}) ->
  IdRegiao = obter_valor_na_matriz(Regiao, {X, Y}),
  PossibilidadesBasicas = lists:seq(1, contar_celulas_na_regiao(Regiao, {X, Y})),
  PossibilidadesFiltrada1 = lists:subtract(PossibilidadesBasicas, valores_presentes_na_regiao(Matriz, Regiao, IdRegiao)),
  PossibilidadesFiltrada2 = lists:subtract(PossibilidadesFiltrada1, valores_adjacentes_na_matriz(Matriz, {X, Y})),
  PossibilidadesFiltrada3 = filtrar_restricoes_verticais(Matriz, Regiao, {X, Y}, PossibilidadesFiltrada2),
  filtrar_unicos_na_regiao(Regiao, {X, Y}, PossibilidadesFiltrada3).

% Atualiza a matriz de inicial com os valores únicos encontrados
atualizar_matriz_com_valores_unicos(Matriz, PossibilidadesPosicoes) ->
  CoordsEValoresUnicos = encontrar_valores_unicos(PossibilidadesPosicoes),
  if CoordsEValoresUnicos == [] -> Matriz;
     true -> atualizar_matriz_com_unicos(Matriz, CoordsEValoresUnicos)
  end.

% Inicializa o contador para as chamadas recurivas, através do valor máximo da região
inicializar_contador_para_unica_possibilidade(Matriz, Regiao) ->
  InicioContador = lists:max(lists:max(Regiao)),
  encontrar_unica_possibilidade(Matriz, Regiao, InicioContador).

% Procura no tabuleiro por um valor unico na região
encontrar_unica_possibilidade(Matriz, Regiao, Contador) ->
  Possibilidades = valores_possiveis_na_regiao(Matriz, Regiao, Contador),
  NovaMatriz = atualizar_matriz_com_valores_unicos(Matriz, Possibilidades),
  if Contador == 1 -> NovaMatriz;
     true -> encontrar_unica_possibilidade(NovaMatriz, Regiao, Contador - 1)
  end.

%Função recursiva para atualizar a tabela
atualizar_matriz_com_unicos(Matriz, [{{X, Y}, Valor} | Resto]) ->
  NovaMatriz = atualizar_valor_na_matriz(Matriz, {X, Y}, hd(Valor)),
  if Resto == [] -> NovaMatriz;
     true -> atualizar_matriz_com_unicos(NovaMatriz, Resto)
  end.

% Checa se na lista de tuplas (coordenadas, possibilidades) se há algum valor de possibilidade que apareça apenas em uma coordenada
encontrar_valores_unicos(ListaCoordsPoss) ->
  ListaPossibilidades = lists:flatten([Poss || {_, Poss} <- ListaCoordsPoss]),
  ListaUnicos = lists:filter(fun(A) -> length(lists:filter(fun(B) -> B == A end, ListaPossibilidades)) == 1 end, ListaPossibilidades),
  coordenadas_com_valores_unicos(ListaCoordsPoss, ListaUnicos).

coordenadas_com_valores_unicos(ListaCoordsPoss, ListaUnicos) ->
  Lista = lists:map(fun({{X, Y}, List}) ->
    Unicos = lists:filter(fun(Value) -> lists:member(Value, List) end, ListaUnicos),
    if length(Unicos) == 1 -> {{X, Y}, Unicos}; true -> {} end
  end, ListaCoordsPoss),
  lists:filter(fun(A) -> A /= {} end, Lista).

% Recebe uma lista com todas as coordenadas vazias e retorna uma matriz com todas as possibilidades para cada coordenada, onde cada linha é uma lista de possibilidades de uma coordenada
gerar_possibilidades_para_vazios(_, _, [], Resultado) -> Resultado;
gerar_possibilidades_para_vazios(Matriz, Regiao, [{X, Y} | Resto], Resultado) ->
  PossibilidadesAtual = obter_possibilidades_para_coordenada(Matriz, Regiao, {X, Y}),
  gerar_possibilidades_para_vazios(Matriz, Regiao, Resto, Resultado ++ [PossibilidadesAtual]).

% Retorna tuplas com as coorenadas vazias e suas possibilidades
valores_possiveis_na_regiao(Matriz, Regiao, IdRegiao) ->
  Coordenadas = coordenadas_da_regiao(Regiao, IdRegiao),
  CoordsVazias = lists:filter(fun({X, Y}) -> obter_valor_na_matriz(Matriz, {X, Y}) == 0 end, Coordenadas),
  [{Coord, obter_possibilidades_para_coordenada(Matriz, Regiao, Coord)} || Coord <- CoordsVazias].

% Aplica as regras do kojun na matriz
aplicar_regras(Matriz, Regiao, [{X, Y} | Resto]) ->
  NovaMatriz1 = inicializar_contador_para_unica_possibilidade(Matriz, Regiao),
  Possibilidades = obter_possibilidades_para_coordenada(NovaMatriz1, Regiao, {X, Y}),
  NovaMatriz2 = if length(Possibilidades) == 1 -> atualizar_valor_na_matriz(NovaMatriz1, {X, Y}, hd(Possibilidades)); true -> NovaMatriz1 end,
  if Resto == [] -> NovaMatriz2; true -> aplicar_regras(NovaMatriz2, Regiao, Resto) end.

% Função de backtracking para testar as possibilidades de uma coordenada
testar_possibilidades(Matriz, _, _, []) -> Matriz;
testar_possibilidades(Matriz, Regiao, {X, Y}, [Possibilidade | Resto]) ->
  NovaMatriz = atualizar_valor_na_matriz(Matriz, {X, Y}, Possibilidade),
  CoordsVazias = coordenadas_vazias_na_matriz(Matriz),
  PossVazios = gerar_possibilidades_para_vazios(Matriz, Regiao, CoordsVazias, []),
  NovasCoordsVazias = coordenadas_vazias_na_matriz(NovaMatriz),
  NovasPossVazios = gerar_possibilidades_para_vazios(NovaMatriz, Regiao, NovasCoordsVazias, []),
  TamanhoNovasPoss = length(NovasPossVazios),
  TamanhoPoss = length(PossVazios),
  if TamanhoNovasPoss < (TamanhoPoss - 1) ->
       testar_possibilidades(Matriz, Regiao, {X, Y}, Resto);
     true -> resolver_tabuleiro(NovaMatriz, Regiao)
  end.

% Resolve o tabuleiro
resolver_tabuleiro(Matriz, Regiao) ->
  CoordsVazias = coordenadas_vazias_na_matriz(Matriz),
  if length(CoordsVazias) == 0 -> Matriz;
     true ->
       NovaMatriz = aplicar_regras(Matriz, Regiao, CoordsVazias),
       if NovaMatriz == Matriz ->
          NovasCoordsVazias = coordenadas_vazias_na_matriz(NovaMatriz),
          if length(NovasCoordsVazias) == 0 -> NovaMatriz;
           true ->
             NovasPossibilidades = obter_possibilidades_para_coordenada(Matriz, Regiao, hd(NovasCoordsVazias)),
             testar_possibilidades(Matriz, Regiao, hd(NovasCoordsVazias), NovasPossibilidades)
          end;
        true -> resolver_tabuleiro(NovaMatriz, Regiao)
       end
  end.

% Imprime a resposta final
imprimir_matriz_final(Matriz) ->
  NumVazios = length(coordenadas_vazias_na_matriz(Matriz)),
  if NumVazios > 0 -> io:format("Não foi possível solucionar.");
     true -> lists:foreach(fun(Linha) -> io:format("~p~n", [Linha]) end, Matriz)
  end.

start() ->
  %Tabuleiros para questão 1
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

% Resposta esperada:
% [2,1,3,2,1,2]
% [1,4,2,3,6,1]
% [4,3,4,2,5,3]
% [3,1,2,1,2,1]
% [1,2,3,5,4,2]
% [2,1,2,1,3,1]

  imprimir_matriz_final(resolver_tabuleiro(ValoresKojun, RegioesKojun)).
