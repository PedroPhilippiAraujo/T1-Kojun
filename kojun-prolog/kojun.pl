:- use_module(library(clpfd)).

kojun(N) :-
    tabuleiro(N,Tab),   % Escolhe tabuleiro.
    restricao_possibilidades(Tab),
    testa_possibilidades(Tab),      % Etapa de backtracking.
    print_tabuleiro(Tab).  % Exibir o resultado.

kojun_entrada(Tabuleiro_entrada) :- %mesmo solucionador mas o tabuleiro e passado como entrada
    restricao_possibilidades(Tabuleiro_entrada),
    testa_possibilidades(Tabuleiro_entrada),      % Etapa de backtracking.
    print_tabuleiro(Tabuleiro_entrada).  % Exibir o resultado.

% Aplicar as regras do jogo:
restricao_possibilidades(Linhas) :-
    maplist(limites(Linhas), Linhas), %define limites do jogo
    possibilidades_unicas(Linhas), %preenche celulas que possuem um unico valor (ex. regiao de tamanho 1)
    maplist(celulas_adjacentes_horizontal, Linhas), %verifica regra da adjacencia ortogonal horizontal
    transpose(Linhas, Colunas),
    maplist(restricao_vertical, Colunas). %verfica regra da adjacencia ortogonal vertical e regra de que o numero superior é maior

limites(Tab, [Regiao-Valor|[]]) :-      % Caso seja a última célula
    maximo_regiao(Tab, Regiao, Max),
    Valor in 1..Max.
limites(Tab, [Regiao-Valor, Next|Tail]) :-      % Recursividade para buscar os limites da célula
    % maximo_regiao encontra a célula de maior valor da região.
    maximo_regiao(Tab, Regiao, Max),
    % O intervalo será entre 1 e o valor encontrado por maximo_regiao.
    Valor in 1..Max,
    limites(Tab, [Next|Tail]).      % recursividade

maximo_regiao(Tab, Regiao, Max) :-
    append(Tab, Tab_linha),     % Concatena todas as linhas em uma
    sort(Tab_linha, Tab_ordenado),      % Ordena todos os itens da lista
    group_pairs_by_key(Tab_ordenado, Grupos),        
    member(Regiao-Valor, Grupos),       % separa as regioes e agrupa em tuplas
    length(Valor, Max).        % retorna valor máximo possível da região é o tamanho da lista de tuplas.

valores_unicos(_-List) :- all_distinct(List). %all distinct = valores diferentes
possibilidades_unicas(Tab) :-       
    append(Tab, Linha_unica),       % a matriz de Tabuleiro é transformada em uma linha só.
    sort(Linha_unica, Linha_ordenada),  % Ordena a linha obtida.
    group_pairs_by_key(Linha_ordenada, Grupos), % Separa os valores por região.
    maplist(valores_unicos, Grupos).    % Aplicamos val_unicos em todas as regiões.


% Verifica adjacências laterais.

celulas_adjacentes_horizontal([]). %caso base
celulas_adjacentes_horizontal([_|[]]). %caso fim
celulas_adjacentes_horizontal([_-Adj_1,Regiao-Adj_2|Tail]) :-
    Adj_1 #\= Adj_2,
    %Recursividade
    celulas_adjacentes_horizontal([Regiao-Adj_2|Tail]).

% Regra de valor inferior/superior
%usa transposicao de matriz
restricao_vertical([]). %caso base
restricao_vertical([_|[]]). %caso fim
restricao_vertical([Regiao_sup-Valor_sup,Regiao_inf-Valor_inf|Tail]) :-
    
    % (Regiao_sup diferente de Regiao_inf E Valor_sup diferente Valor_inf) OU (Valor_sup > Valor_inf)
    ((Regiao_sup #\= Regiao_inf) #/\ (Valor_sup #\= Valor_inf)) #\/ (Valor_sup #> Valor_inf),
    % Recursividade
    restricao_vertical([Regiao_inf-Valor_inf|Tail]).

% Função de backtracking que irá "chutar" os valores possíveis.
testa_possibilidades(Linhas) :-
    valores_regiao(Linhas, Valores),    % Obtém uma lista de valores possíveis.
    % usa-se o predicado base "label para testar todas as possibilidades de cada celuca
    maplist(label, Valores).


% Obtém recursivamente os valores de cada regiao
valores_regiao([], []).     % Caso base
valores_regiao([H|T], [H2|T2]) :-  
    pairs_values(H, H2),    % Insere o valor de H em H2.
    valores_regiao(T, T2). %recursividade


%display do tabuleiro
print_tabuleiro(Linhas) :- 
    valores_regiao(Linhas, Valores), 
    maplist(portray_clause, Valores).


%Tabuleiro 1: https://www.janko.at/Raetsel/Kojun/001.a.htm */

tabuleiro(1, [ [(01-2), (01-_), (02-_), (02-_), (02-1), (03-_)],
               [(04-_), (04-_), (04-_), (04-3), (04-_), (03-_)],
               [(05-_), (06-3), (06-_), (06-_), (04-5), (07-3)],
               [(05-_), (05-_), (05-_), (06-_), (07-_), (07-_)],
               [(08-_), (08-_), (10-3), (11-_), (11-4), (11-2)],
               [(09-_), (09-_), (10-_), (10-_), (11-_), (11-_)]]).

