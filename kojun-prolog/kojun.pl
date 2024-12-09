:- use_module(library(clpfd)).

%Tabuleiro 1: https://www.janko.at/Raetsel/Kojun/001.a.htm */

tabuleiro(1, [ [(01-2), (01-_), (02-_), (02-_), (02-1), (03-_)],
               [(04-_), (04-_), (04-_), (04-3), (04-_), (03-_)],
               [(05-_), (06-3), (06-_), (06-_), (04-5), (07-3)],
               [(05-_), (05-_), (05-_), (06-_), (07-_), (07-_)],
               [(08-_), (08-_), (10-3), (11-_), (11-4), (11-2)],
               [(09-_), (09-_), (10-_), (10-_), (11-_), (11-_)]]).

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

maior_regiao(Tab, Regiao, Max) :-
    append(Tab, Tab_linha),     % Concatena todas as linhas do tabuleiro em uma única linha
    sort(Tab_linha, Tab_ordenado),      % Ordena em ordem crescente todos os itens da lista
    group_pairs_by_key(Tab_ordenado, Grupos),       % Agrupa os itens da lista ordenada de acordo com o valor da região.
    member(Regiao-Valor, Grupos),       % Organiza em tuplas (regiao, valor) para dada região.
    length(Valor, Max).        % valor máximo possível da região é o tamanho da lista de tuplas.

limites(Tab, [Regiao-Valor|[]]) :-      % Caso seja a última célula
    maior_regiao(Tab, Regiao, Max),
    Valor in 1..Max.
limites(Tab, [Regiao-Valor, Next|Tail]) :-      % Recursividade para buscar os limites da célula
    % maximo_regiao encontra a célula de maior valor da região.
    maior_regiao(Tab, Regiao, Max),
    % O intervalo será entre 1 e o valor encontrado por maximo_regiao.
    Valor in 1..Max,
    limites(Tab, [Next|Tail]).      % Aplicamos recursivamente para ter todos os limites.

% Verifica se não há repetições na lista de linha/coluna.
% Se não houver, retorna verdadeiro.
valores_unicos(_-List) :- all_distinct(List).
