:- use_module(library(clpfd)).


kojun(N) :-
    tabuleiro(N,Tab,Tab_region),
    diminuir_possibilidades(Tab, Tab_region).
    
diminuir_possibilidades(Linhas,Linhas_region) :-    %terminar
    maplist(limites(Linhas,Linhas_region), Linhas),
    regiaounica(Linhas).
    


celula_adjacente([]).
celula_adjacente([_|[]]).
celula_adjacente([Adj_1,Adj_2|Tail]) :-
    adj_1 #\= adj_2,
    
    %recursao
    celula_adjacente([Adj_2|Tail]).


limites(Tab,[Valor|[]], Tab_region, [Regiao|[]]) :-      % rever o fato de ser tabuleiros separados
    maior_regiao(Tab_region, Regiao, Max),
    Valor in 1..Max.
limites(Tab,[Valor, ValorNext|[Tail]], Tab_region, [Regiao, Next|Tail]) :-
    maior_regiao(Tab_region, Regiao, Max),
    Valor in 1..Max,
    limites(Tab, [ValorNext|Tail], Tab_region, [Next|Tail]).


maior_regiao(Tab_region, Regiao, Max) :-   %testar
    append(Tab_region, Tab_region_linha),
    ocurrenceof(Tab_region_linha, Regiao, Max).
    
    
    
ocurrenceof([] , _,0).    %testar
ocurrenceof([H|T] , H,NewCount):-
 ocurrenceof(T,H,OldCount),
 NewCount is OldCount +1.
ocurrenceof([H|T] , H2,Count):-
 dif(H,H2),
 ocurrenceof(T,H2,Count).    	


tabuleiro(1, [[2,_,_,_,1,_],
             [_,_,_,3,_,_],
             [_,3,_,_,5,3],
             [_,_,_,_,_,_],
             [_,_,3,_,4,2],
             [_,_,_,_,_,_]]).
