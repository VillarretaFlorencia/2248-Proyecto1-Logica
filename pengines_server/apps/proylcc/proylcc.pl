:- module(proylcc, 
	[  
        columnas/6,
        eliminar_ceros_y_contar/3,
        completar_con_ceros/3,
        armando/6,
        positions_to_numbers/4,
        sort/2,
        replace_with_zeros/4,
        cerosArriba/2,
        enlistar/3,
		join/4
	]).

positions_to_numbers(_,[], [], _).
positions_to_numbers(C,[[X,Y]], [Result], Result) :- Result is (X*C + Y).
positions_to_numbers(C,[[X,Y]|Rest], [Result|Results], Ultimo) :-
Result is (X*C + Y),
    positions_to_numbers(C, Rest, Results, Ultimo).

replace_with_zeros(U, P, L, Result) :-
    replace_with_zeros(U, P, L, Result, 0).

replace_with_zeros(_, [], L, L, _).
replace_with_zeros(U, [P|Ps], [_|Ls], [1|Result], Index) :-
    P =:= Index,
    P =:= U,
    !,
    NextIndex is (Index + 1),
    replace_with_zeros(U, Ps, Ls, Result, NextIndex).
replace_with_zeros(U, [P|Ps], [_|Ls], [0|Result], Index) :-
    P =:= Index,
    !,
    NextIndex is (Index + 1),
    replace_with_zeros(U, Ps, Ls, Result, NextIndex).
replace_with_zeros(U,Ps, [L|Ls], [L|Result], Index) :-
    NextIndex is (Index + 1),
    replace_with_zeros(U, Ps, Ls, Result, NextIndex).



%------Parte 2------

%Divide la grilla en 5 columnas 
columnas(G,C1,C2,C3,C4,C5):- columnas(G,0,C1,C2,C3,C4,C5).
columnas([],_,[],[],[],[],[]).
columnas([X|Xs],Index,[X|C1],C2,C3,C4,C5) :- 
    Index mod 5 =:= 0,
    !,
    NextIndex is (Index + 1),
    columnas(Xs,NextIndex,C1,C2,C3,C4,C5).

columnas([X|Xs],Index,C1,[X|C2],C3,C4,C5) :- 
     Index mod 5 =:= 1,
    !,
    NextIndex is (Index + 1),
    columnas(Xs,NextIndex,C1,C2,C3,C4,C5).

columnas([X|Xs],Index,C1,C2,[X|C3],C4,C5) :- 
     Index mod 5 =:= 2,
    !,
    NextIndex is (Index + 1),
    columnas(Xs,NextIndex,C1,C2,C3,C4,C5).

columnas([X|Xs],Index,C1,C2,C3,[X|C4],C5) :- 
     Index mod 5 =:= 3,
    !,
    NextIndex is (Index + 1),
    columnas(Xs,NextIndex,C1,C2,C3,C4,C5).

columnas([X|Xs],Index,C1,C2,C3,C4,[X|C5]) :- 
     Index mod 5 =:= 4,
    !,
    NextIndex is (Index + 1),
    columnas(Xs,NextIndex,C1,C2,C3,C4,C5).

eliminar_ceros_y_contar([], [], 0).              % Caso base: si la lista está vacía, la lista resultante también está vacía y el contador es cero.
eliminar_ceros_y_contar([0|Cola], Resultado, Contador) :- % Si la cabeza de la lista es 0, se ignora y se sigue procesando la cola.
    eliminar_ceros_y_contar(Cola, Resultado, Contador0),
    Contador is (Contador0 + 1).
eliminar_ceros_y_contar([Cabeza|Cola], [Cabeza|Resultado], Contador) :- % Si la cabeza de la lista no es 0, se conserva y se procesa la cola.
    Cabeza \= 0,                                                       % Se asegura de que la cabeza no sea 0
    eliminar_ceros_y_contar(Cola, Resultado, Contador).

completar_con_ceros(0,L,L).
completar_con_ceros(N,L,[0|Y]) :- 
    Cont is (N-1),
    completar_con_ceros(Cont,L,Y).

% Dada 5 columnas genera una grilla traspuesta
armando(C1,C2,C3,C4,C5,G):- armando(0,C1,C2,C3,C4,C5,G).
armando(_,[],[],[],[],[],[]).
armando(Index,[X|C1],C2,C3,C4,C5,[X|M]) :- 
    Index mod 5 =:= 0,
    !,
    NextIndex is (Index + 1),
    armando(NextIndex,C1,C2,C3,C4,C5,M).

armando(Index,C1,[X|C2],C3,C4,C5,[X|M]) :- 
    Index mod 5 =:= 1,
    !,
    NextIndex is (Index + 1),
    armando(NextIndex,C1,C2,C3,C4,C5,M).

armando(Index,C1,C2,[X|C3],C4,C5,[X|M]) :- 
    Index mod 5 =:= 2,
    !,
    NextIndex is (Index + 1),
    armando(NextIndex,C1,C2,C3,C4,C5,M).

armando(Index,C1,C2,C3,[X|C4],C5,[X|M]) :- 
    Index mod 5 =:= 3,
    !,
    NextIndex is (Index + 1),
    armando(NextIndex,C1,C2,C3,C4,C5,M).

armando(Index,C1,C2,C3,C4,[X|C5],[X|M]) :- 
    Index mod 5 =:= 4,
    !,
    NextIndex is (Index + 1),
    armando(NextIndex,C1,C2,C3,C4,C5,M).

cerosArriba(M,T):-
	columnas(M,L1,L2,L3,L4,L5),
	eliminar_ceros_y_contar(L1, C1, N1),
	eliminar_ceros_y_contar(L2, C2, N2),
	eliminar_ceros_y_contar(L3, C3, N3),
	eliminar_ceros_y_contar(L4, C4, N4),
	eliminar_ceros_y_contar(L5, C5, N5),
	completar_con_ceros(N1,C1,Col1),
	completar_con_ceros(N2,C2,Col2),
	completar_con_ceros(N3,C3,Col3),
	completar_con_ceros(N4,C4,Col4),
	completar_con_ceros(N5,C5,Col5),
	armando(Col1,Col2,Col3,Col4,Col5,T).

enlistar(Lista1, Lista2, [Lista1, Lista2]).

/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, _NumOfColumns, _Path, RGrids):-
    %Grid  = [N|Ns],
    %N2 is N*2,
	positions_to_numbers(NumOfColumns, Path, Posiciones, Ult),
	sort(Posiciones, P),
	replace_with_zeros(Ult, Grid, P, R),
    cerosArriba(R, R2),
    enlistar(R, R2, RGrids).
    %RGrids[Grid,[X|Xs]].
