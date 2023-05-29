:- module(proylcc, 
	[  
        randomPotencia/3,
        potencia_dos/2,
        posiciones_a_indices/4,
        sort/2,
        reemplazar_por_ceros_y_ultimo/5,
        reemplazar_por_ceros_y_ultimo/6,
        agregar_suma_ultimo/3,
        enColumnas/3,
        enColumnas/4,
        columna/4,
        columnas/6,
        columnas/7,
        eliminar_ceros_y_contar/3,
        completar_con_randoms/3,
        enFilas/2,
        sacarPrimerElem/3,
        armando_grilla/6,
        armando_grilla/7,
        eliminando_bloques/3,
        eliminando_bloques/2,
        enlistar/3,
        posicionesAdyacentes/3,
        posicionValida/2,
        agrupar/7,
        colapsarIguales/3,
        colapsarIguales/6,
        destruirGrupos/3,
		join/4,
        booster/3
	]).
  /**
     * randomPotencia(Potencia) 
     * CI es la cota inferior, CS es la cota uperior, y Potencia es un numero random entre CI y CS.
     */ 
    randomPotencia(CI,CS,Potencia) :-
        random(CI, CS, Potencia).
    
    /**
     * potencia_dos(N, Potencia)
     * N es un numero dado y Potencia es la menor potencia de 2 mayoro igual que N.
     */ 
    potencia_dos(N, Potencia) :-
        Potencia is (2 ** ceil(log(N) / log(2))).
    

    /**
     * posiciones_a_indices(C, Posiciones, Indices, Ultimo).
     * C es la cantidad de Columnas, Posiciones es una lista de posiciones [X,Y],
     * Indices es la lista de salida que contiene las posiciones convertidas a indices, 
     * Ultimo es el ultimo elemento de la lista Posiciones.
     */ 
    posiciones_a_indices(_,[], [], _).
    posiciones_a_indices(C,[[X,Y]], [Index], Index) :- Index is (X*C + Y).
    posiciones_a_indices(C,[[X,Y]|Posiciones], [Index|Indices], Ultimo) :-
        Index is (X*C + Y),
        posiciones_a_indices(C, Posiciones, Indices, Ultimo).

    /**
     * reemplazar_por_ceros_y_ultimo(U, P, L, Result, Index, S) 
     * U es el ultimo elemento de la lista P.
     * P es la lista de indices,
     * L es la lista Matriz,
     * Result es la lista resultante con 0 en las posiciones de los bloques a eliminar y 
     * 1 en la posicion del ultimo elemento de la lista P,
     * Index es el indice de la posicion actual
     * S es la suma del camino.
     */ 
    reemplazar_por_ceros_y_ultimo(U, P, L, Result, S) :-    % Inicial
        reemplazar_por_ceros_y_ultimo(U, P, L, Result, 0, S). % llamado
    
    reemplazar_por_ceros_y_ultimo(_, [], L, L, _, 0). % Caso Base
    reemplazar_por_ceros_y_ultimo(U, [P|Ps], [X|Ls], [1|Result], Index, Suma) :- 
    % CR: si el indice esta en la lista de posiciones y es la ultima posicion del Camino (U), entonces reemplazo X por 1.
        P =:= Index,
        P =:= U,
        !,
        NextIndex is (Index + 1),
        reemplazar_por_ceros_y_ultimo(U, Ps, Ls, Result, NextIndex, SumaAux),
        Suma is (X + SumaAux).
    reemplazar_por_ceros_y_ultimo(U, [P|Ps], [X|Ls], [0|Result], Index, Suma) :-
         % CR: si el indice esta en la lista de posiciones, entonces reemplazo X por 0.
        P =:= Index,
        !,
        NextIndex is (Index + 1),
        reemplazar_por_ceros_y_ultimo(U, Ps, Ls, Result, NextIndex, SumaAux),
        Suma is (X + SumaAux).
    reemplazar_por_ceros_y_ultimo(U,Ps, [L|Ls], [L|Result], Index, Suma) :-
        % CR: si el indice no esta en la lista de posiciones, entonces copio X.
        NextIndex is (Index + 1),
        reemplazar_por_ceros_y_ultimo(U, Ps, Ls, Result, NextIndex,Suma).
    

     /**
     * agregar_suma_ultimo(G, Suma, Resultado)
     * G es la lista Matriz,
     * Suma es la suma del camino.
     * Resultado es la lista resultante reemplazando el ultimo elemento del camino por la suma del mismo.
     */ 
    agregar_suma_ultimo([],_,[]). % Caso base: si la lista está vacía, la lista resultante también está vacía.
    agregar_suma_ultimo([1|G],Suma,[Suma|Resultado]) :- 
        % Si la cabeza de la lista es 1, se reemplaza por la suma del camino y se sigue procesando la cola.	
    	agregar_suma_ultimo(G, Suma, Resultado).
	agregar_suma_ultimo([X|G],Suma, [X|Resultado]) :- % Sino copio X.
        agregar_suma_ultimo(G, Suma, Resultado).
    
    
    %------Parte 2------
     /**
     * enColumnas(G,Columnas,Res,Pos)
     * G es la lista Matriz,
     * Columnas es la cantidad de columnas que componen la Matriz
     * os es el indice del elemento actual,
     * Res es una lista de listas donde cada una es una columna de la  Matriz.
     */ 
    enColumnas(G,Columnas, Res) :- enColumnas(G,Columnas,Res,0).
    enColumnas(_,Columnas,[],Columnas).
    enColumnas(G,Columnas,[C|Res],Pos) :- 
        columna(G,Pos,Columnas,C),
        Posicion is (Pos+1),
        enColumnas(G,Columnas,Res,Posicion).

    columna(G,N,_,[]) :- 
        length(G,Tam),
        N>=Tam.
    columna(G,N,Columnas,[Elem|Resultado]) :-
        nth0(N,G,Elem),
        Naux is (N+Columnas),
        columna(G,Naux,Columnas,Resultado).

     
    /**
     * eliminar_ceros_y_contar(L, Resultado, Contador)
     * G es una lista,
     * Resultado es la lista sin 0
     * Contador es la cantidad de 0 que se eliminaron de la lista.
     */ 
    eliminar_ceros_y_contar([], [], 0). 
	eliminar_ceros_y_contar([0|L], Resultado, Contador) :- % Si la cabeza de la lista es 0, se ignora y se sigue procesando L.
        eliminar_ceros_y_contar(L, Resultado, Contador0),
        Contador is (Contador0 + 1).
    eliminar_ceros_y_contar([Cabeza|L], [Cabeza|Resultado], Contador) :- % Si la cabeza de la lista no es 0, se conserva y se procesa L.
        Cabeza \= 0,                                                       % Se asegura de que la cabeza no sea 0
        eliminar_ceros_y_contar(L, Resultado, Contador).
    
   
     /**
     * completar_con_randoms(N,L,Resultado)
     * N es la cantidad de numeros randoms a agregar
     * L es la lista a modificar
     * Resultado es una lista con los N numero randoms seguidos de la lista L
     */ 
    completar_con_randoms(0,L,L). % Caso Base: si N es 0 entonces Resultado es L
    completar_con_randoms(N,L,[Result|Resultado]) :- 
        % Caso Rescursivo: agrego un random a la lista Resultado y disminuye la cantidad de randoms a agregar
        randomPotencia(1,6,Potencia),
        Result is (2 ** Potencia),
        Cont is (N-1),
        completar_con_randoms(Cont,L,Resultado).

     /**
     * armando_grilla(Index,C1,C2,C3,C4,C5,G)
     * C1,C2,C3,C4,C5 son listas donde cada una es una columna de la Matriz a armar,
     * Index es el indice del elemento actual,
     * G es la lista Matriz resultate compuesta por las 5 columna de entrada.
     */     
    enFilas(M, Resultado):- enFilas(M, [], [],Resultado).
    enFilas([],[],R,R).
    enFilas([],Maux,R,Resultado) :- enFilas(Maux,[],R,Resultado).
    enFilas([X|M], Maux, R, Resultado) :-
        sacarPrimerElem(X,E,Res),
        append(R,[E],NR),
        (Res \= [] -> append(Maux,[Res],NMaux); NMaux=Maux),
        enFilas(M, NMaux, NR, Resultado).
        
    sacarPrimerElem([],_,[]).
    sacarPrimerElem([E|L],E,L).
    
    /**
     * eliminar(M,T)
     * M es una lista de listas donde cada una es una columna de una Matriz.
     * T es una lista de listas donde cada una es una columna de una Matriz 
     * con los nuevos randoms sobre los bloques eliminados (efecto gravedad).
     */
    eliminar([],[]).
    eliminar([X|M],[CR|T]):-
    eliminar_ceros_y_contar(X, SC, Cont),
    completar_con_randoms(Cont,SC,CR),
    eliminar(M,T).

    /**
    * eliminando_bloques(M,T)
    * M es la lista Matriz de entrada.
    * T es la lista Matriz con los nuevos randoms sobre los bloques eliminados.
    */ 
    eliminando_bloques(M,Columnas,T):-
    enColumnas(M,Columnas,Res),
    eliminar(Res,MCR),
    enFilas(MCR,T).
    
    enlistar(Lista1, Lista2, [Lista1, Lista2]).

%------------------------------------------------
posicionesAdyacentes(Pos, Col, P) :-
    X is Pos // Col,
    Y is Pos - (X * Col),
    Arriba is X-1,
    Abajo is X+1,
    Derecha is Y+1,
    Izquierda is Y-1,

    (X=:=0 -> PosArriba = [] ; PosArriba = [Arriba,Y]),
    (X=:=7 -> PosAbajo = [] ; PosAbajo = [Abajo,Y]),
    (Y=:=4 -> PosDerecha = [] ; PosDerecha = [X,Derecha]),
    (Y=:=0 -> PosIzquierda = [] ; PosIzquierda = [X,Izquierda]),
    ((X=:=0;Y=:=0) -> PosArrIz = [] ; PosArrIz = [Arriba,Izquierda]),
    ((X=:=0;Y=:=4) -> PosArrDr = [] ; PosArrDr = [Arriba,Derecha]),
    ((X=:=7;Y=:=0) -> PosAbjIz = [] ; PosAbjIz = [Abajo,Izquierda]),
    ((X=:=7;Y=:=4) -> PosAbjDr = [] ; PosAbjDr = [Abajo,Derecha]),
    findall(N, (
        member(N, [PosArrIz,PosArriba,PosArrDr,PosIzquierda,PosDerecha,PosAbjIz,PosAbajo,PosAbjDr]),
        dif(N,[]))
    , Ady),
    posiciones_a_indices(5,Ady,P,_).

    posicionValida(P, CS) :- P >= 0, P < CS.

    agrupar(_, _, _, [], VisI, VisI, []).
    agrupar(M, C, N, [P|Pos], VisI, V, NewG) :-
        not(member(P,VisI)),
        append(VisI, [P], NewVisI),
        nth0(P, M, N),
        posicionesAdyacentes(P, C, NPos),
    	!,
    	agrupar(M, C, N, NPos, NewVisI, VisIAux, G1),
    	agrupar(M, C, N, Pos,  VisIAux, V, G2),
        append([P|G1], G2, NewG).
    agrupar(M, C, N, [P|Pos], VisI, V, G) :- 
        not(member(P,VisI)),
        append(VisI, [P], NewVisI),
        agrupar(M, C, N, Pos,  NewVisI, V, G).
    agrupar(M, C, N, [_|Pos],VisI, VisIAux, G) :- agrupar(M, C, N, Pos, VisI, VisIAux, G).


    colapsarIguales(M, C, G) :- colapsarIguales(M, M, C, [], G, 0). 
    colapsarIguales(_, [], _, _, [], _).
    colapsarIguales(M, [X|Ms], C, Vis, [G|Grupos], Pos) :-
        not(member(Pos, Vis)),
        agrupar(M, C, X, [Pos], [], _, G),
        append(Vis, G, NewVista),
        Posicion is (Pos + 1), 
        colapsarIguales(M, Ms, C, NewVista, Grupos, Posicion).
    
    colapsarIguales(M, [_|Ms], C, Vis, Grupos, Pos) :- 
        Posicion is (Pos + 1),
        colapsarIguales(M, Ms, C, Vis, Grupos, Posicion).

    destruirGrupos(_, [], []).
    destruirGrupos(Grilla, [X|G], [R1|Resultado1]) :-
        length(X, Tam),
    	Tam > 1,  
    	I is Tam-1,
        nth0(I, X, Ult),
    	sort(X, Xs),
        reemplazar_por_ceros_y_ultimo(Ult, Xs, Grilla, R, Suma),
    	potencia_dos(Suma, S),
    	agregar_suma_ultimo(R,S,R1),
        destruirGrupos(R1, G, Resultado1).
	destruirGrupos(Grilla, [_|G], Resultado1) :- destruirGrupos(Grilla, G, Resultado1).
    
    
    /**
     * join(+Grid, +NumOfColumns, +Path, -RGrids) 
     * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
     * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
     */ 
    
    join(Grid, NumOfColumns, Path, RGrids) :-
        posiciones_a_indices(NumOfColumns, Path, Posiciones, Ult),
        sort(Posiciones, P),
        reemplazar_por_ceros_y_ultimo(Ult, P, Grid, R, Suma),
    	potencia_dos(Suma, S),
    	agregar_suma_ultimo(R,S,R1),
        eliminando_bloques(R1,NumOfColumns, R2),
        enlistar(R1, R2, RGrids).
    
    booster(Grid, NumOfColumns, RGrids) :-
        colapsarIguales(Grid, NumOfColumns, G),
        destruirGrupos(Grid, G, R),
    	length(R, Tam),
    	I is Tam-1,
        nth0(I, R, Ult),
    	eliminando_bloques(Ult,NumOfColumns, Result),
    	enlistar(Ult,Result,RGrids).