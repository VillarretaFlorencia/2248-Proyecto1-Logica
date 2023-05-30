:- module(proylcc, 
	[  
        random_potencia/3,
        potencia_dos/2,
        posiciones_a_indices/4,
        sort/2,
        reemplazar_por_ceros_y_ultimo/5,
        reemplazar_por_ceros_y_ultimo/6,
        agregar_suma_ultimo/3,
        en_columnas/3,
        en_columnas/4,
        eliminar_ceros_y_contar/3,
        completar_con_randoms/3,
        en_filas/2,
        sacar_primer_elem/3,
        eliminando_bloques/3,
        eliminando_bloques/2,
        enlistar/3,
        posiciones_adyacentes/4,
        posicionValida/2,
        agrupar/8,
        colapsar_iguales/4,
        colapsar_iguales/7,
        destruir_grupos/3,
		join/4,
        booster/3
	]).
  
    /**
     * random_potencia(+CI,+CS,-Potencia) 
     * CI es la cota inferior, CS es la cota uperior, y Potencia es un numero random entre CI y CS.
     */ 
        random_potencia(CI,CS,Potencia) :-
            random(CI, CS, Potencia).
    
    /**
     * potencia_dos(+N, -Potencia)
     * N es un numero dado y Potencia es la menor potencia de 2 mayoro igual que N.
     */ 
        potencia_dos(N, Potencia) :-
            Potencia is (2 ** ceil(log(N) / log(2))).
        

    /**
     * posiciones_a_indices(+C, +Posiciones, -Indices, -Ultimo).
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
     * reemplazar_por_ceros_y_ultimo(+U, +P, +L, -Result, +Index, -S) 
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
     * agregar_suma_ultimo(+G, +Suma, -Resultado)
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
    
    
    
     /**
     * en_columnas(+G,+Columnas,-Res,+Pos)
     * G es la lista Matriz a trasponer,
     * Columnas es la cantidad de columnas que componen la Matriz,
     * Pos es el indice del elemento actual,
     * Res es una lista de listas donde cada una es una columna de la  Matriz.
     */ 
        en_columnas(G,Columnas, Res) :- en_columnas(G,Columnas,Res,0).
        en_columnas(_,Columnas,[],Columnas).
        en_columnas(G,Columnas,[C|Res],Pos) :- 
            columna(G,Pos,Columnas,C),
            Posicion is (Pos+1),
            en_columnas(G,Columnas,Res,Posicion).

    /**
     * columna(+G,+N,+Columnas,-Resultado)
     * G es la lista Matriz,
     * N es la posicion actual desde donde se desea obtener la columna,
     * Columnas es la cantidad de columnas que componen la Matriz,
     * Resultado es una lista que contiene una columna de la  Matriz.
     */ 
        columna(G,N,_,[]) :- 
            length(G,Tam),
            N>=Tam.
        columna(G,N,Columnas,[Elem|Resultado]) :-
            nth0(N,G,Elem),
            Naux is (N+Columnas),
            columna(G,Naux,Columnas,Resultado).

     
    /**
     * eliminar_ceros_y_contar(+L, -Resultado, -Contador)
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
     * completar_con_randoms(+N,+L,-Resultado)
     * N es la cantidad de numeros randoms a agregar
     * L es la lista a modificar
     * Resultado es una lista con los N numero randoms seguidos de la lista L
     */ 
        completar_con_randoms(0,L,L). % Caso Base: si N es 0 entonces Resultado es L
        completar_con_randoms(N,L,[Result|Resultado]) :- 
            % Caso Rescursivo: agrego un random a la lista Resultado y disminuye la cantidad de randoms a agregar
            random_potencia(1,6,Potencia),
            Result is (2 ** Potencia),
            Cont is (N-1),
            completar_con_randoms(Cont,L,Resultado).

     /**
     * en_filas(+M, +Maux, +R, -Resultado) 
     * M es una lista de listas que representa una Matriz.
     * M aux son las listas sin su primer elemento en cada iteracion.
     * R es una lista de entrada donde se actualiza la lista Matriz resultate.
     * Resultado es una lista de salida que representa la Matriz traspuesta de la que 
     * componen las listas de la lista de entada M.
     */     
        en_filas(M, Resultado):- en_filas(M, [], [],Resultado).
        en_filas([],[],R,R).
        en_filas([],Maux,R,Resultado) :- en_filas(Maux,[],R,Resultado).
        en_filas([X|M], Maux, R, Resultado) :-
            sacar_primer_elem(X,E,Res),
            append(R,[E],NR),
            (Res \= [] -> append(Maux,[Res],NMaux); NMaux=Maux),
            en_filas(M, NMaux, NR, Resultado).

     /**
     * sacar_primer_elem(+L,-E,-Resto)
     * L lista de entrada.
     * E primer elemento de la lista.
     * Resto lo que queda en la lista sin su primer elemento.
     */      
        sacar_primer_elem([],_,[]).
        sacar_primer_elem([E|L],E,L).
    
    /**
     * eliminar(+M,-T)
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
    * eliminando_bloques(+M,+Columnas,-T)
    * M es la lista Matriz de entrada,
    * Columnas cantidad de columnas de M
    * T es la lista Matriz con los nuevos randoms sobre los bloques eliminados,
    */ 
        eliminando_bloques(M,Columnas,T):-
            en_columnas(M,Columnas,Res),
            eliminar(Res,MCR),
            en_filas(MCR,T).
    
    /**
    * enlistar(+Lista1, +Lista2, -Resultado)
    * Lista1, Lista2, dos listas,
    * Resultado una lista compuesta con las dos listas de entrada.
    */ 
        enlistar(Lista1, Lista2, [Lista1, Lista2]).

%------------------------------------------------
/**
    * posiciones_adyacentes(+Pos, +Col, +F, -P)
    * Pos es una posicion,
    * Col es la cantidad de columnas,
    * F es la cantidad de filas,
    * P es una lista con las posiciones adyacentes validas.
    */ 
    posiciones_adyacentes(Pos, Col, F, P) :-
        X is Pos // Col,
        Y is Pos - (X * Col),
        Arriba is X-1,
        Abajo is X+1,
        Derecha is Y+1,
        Izquierda is Y-1,
        LC is Col-1, %limite Columnas
        LF is F-1, %limete Filas
        (X=:=0 -> PosArriba = [] ; PosArriba = [Arriba,Y]),
        (X=:=LF -> PosAbajo = [] ; PosAbajo = [Abajo,Y]),
        (Y=:=LC -> PosDerecha = [] ; PosDerecha = [X,Derecha]),
        (Y=:=0 -> PosIzquierda = [] ; PosIzquierda = [X,Izquierda]),
        ((X=:=0;Y=:=0) -> PosArrIz = [] ; PosArrIz = [Arriba,Izquierda]),
        ((X=:=0;Y=:=LC) -> PosArrDr = [] ; PosArrDr = [Arriba,Derecha]),
        ((X=:=LF;Y=:=0) -> PosAbjIz = [] ; PosAbjIz = [Abajo,Izquierda]),
        ((X=:=LF;Y=:=LC) -> PosAbjDr = [] ; PosAbjDr = [Abajo,Derecha]),
        findall(N, (
            member(N, [PosArrIz,PosArriba,PosArrDr,PosIzquierda,PosDerecha,PosAbjIz,PosAbajo,PosAbjDr]),
            dif(N,[]))
        , Ady),
        posiciones_a_indices(Col,Ady,P,_).

/**
    *  agrupar(+M, +C, +F, +N, +Pos, +VisI, +V, -G)
    * M es una Matriz,
    * C es la cantidad de columnas,
    * F es la cantidad de filas,
    * N es el numero que se encuentra en la Matriz en la Posicion P,
    * P es una lista con las posiciones adyacentes validas,
    * VisI es la lista de visitados internos,
    * V es la lista de visitados generales,
    * G es la lista con las posiciones que componen el grupo formado por las posiciones adyacentes que
    * tienen el mismo N.
    */ 
        agrupar(_, _, _, _, [], VisI, VisI, []).
        agrupar(M, C, F, N, [P|Pos], VisI, V, NewG) :-
            not(member(P,VisI)),
            append(VisI, [P], NewVisI),
            nth0(P, M, N),
            posiciones_adyacentes(P, C, F, NPos),
            !,
            agrupar(M, C, F, N, NPos, NewVisI, VisIAux, G1),
            agrupar(M, C, F, N, Pos,  VisIAux, V, G2),
            append([P|G1], G2, NewG).
        agrupar(M, C, F, N, [P|Pos], VisI, V, G) :- 
            not(member(P,VisI)),
            append(VisI, [P], NewVisI),
            agrupar(M, C, F, N, Pos,  NewVisI, V, G).
        agrupar(M, C, F, N, [_|Pos],VisI, VisIAux, G) :- agrupar(M, C, F, N, Pos, VisI, VisIAux, G).

    /**
    * colapsar_iguales(+M, +Maux, +C, +F, +Vis, -G, +Pos)
    * M es una Matriz,
    * Maux es la Matriz qu se va visitando,
    * C es la cantidad de columnas,
    * F es la cantidad de filas,
    * Vis es la lista de visitados generales,
    * G es la lista con los grupos, donde cada grupo esta formado por las posiciones adyacentes que
    * tienen el mismo numero,
    * Pos es la posicion actual.
    */ 
        colapsar_iguales(M, C, F, G) :- colapsar_iguales(M, M, C, F, [], G, 0). 
        colapsar_iguales(_, [], _, _, _, [], _).
        colapsar_iguales(M, [X|Ms], C, F, Vis, [G|Grupos], Pos) :-
            not(member(Pos, Vis)),
            agrupar(M, C, F, X, [Pos], [], _, G),
            append(Vis, G, NewVista),
            Posicion is (Pos + 1), 
            colapsar_iguales(M, Ms, C, F, NewVista, Grupos, Posicion).    
        colapsar_iguales(M, [_|Ms], C, F, Vis, Grupos, Pos) :- 
            Posicion is (Pos + 1),
            colapsar_iguales(M, Ms, C, F, Vis, Grupos, Posicion).

 /**
    * destruir_grupos(+Grilla, +G, -Resultado)
    * Grilla es una Matriz,
    * G es la Matriz que se va visitando,
    * Resultado es una lista con listas donde cada una representa la Matriz con 0's en los bloques a eliminar
    * y la suma donde termina el grupo.
    */ 
        destruir_grupos(_, [], []).
        destruir_grupos(Grilla, [X|G], [R1|Resultado]) :-
            length(X, Tam),
            Tam > 1,  
            I is Tam-1,
            nth0(I, X, Ult),
            sort(X, Xs),
            reemplazar_por_ceros_y_ultimo(Ult, Xs, Grilla, R, Suma),
            potencia_dos(Suma, S),
            agregar_suma_ultimo(R,S,R1),
            destruir_grupos(R1, G, Resultado).
        destruir_grupos(Grilla, [_|G], Resultado) :- destruir_grupos(Grilla, G, Resultado).
    
    
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
    
     /**
     * booster(+Grid, +NumOfColumns, +Path, -RGrids) 
     * RGrids es la lista de grillas representando el efecto, en etapas, de e eliminar los grupos de bloques 
     * adyacentes que comparten el mismo numero, en la grilla Grid, con número de columnas NumOfColumns. 
     * El número 0 representa que la celda está vacía. 
     */ 
        booster(Grid, NumOfColumns, RGrids) :-
            length(Grid, T),
            Filas is (T//NumOfColumns),
            colapsar_iguales(Grid, NumOfColumns, Filas, G),
            destruir_grupos(Grid, G, R),
            length(R, Tam),
            I is Tam-1,
            nth0(I, R, Ult),
            eliminando_bloques(Ult,NumOfColumns, Result),
            append(R, [Result],RGrids).