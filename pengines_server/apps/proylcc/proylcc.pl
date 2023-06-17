:- module(proylcc, 
	[  
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
            posiciones_a_indices(C, Posiciones, Indices, Ultimo). posiciones_a_indices(_,[], [], _).
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
            sort(X, Xs), %ordena para ubicar el bloque final en la posición más abajo y a la derecha.
            nth0(I, Xs, Ult),            
            reemplazar_por_ceros_y_ultimo(Ult, Xs, Grilla, R, Suma),
            potencia_dos(Suma, S),
            agregar_suma_ultimo(R,S,R1),
            destruir_grupos(R1, G, Resultado).
        destruir_grupos(Grilla, [_|G], Resultado) :- destruir_grupos(Grilla, G, Resultado).
    
    %----------------------------------------------------
/*
valor_ultimo_bloque(+Grilla,+Col,+Camino,-Result)
    -Result es el valor de la menor potencia de 2 mayor o igual a la suma de las coordenadas.
    -Camino es una lista de coordenadas de formato [Fila, Columna]
*/
valor_ultimo_bloque(Grilla, Col, Camino, Result):-
    valor_ultimo_bloque_aux(Grilla, Col, Camino, Suma),
    menorPotenciaDe2(Suma, Result).

/*
valor_ultimo_bloque_aux(+Grilla,+Col,+Camino, Result)
    -Función recursiva auxiliar para calcularPrimero/4
    -Result es la suma de los valores del camino en la grilla.
*/
% Caso Base:
valor_ultimo_bloque_aux(_,_,[],0).
% Caso Recursivo:
valor_ultimo_bloque_aux(Grilla, Col,[PosActual|Resto],Suma):-
    nth0(PosActual, Grilla, Valor),
    valor_ultimo_bloque_aux(Grilla,Col,Resto,Suma1),
    Suma is Valor+Suma1.

%coordenada_a_indice([X,Y], Col, Index) :- Index is (X*Col + Y).

/*
    valorEnCoordenada(+Grilla, +Col, +Coordenada, -Result)
        -Result es el velor almacenado en la Coordenada
*/

 /*
    visitar(+Coordenada, +Fil, +Col, +Grilla, +ListaVisitados, -Cluster)
        -Cluster es una lista de coordenadas ady con el mismo valor.
        -Marca como visitados los nodos a los que se puede mover y los agrega al cluster
        -NO actualiza la lista de visitados, sino que se debe usar Append a visitados con el Cluster
        al salir de la sentencia.
*/
visitar(Pos, Fil,Col, Grilla, Visitados,[Pos|GrupoN]):-
    %Lista es una lista de coordenadas ady que se pueden visitar
    valido_visitar(Grilla, Fil, Col,Visitados,Pos,Lista),
    visitar_aux(Lista,Fil,Col,Grilla, [Pos|Visitados], [],GrupoN).


/*
    visitar_aux(+ListaCoordenadas, +Fil, +Col, +Grilla, +Visitados, +GrupoActual,GrupoN)
        - GrupoN es el grupo formado por las coordenadas ady del mismo valor.
        - Se inicia con GrupoActual vacío.
*/
%Caso base la lista a visitar está vacía:
visitar_aux([],_,_,_,_,Grupo,Grupo).

%Caso Recursivo: el nodo no fue visitado.
visitar_aux([PosActual|Resto],Fil,Col, Grilla,Visitados,Grupo,[PosActual|GrupoN]):-
    not(member(PosActual, Visitados)),
    %Primero Visito la vecindad de la CoordenadaActual y  marco como visitados 
    valido_visitar(Grilla, Fil, Col,Visitados,PosActual,Lista),
    visitar_aux(Lista,Fil,Col,Grilla, [PosActual|Visitados], Grupo,GrupoN1),
    append([PosActual|Visitados],GrupoN1, VisitadosN),

    %Después visito el resto de la lista
    visitar_aux(Resto,Fil,Col,Grilla,VisitadosN, GrupoN1, GrupoN).

% Caso Recursivo: La coordenada ya fue visitada, sigo visitando las otras.
visitar_aux([PosActual|Resto],Fil,Col, Grilla,Visitados,Grupo,GrupoN):-
    member(PosActual, Visitados),
    visitar_aux(Resto,Fil,Col, Grilla,Visitados,Grupo,GrupoN).

/*
    valido_visitar(+Grilla, +Fil, +Col, +Visitados, +Coordenada, -Result)
        -Result es la lista de las coordenadas ady que puedo visitar.
        -Se evalúa si ya están visitadas, si se encuentran en un borde y si coinciden en el valor.
*/
valido_visitar(Grilla, Fil,Col,Visitados,Pos,Result):-
    %ady([Fila,Columna],Ady), 
    %valorEnCoordenada(Grilla, Col, [Fila,Columna], Valor),
    posiciones_ady(Pos,Col, Fil, Ady), 
    nth0(Pos, Grilla, Valor),
    findall(
        P,
        (
            member(P,Ady), 
            valido_visitar_aux(Grilla, Fil, Col, Visitados, P, Valor)
        ), 
        Result
    ).


%Devuelve True si La coordenada no está, visitada, si no se encuentra en un borde y si coincide en el valor.
valido_visitar_aux(Grilla, _, _, Visitados, Pos, Valor):-
    %coordenadaValida(Fil,Col,Pos),
    not(member(Pos, Visitados)),
    %valorEnCoordenada(Grilla, Col, Pos, Valor1),
    nth0(Pos, Grilla, Valor1),
    Valor=:=Valor1.

/*
    seleccionar_grupos(+Lista,NuevaLista)
        -Si la lista tiene un solo elemento, devuelve la lista vacía.
        -Se usa porque Clusters() devuelve como seleccionar_grupos las coordenadas que tienen un solo elemento,
        que no cuentan a la hora de agrupar
*/
seleccionar_grupos([_], []).
seleccionar_grupos([H|T], [H|T]).


 /*
    caminoMax
*/
movida_maxima(Grilla, Col, CaminoMax):-
    /*
        Paso 1: Consigo la cantidad de Filas:
    */
    length(Grilla, Size),
    Fil is Size/Col,

    caminos_posibles(0, Grilla, Size, Fil, Col, [], Caminos),

    camino_maximo(Grilla,Col,Caminos,CaminoMax1),
    reverse(CaminoMax1, CaminoMax).
    

/*
    clusters(+Grilla, +Fil, +Col, -Grupos)
        -Grupos es una lista de listas de coordenadas, conteniendo cada uno de los clusters de 
        ítems iguales.
*/
%Caso Base:
%caminoMax([],_,_,[]).

%Caso Recursivo:
% caminoMax([X|Xs],Fil, Col, CaminoMax):-

%   caminos_posibles([0,0], [X|Xs],Fil,Col,[], Grupos1),
%   delete(Grupos1, [], Grupos),
%   camino_maximo(Grilla, Col,Grupos, CaminoMax).


camino_maximo(Grilla, Col, Grupos, CaminoMax) :-
    valores_caminos(Grilla, Col, Grupos, ValorCaminos),
    max_list(ValorCaminos, ValorMax),
    nth0(IndiceMax, ValorCaminos, ValorMax),
    nth0(IndiceMax, Grupos, CaminoMax).
    
valores_caminos(_, _, [], []).
valores_caminos(Grilla, Col, [Camino|Resto], [Valor|ValorResto]) :-
    % length(Camino, CantidadCoordenadas),
    valor_ultimo_bloque_aux(Grilla, Col, Camino, Valor),
    valores_caminos(Grilla, Col, Resto,ValorResto).


/*
    caminos_posibles(+Coordenada, +Grilla, +Fil, +Col, +Visitados, -Result)
        -Result es la lista de todos los clusters en la grilla empezando por la coordenada dada.
        -Mantiene una lista visitados, que actualiza para verificar las siguientes coordenadas.
*/
%Caso Base:Pasé por todas las filas.
caminos_posibles(Tam, _,Tam, _,_,_, []).

%Caso Recursivo: La coordenada no fue visitada y es válida.
caminos_posibles(Pos,Grilla,Tam,Fil,Col, Visitados, [ClusterLimpio|Result]):-
    not(member(Pos,Visitados)),
    %coordenadaValida(Fil, Col,[Fila,Columna]),

    visitar_camino(Pos, Fil,Col,Grilla,Cluster),
    %El nuevo Cluster se marca como visitado porque todas sus coordenadas ya fueron visitadas.
    append(Visitados,Cluster,Visitados1),
    seleccionar_grupos(Cluster, ClusterLimpio), %Si el Cluster es de una sola coordenada, lo dejamos vacío.

    P is (Pos+1),
    caminos_posibles(P,Grilla,Tam,Fil,Col,Visitados1,Result).


% Caso Recursivo: La coordenada es válida pero fue Visitada previamente.
caminos_posibles(Pos,Grilla,Tam,Fil,Col, Visitados, Result):-
    member(Pos,Visitados),
    %coordenadaValida(Fil, Col,[Fila,Columna]),
    
    P is (Pos+1),
    caminos_posibles(P,Grilla,Tam,Fil,Col,Visitados,Result).

% Caso Recursivo: La columna no es válida, empiezo por la siguiente fila.
%caminos_posibles([Fila,Columna],Grilla,Fil,Col, Visitados, Result):-
%   not(coordenadaValida(Fil, Col,[Fila,Columna])),
%   Fila1 is Fila+1,
%   caminos_posibles([Fila1,0],Grilla,Fil,Col, Visitados, Result).


/*
    visitar_camino(+Coordenada, +Fil, +Col, +Grilla, +ListaVisitados, -Cluster)
        -Cluster es una lista de coordenadas ady con el mismo valor.
        -Marca como visitados los nodos a los que se puede mover y los agrega al cluster
        -NO actualiza la lista de visitados, sino que se debe usar Append a visitados con el Cluster
        al salir de la sentencia.
*/

visitar_camino(Pos, Fil,Col,Grilla,Maximo):-
    %Lista es una lista de coordenadas ady que se pueden visitar
    valido_visitar(Grilla, Fil, Col,[],Pos,Lista),
    visitar_camino_aux(Lista,Fil,Col,Grilla,[Pos], [], ColecFinal),
    camino_maximo(Grilla, Col, ColecFinal, Maximo).

/*
    visitar_camino_aux(+ListaCoordenadas, +Fil, +Col, +Grilla, +Visitados, +GrupoActual,GrupoN)
        - GrupoN es el grupo formado por las coordenadas ady del mismo valor.
        - Se inicia con GrupoActual vacío.
*/
%Caso base la lista a visitar está vacía:

visitar_camino_aux([],_,_,_,Grupo,Coleccion,[Grupo|Coleccion]).
    


%Caso Recursivo: el nodo no fue visitado.
visitar_camino_aux([PosActual|Resto],Fil,Col, Grilla,Grupo,Coleccion, ColecFinal):-
    not(member(PosActual, Grupo)),
    %Primero Visito la vecindad de la CoordenadaActual y  marco como visitados 
    valido_visitar_camino(Grilla, Fil, Col,Grupo,PosActual,Lista),
    %nth0(PosActual, Grilla, Valor),
    
    visitar_camino_aux(Lista,Fil,Col,Grilla, [PosActual|Grupo], Coleccion, ColeccionNueva1),
    
    %Después visito el resto de la lista
    visitar_camino_aux(Resto,Fil,Col,Grilla, Grupo, ColeccionNueva1,ColecFinal).



% Caso Recursivo: La coordenada ya fue visitada, sigo visitando las otras.
visitar_camino_aux([PosActual|Resto],Fil,Col, Grilla,Grupo, Coleccion, ColecFinal):-
    member(PosActual, Grupo),
    visitar_camino_aux(Resto,Fil,Col, Grilla,Grupo,Coleccion,ColecFinal).



/*s
    valido_visitar_camino(+Grilla, +Fil, +Col, +Visitados, +Coordenada, -Result)
        -Result es la lista de las coordenadas ady que puedo visitar (Para el Proyecto 2).
        -Se evalúa si ya están visitadas, si se encuentran en un borde y si coinciden en el valor.
*/
valido_visitar_camino(Grilla, Fil,Col,Grupo,Pos,Result):-
    posiciones_ady(Pos,Col, Fil, Ady), 
    nth0(Pos, Grilla, Valor),
    findall(
        P,
        (
            member(P,Ady), 
            valido_visitar_camino_aux(Grilla, Fil, Col, Grupo, P, Valor)
        ), 
        Result
    ).


%Devuelve True si La coordenada no está, visitada, si no se encuentra en un borde y si coincide en el valor.
valido_visitar_camino_aux(Grilla, _, _, Grupo, Pos,Valor):-
    %coordenadaValida(Fil,Col,[Fila,Columna]),
    not(member(Pos, Grupo)),
    %valorEnCoordenada(Grilla, Col, [Fila,Columna], Valor1),
    nth0(Pos, Grilla, Valor1),
    (
        Valor=:=Valor1;
        (
            potencia_dos(Valor, Siguiente),
            Valor1=:=Siguiente
        )
    ).

    
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

