%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

fila(1,[_]).
fila(N,[_|XS]) :- N\=1, N1 is N-1, fila(N1,XS). 

tablero(1,CC,[TAB]) :- fila(CC,TAB).
tablero(CF,CC,[F|TAB]) :- CF\=1,fila(CC,F), CF1 is CF-1, tablero(CF1,CC,TAB).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(X,Y),TAB) :- nth0(X,TAB,FX,RTAB), nth0(Y,FX,_,RF), nth0(Y,FN,ocupada,RF), nth0(X,TAB,FN,RTAB).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

posicionValida(pos(X,Y),[F|TAB]) :- 0=<X, 0=<Y , length([F|TAB],CF), length(F,CC), CF > X, CC > Y. 

vecino(pos(X,Y),T,pos(Z,W)) :- Z is X+1, W is Y, posicionValida(pos(Z,W),T).
vecino(pos(X,Y),T,pos(Z,W)) :- Z is X-1, W is Y, posicionValida(pos(Z,W),T).
vecino(pos(X,Y),T,pos(Z,W)) :- Z is X, W is Y+1, posicionValida(pos(Z,W),T).
vecino(pos(X,Y),T,pos(Z,W)) :- Z is X, W is Y-1, posicionValida(pos(Z,W),T).

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero

vecinoLibre(pos(X,Y),T,pos(Z,W)) :- vecino(pos(X,Y),T,pos(Z,W)), nth0(Z,T,F,_), nth0(W,F,E,_).  

%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas

/*
  camino y camino2 no estan probados que funcionan, solo es un intento de algo que podria tener sentido
*/
caminoAux(pos(XF,YF),pos(XF,YF),T,[pos(XF,YF)],_).
caminoAux(pos(XI,YI),pos(XF,YF),T,[pos(XI,YI)|C],[pos(XI,YI)|VISIT]) :- vecinoLibre(pos(X,Y),T,V) ,caminoAux(V,T,C,VISIT), not(member(pos(X,Y),VISIT)).

camino(pos(XI,YI),pos(XF,YF),T,C) :- caminoAux(pos(XI,YI),pos(XF,YF),T,VISIT).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace



%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.
camino2(INIT,FIN,T,C) :- camino(INIT,FIN,T,C1), sort(C1,C).

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.


%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
caminoDual(_,_,_,_,_).

%%%%%%%%
%% TESTS
%%%%%%%%

cantidadTestsTablero(2). % Actualizar con la cantidad de tests que entreguen
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
% Agregar más tests

cantidadTestsVecino(1). % Actualizar con la cantidad de tests que entreguen
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
% Agregar más tests

cantidadTestsCamino(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsCaminoOptimo(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsCaminoDual(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual).

tests :- tests(todos).