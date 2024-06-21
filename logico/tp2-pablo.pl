%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(0,_,[]).
tablero(FS,CS,[F|TAB]) :- FS > 0, fila(CS,F), FS2 is FS-1, tablero(FS2,CS,TAB).

%% fila(+Columnas,-Fila) instancia una estructura de fila de Columnas
%% con todas las celdas en blanco
fila(0,[]).
fila(CS,[_|FS]) :- CS > 0, CS2 is CS-1, fila(CS2,FS).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(0,J),[F|TAB]) :- ocuparFila(J,F), esTablero([F|TAB]).
ocupar(pos(I,J),[F|TAB]) :- I>0, I2 is I-1, ocupar(pos(I2,J),TAB), esTablero([F|TAB]).

%% ocuparFila(+J,?Fila) será verdadero cuando la columna J esté ocupada.
ocuparFila(0,[ocupar|F]) :- length(F,L), L>=0.
ocuparFila(J,[_|FS]) :- J>0, J2 is J-1, ocuparFila(J2,FS).

%% esTablero(?Tablero) será verdadero cuando el Tablero tenga filas de mismo largo.
esTablero([]).
esTablero(TAB) :- mismoLargo(TAB,_).

%% mismoLargo(?TAB,?L) será verdadero cuando todas las filas tengan mismo largo.
mismoLargo([],_).
mismoLargo([F|TAB],L) :- var(F), mismoLargo(TAB,L), length(F,L).
mismoLargo([F|TAB],L) :- nonvar(F), length(F,L), mismoLargo(TAB,L).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(PI,[F|TAB],pos(IV,JV)) :- distDesde(PI,pos(IV,JV),1), IV>=0, JV>=0,
                                 length([F|TAB],LF), IV<LF, 
                                 length(F,LC), JV<LC.

%% distDesde(+posicioInicial, ?posicionFinal, ?dist) sera verdadero cuando la posicionFinal este en un eje
%% a dist desde la otra
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II+D, JF is JI.
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II-D, JF is JI.
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II, JF is JI+D.
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II, JF is JI-D.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(_,_,_).

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
camino(_,_,_,_).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace



%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.
camino2(_,_,_,_).

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