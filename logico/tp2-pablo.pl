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
ocupar(P,TAB) :- posTablero(P,TAB,E), var(E), E=ocupar.

%% posTablero(?Pos,?Tablero,?Elem)
posTablero(pos(I,J),TAB,E) :- nth0(I,TAB,F), nth0(J,F,E), esTablero(TAB).

%% esTablero(?Tablero) será verdadero cuando el Tablero tenga filas de mismo largo.
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
vecino(PI,TAB,PV) :- posicionValida(PI,TAB), distDesde(PI,PV,1), posicionValida(PV,TAB).

%% posicionValida(+pos(I,J),+Tablero) será verdadero cuando la posición este dentro del tablero
posicionValida(P,TAB) :- posTablero(P,TAB,_).

%% distDesde(+posicioInicial, ?posicionFinal, ?dist) sera verdadero cuando la posicionFinal este en un eje
%% a dist desde la otra
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II+D, JF is JI.
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II-D, JF is JI.
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II, JF is JI+D.
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II, JF is JI-D.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(PI,TAB,PV) :- posicionValida(PI,TAB), vecino(PI,TAB,PV), posicionLibre(PV,TAB).

%% posicionLibre(?posición,?Tablero) valido cuando la posición se encuentre en el tablero y es libre. 
posicionLibre(P,TAB) :- posTablero(P,TAB,E), var(E).

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
camino(PI,PF,TAB,CAM) :- caminoYVisitados(PI,PF,TAB,CAM,[PI]).

%% caminoYVisitados(+Inicio, +Fin, +Tablero, -Camino, +Visitadas) será verdadero en los mismos casos que camino
%% pero registra las posiciones visitadas y no las pisa.
caminoYVisitados(PF,PF,TAB,[PF],_) :- posicionValida(PF,TAB).
caminoYVisitados(PI,PF,TAB,[PI,P2|CAM],V) :- vecinoLibre(PI,TAB,P2), not(member(P2,V)), 
                                             caminoYVisitados(P2,PF,TAB,[P2|CAM],[P2|V]).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace
%% Los parametros fin y camino son reversibles tanto juntos como separados.
%%   -Camino sin instanciar, va explorando vecinos de la ultima posicion valida visitada hasta encontrarse con la final
%%   -Fin sin instanciar, ira asociando posiciones inicio al valor de la cabeza de camino hasta que llegue a la ultima 
%%    y unifique cabeza con posicion final
%%   -Ambos sin instanciar, empezando con el camino que es solo Inicio ira creando caminos explorando por vecinos validos 
%%    y por cada exploracion lo dara como una salida valida con Fin como la ultima posicion visitada



%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.
camino2(PI,PF,T,C) :- caminoDeLargoM(PI,PF,T,C,0).

%% caminoDeLargoM(+Inicio, +Fin, +Tablero, -Camino, +LargoMinimo) será verdadero cuando encuentre un camino con un largo 
%% de como minimo LargoMinimo
caminoDeLargoM(PI,PF,T,C,L) :- camino(PI,PF,T,C), length(C,L).
caminoDeLargoM(PI,PF,T,C,L) :- tamanioTablero(T,MAX), L<MAX, L2 is L+1, caminoDeLargoM(PI,PF,T,C,L2).

%% tamanioTablero(+Tablero, -Area) será verdadero cuando el area corresponda al area del tablero
tamanioTablero([F|T],A) :- length(F,B), length([F|T],H), A is B*H.

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.
%% Los parametros son reversibles juntos o separados.
%% Inicio sin instanciar, 

%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(PI,PF,T,C) :- camino(PI,PF,T,C), length(C,L), not((camino(PI,PF,T,C2), length(C2,L2), L2<L)).

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

tablero(tablero2x2,T) :- tablero(2,2,T).
tablero(tablero3x3,T) :- tablero(3,3,T).
tablero(tablero5x5,T) :- tablero(5,5,T).
tablero(tablero0x0,T) :- tablero(0,0,T).

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