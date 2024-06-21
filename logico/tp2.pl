%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

%% fila(+CantPosiciones,-Fila) instancia una estructura de una fila de tamaño CantPosiciones para la creacion de un tablero.
fila(1,[_]).
fila(CantPosiciones,[_|RestoFila]) :- CantPosiciones\=1, PosicionesPorAgregar is CantPosiciones-1, fila(PosicionesPorAgregar,RestoFila). 

tablero(0,_,[]).
tablero(CantFilas,CantColumns,[Fila|RestoTablero]) :- CantFilas\=0,fila(CantColumns,Fila), 
                                                      FilasPorAgregar is CantFilas-1, tablero(FilasPorAgregar,CantColumns,RestoTablero).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
 
esTablero([]).
esTablero(TAB) :- mismoLargo(TAB,_).

%% mismoLargo(?TAB,?L) será verdadero cuando todas las filas tengan mismo largo.
mismoLargo([],_).
mismoLargo([F|TAB],L) :- var(F), mismoLargo(TAB,L), length(F,L).
mismoLargo([F|TAB],L) :- nonvar(F), length(F,L), mismoLargo(TAB,L).

ocupar(pos(X,Y),Tablero) :- nth0(X,Tablero,FilaX,RestoTablero), nth0(Y,FilaX,E,RestoFila),
                            nth0(Y,FilaNueva,ocupada,RestoFila), nth0(X,Tablero,FilaNueva,RestoTablero),
                            esTablero(Tablero), length(Tablero,N), N > 1.

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

%% posicionValida(+Posicion,+Tablero) sera verdadero si la posicion dada esta dentro de los limites del tablero.
posicionValida(pos(X,Y),[Fila|RestoTablero]) :- 0=<X, 0=<Y , length([Fila|RestoTablero],CantFilas), length(Fila,CantColums), CantFilas > X, CantColums > Y. 

vecino(pos(X1,Y1),Tablero,pos(X2,Y2)) :- posicionValida(pos(X1,Y1),Tablero), X2 is X1 + 1, Y2 is Y1, posicionValida(pos(X2,Y2),Tablero).
vecino(pos(X1,Y1),Tablero,pos(X2,Y2)) :- posicionValida(pos(X1,Y1),Tablero), X2 is X1 - 1, Y2 is Y1, posicionValida(pos(X2,Y2),Tablero).
vecino(pos(X1,Y1),Tablero,pos(X2,Y2)) :- posicionValida(pos(X1,Y1),Tablero), X2 is X1, Y2 is Y1 + 1, posicionValida(pos(X2,Y2),Tablero).
vecino(pos(X1,Y1),Tablero,pos(X2,Y2)) :- posicionValida(pos(X1,Y1),Tablero), X2 is X1, Y2 is Y1 - 1, posicionValida(pos(X2,Y2),Tablero).

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero

vecinoLibre(pos(X,Y),Tablero,pos(Xvecino,Yvecino)) :- vecino(pos(X,Y),Tablero,pos(Xvecino,Yvecino)), 
                                                      nth0(Xvecino,Tablero,Fila,_), nth0(Yvecino,Fila,E,_), var(E).  

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

%% generarCamino(+Inicio, +Fin, +Tablero, -Camino, +Visitados) permite generar un camino de inicio a fin manteniendo una lista de los nodos visitados en cada paso.
%% XI = valor de X en la posicion inicial. YI = valor de Y en la posicion incial.
%% XF = valor de X en la posicion final. YF = valor de Y en la posicion final.
%% XA = valor de X en la posicion actual. YA = valor de Y en la posicion actual.
generarCamino(pos(XF,YF),pos(XF,YF),_,[pos(XF,YF)],Visitados) :- not(member(pos(XF,YF),Visitados)).
generarCamino(pos(XA,YA),pos(XF,YF),Tablero,[pos(XA,YA)|RestoCamino],Visitados) :- not(member(pos(XA,YA),Visitados)), vecinoLibre(pos(XA,YA),Tablero,ProxPosicion), 
                                                                                   append(Visitados,[pos(XA,YA)],VisitadosNuevo),
                                                                                   generarCamino(ProxPosicion,pos(XF,YF),Tablero,RestoCamino,VisitadosNuevo).

camino(pos(XF,YF),pos(XF,YF),Tablero,[pos(XF,YF)]) :- length(Tablero,Longitud), Longitud =\= 0.
camino(pos(XI,YI),pos(XF,YF),Tablero,[pos(XI,YI)|Camino]) :- vecinoLibre(pos(XI,YI),Tablero,ProxPosicion), 
                                                             generarCamino(ProxPosicion,pos(XF,YF),Tablero,Camino,[pos(XI,YI)]).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace



%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

%% longitudCaminoMaximo(+Inicio, +fin, +Tablero, -Longitud) instancia la longitud del camino maximo desde incio a fin dentro de un tablero. 
longitudCaminoMaximo(Inicio,Fin,Tablero,MaxLongitud) :- camino(Inicio,Fin,Tablero,Camino), length(Camino,MaxLongitud), 
                                                        not((camino(Inicio,Fin,Tablero,OtroCamino),length(OtroCamino,Longitud),MaxLongitud < Longitud)), !.

%% Utilizo Generate and Test. Para crear los caminos de forma creciente, voy generando las posibles longitudes de caminos para esa posicion de inicio y fin,
%% luego por cada longitud que genero me fijo si hay un camino valido con esa longitud.
camino2(Inicio,Fin,Tablero,Camino) :- longitudCaminoMaximo(Inicio,Fin,Tablero,MaxLongitud), between(1,MaxLongitud,Longitud), 
                                      camino(Inicio,Fin,Tablero,Camino), length(Camino,Longitud). 

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.

%% En el caso del parametro Fin. tomando con instanciados los parametros Inicio, Tablero y Camino el predicado Camino2 fallara.
%% Dado que camino2 depende del predicao longitudCaminoMaximo el cual solicita que sus parametros de Inicio, Fin y Tablero, al no
%% estar instanciado Fin, el predicado actura de forma extraña. Intanciando el Parametro de Longitud en un valor que nos pertmitira
%% computar los valores de 

%% 


%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(Inicio,Fin,Tablero,Camino) :- camino(Inicio,Fin,Tablero,Camino), length(Camino,Longitud), 
                                           not((camino(Inicio,Fin,Tablero,OtroCamino),length(OtroCamino,LongitudOtroCamino), LongitudOtroCamino < Longitud)).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
caminoDual(Inicio,Fin,Tablero1,Tablero2,Camino) :- camino(Inicio,Fin,Tablero1,Camino), 
                                                   camino(Inicio,Fin,Tablero2,Camino2), append(Camino,[],Camino2).

%%%%%%%%
%% TESTS
%%%%%%%%

tablero(tablero2x2,T) :- tablero(2,2,T).
tablero(tablero3x3,T) :- tablero(3,3,T).
tablero(tablero5x5,T) :- tablero(5,5,T).
tablero(tablero0x0,T) :- tablero(0,0,T).

cantidadTestsTablero(8). % Actualizar con la cantidad de tests que entreguen
testTablero(1) :- tablero(tablero0x0,[]).
testTablero(2) :- tablero(tablero2x2,[[_,_],[_,_]]).
testTablero(3) :- tablero(tablero3x3,[[_,_,_],[_,_,_],[_,_,_]]).
testTablero(4) :- tablero(tablero5x5,[[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_]]).
testTablero(5) :- ocupar(pos(0,0), [[ocupada]]).
testTablero(6) :- tablero(tablero3x3,T), ocupar(pos(1,1),T), ocupar(pos(0,0),T),
                  T = [[ocupada,_,_],[_,ocupada,_],[_,_,_]].
testTablero(7) :- tablero(tablero2x2,T), ocupar(pos(0,0),T), ocupar(pos(1,0),T),
                  T = [[ocupada,_],[ocupada,_]].
testTablero(8) :- tablero(tablero5x5,T), ocupar(pos(4,1),T),ocupar(pos(2,2),T), ocupar(pos(2,1),T),
                  T = [[_,_,_,_,_],[_,_,_,_,_],[_,ocupada,ocupada,_,_],[_,_,_,_,_],[_,ocupada,_,_,_]].

cantidadTestsVecino(6). 
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
testVecino(2) :- tablero(tablero2x2,T), vecino(pos(1,1),T,pos(1,0)),vecino(pos(1,1),T,pos(0,1)).
testVecino(3) :- tablero(tablero3x3,T), vecino(pos(1,1),T,pos(0,1)), vecino(pos(1,1),T,pos(2,1)),
                 vecino(pos(1,1),T,pos(1,0)), vecino(pos(1,1),T,pos(1,2)).
testVecino(4) :- tablero(tablero2x2,T), ocupar(pos(0,1),T), vecinoLibre(pos(0,0),T,pos(1,0)).
testVecino(5) :- not((tablero(tablero2x2,T), ocupar(pos(0,1),T),ocupar(pos(1,0),T), 
                      vecinoLibre(pos(0,0),T,pos(0,1)), vecinoLibre(pos(0,0),T,pos(1,0)))). 
testVecino(6) :- tablero(tablero3x3,T), ocupar(pos(0,1),T), ocupar(pos(2,1),T), 
                 vecinoLibre(pos(1,1),T,pos(1,0)), vecinoLibre(pos(1,1),T,pos(1,2)).
testVecino(7) :- tablero(tablero5x5,T), vecinoLibre(pos(4,4),T,pos(4,3)), vecinoLibre(pos(4,4),T,pos(3,4)).

cantidadTestsCamino(3).
testCamino(1) :- not((tablero(tablero0x0,T), camino(pos(0,0),pos(0,0),T,[pos(0,0)]))).
testCamino(2) :- tablero(tablero2x2,T), camino(pos(0,0),pos(0,0),T,[pos(0,0)]).
testCamino(3) :- tablero(tablero2x2,T), camino(pos(0,0),pos(1,1),T,[pos(0,0),pos(0,1),pos(1,1)]),
                 camino(pos(0,0),pos(1,1),T,[pos(0,0),pos(1,0),pos(1,1)]).
testCamino(4) :- tablero(tablero3x3,T), ocupar(pos(1,1),T), 
                 camino(pos(0,0),pos(2,2),T,[pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2)]),
                 camino(pos(0,0),pos(2,2),T,[pos(0, 0), pos(0, 1), pos(0, 2), pos(1, 2), pos(2, 2)]).


cantidadTestsCaminoOptimo(3).
testCaminoOptimo(1) :- tablero(tablero3x3,T), ocupar(pos(1,1),T), 
                       caminoOptimo(pos(0,0),pos(2,2),T,[pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2)]),
                       caminoOptimo(pos(0,0),pos(2,2),T,[pos(0, 0), pos(0, 1), pos(0, 2), pos(1, 2), pos(2, 2)]).
testCaminoOptimo(2) :- tablero(tablero3x3,T),caminoOptimo(pos(0,0),pos(1,1),T,C), length(C,N), N is 3.
testCaminoOptimo(3) :- tablero(tablero3x3,T),caminoOptimo(pos(0,0),pos(2,2),T,C), length(C,N), N is 5.
testCaminoOptimo(4) :- tablero(tablero5x5,T), caminoOptimo(pos(2,3),pos(2,4),T,C), length(C,N), N is 2.

cantidadTestsCaminoDual(1).
testCaminoDual(1) :- not((tablero(tablero2x2,T1), tablero(tablero2x2,T2), ocupar(pos(1,0),T2), 
                     caminoDual(pos(0,0),pos(1,1),T1,T2,[pos(0,0),pos(1,0),pos(1,1)]))).

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