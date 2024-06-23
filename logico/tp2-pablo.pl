%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(0,_,[]).
tablero(FS,CS,[F|T]) :- FS > 0, fila(CS,F), FS2 is FS-1, tablero(FS2,CS,T).

%% fila(+Columnas,-Fila) instancia una estructura de fila de Columnas
%% con todas las celdas en blanco
fila(0,[]).
fila(CS,[_|FS]) :- CS > 0, CS2 is CS-1, fila(CS2,FS).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(P,T) :- posTablero(P,T,ocupada).

%% posTablero(?Pos,?Tablero,?Elem)
posTablero(pos(I,J),T,E) :- nth0(I,T,F), nth0(J,F,E), esTablero(T).

%% esTablero(?Tablero) será verdadero cuando el Tablero tenga filas de mismo largo.
esTablero(T) :- mismoLargo(T,_).

%% mismoLargo(?T,?L) será verdadero cuando todas las filas tengan mismo largo.
mismoLargo([],_).
mismoLargo([F|T],L) :- var(F), mismoLargo(T,L), length(F,L).
mismoLargo([F|T],L) :- nonvar(F), length(F,L), mismoLargo(T,L).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(PI,T,PV) :- posicionValida(PI,T), distDesde(PI,PV,1), posicionValida(PV,T).

%% posicionValida(+pos(I,J),+Tablero) será verdadero cuando la posición este dentro del tablero
posicionValida(P,T) :- posTablero(P,T,_).

%% distDesde(+posicioInicial, ?posicionFinal, ?dist) sera verdadero cuando la posicionFinal este en un eje
%% a dist desde la otra
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II+D, JF is JI.
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II-D, JF is JI.
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II, JF is JI+D.
distDesde(pos(II,JI),pos(IF,JF),D) :- IF is II, JF is JI-D.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(PI,T,PV) :- posicionValida(PI,T), vecino(PI,T,PV), posicionLibre(PV,T).

%% posicionLibre(?posición,?Tablero) valido cuando la posición se encuentre en el tablero y es libre. 
posicionLibre(P,T) :- posTablero(P,T,E), var(E).

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
camino(PI,PF,T,CAM) :- caminoYVisitados(PI,PF,T,CAM,[PI]).

%% caminoYVisitados(+Inicio, +Fin, +Tablero, -Camino, +Visitadas) será verdadero en los mismos casos que camino
%% pero registra las posiciones visitadas y no las pisa.
caminoYVisitados(PF,PF,T,[PF],_) :- posicionValida(PF,T).
caminoYVisitados(PI,PF,T,[PI,P2|CAM],V) :- vecinoLibre(PI,T,P2), not(member(P2,V)), 
                                           caminoYVisitados(P2,PF,T,[P2|CAM],[P2|V]).

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
camino2(PI,PF,T,C) :- tamanioTablero(T,MAX), caminoDeLargoM(PI,PF,T,C,0,MAX).

%% caminoDeLargoM(+Inicio, +Fin, +Tablero, -Camino, +LargoMinimo, +LargoMaximo) será verdadero cuando encuentre un camino
%% con un largo de como minimo LargoMinimo y como maximo LargoMaximo
caminoDeLargoM(PI,PF,T,C,MIN,_) :- length(C,MIN), camino(PI,PF,T,C).
caminoDeLargoM(PI,PF,T,C,MIN,MAX) :- MIN<MAX, MIN2 is MIN+1, caminoDeLargoM(PI,PF,T,C,MIN2,MAX).

%% tamanioTablero(+Tablero, -Area) será verdadero cuando el area corresponda al area del tablero
tamanioTablero([F|T],A) :- length(F,B), length([F|T],H), A is B*H.

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.
%% Los parametros son reversibles juntos o separados.
%% Inicio sin instanciar, 

%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(PI,PF,T,C) :- caminoOptimoAux(PI,PF,T,C,0).

caminoOptimoAux(PI,PF,T,C,L) :- (length(C2,L), camino(PI,PF,T,C2) -> 
                                       (length(C,L), camino(PI,PF,T,C))
                                       ;
                                       (L2 is L+1, caminoOptimoAux(PI,PF,T,C,L2))).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
caminoDual(PI,PF,T1,T2,C) :- camino(PI,PF,T1,C), camino(PI,PF,T2,C).

%%%%%%%%
%% TESTS
%%%%%%%%

%% Instancia un tablero de 2X2 sin posiciones ocupadas
tablero(tablero2x2,T) :- tablero(2,2,T).

%% Instancia un tablero de 3X3 sin posiciones ocupadas
tablero(tablero3x3,T) :- tablero(3,3,T).

%% Instancia un tablero de 5X5 sin posiciones ocupadas
tablero(tablero5x5,T) :- tablero(5,5,T).

%% Instancia un tablero de 0X0 
tablero(tablero0x0,T) :- tablero(0,0,T).

cantidadTestsTablero(8). 
%% Tests de instanciacion de tableros sin posiciones ocupadas
testTablero(1) :- tablero(tablero0x0,[]).
testTablero(2) :- tablero(tablero2x2,[[_,_],[_,_]]).
testTablero(3) :- tablero(tablero3x3,[[_,_,_],[_,_,_],[_,_,_]]).
testTablero(4) :- tablero(tablero5x5,[[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_],[_,_,_,_,_]]).

%% Tests de instanciacion de tableros con posiciones ocupadas
testTablero(5) :- ocupar(pos(0,0), T), T = [[V]], nonvar(V).
testTablero(6) :- tablero(tablero3x3,T), ocupar(pos(1,1),T), ocupar(pos(0,0),T),
                  T = [[V1,_,_],[_,V2,_],[_,_,_]], nonvar(V1), nonvar(V2).
testTablero(7) :- tablero(tablero2x2,T), ocupar(pos(0,0),T), ocupar(pos(1,0),T),
                  T = [[V1,_],[V2,_]], nonvar(V1), nonvar(V2).
testTablero(8) :- tablero(tablero5x5,T), ocupar(pos(4,1),T),ocupar(pos(2,2),T), ocupar(pos(2,1),T),
                  T = [[_,_,_,_,_],[_,_,_,_,_],[_,V1,V2,_,_],[_,_,_,_,_],[_,V3,_,_,_]],
                  nonvar(V1), nonvar(V2), nonvar(V3).

cantidadTestsVecino(6). 
%% Tests vecino
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
testVecino(2) :- tablero(tablero2x2,T), vecino(pos(1,1),T,pos(1,0)),vecino(pos(1,1),T,pos(0,1)).
testVecino(3) :- tablero(tablero3x3,T), vecino(pos(1,1),T,pos(0,1)), vecino(pos(1,1),T,pos(2,1)),
                 vecino(pos(1,1),T,pos(1,0)), vecino(pos(1,1),T,pos(1,2)).

%% Tests vecino libre
testVecino(4) :- tablero(tablero2x2,T), ocupar(pos(0,1),T), vecinoLibre(pos(0,0),T,pos(1,0)).
testVecino(5) :- not((tablero(tablero2x2,T), ocupar(pos(0,1),T),ocupar(pos(1,0),T), 
                      vecinoLibre(pos(0,0),T,pos(0,1)), vecinoLibre(pos(0,0),T,pos(1,0)))). 
testVecino(6) :- tablero(tablero3x3,T), ocupar(pos(0,1),T), ocupar(pos(2,1),T), 
                 vecinoLibre(pos(1,1),T,pos(1,0)), vecinoLibre(pos(1,1),T,pos(1,2)).
testVecino(7) :- tablero(tablero5x5,T), vecinoLibre(pos(4,4),T,pos(4,3)), vecinoLibre(pos(4,4),T,pos(3,4)).

cantidadTestsCamino(6).
%% Tests camino
testCamino(1) :- not((tablero(tablero0x0,T),camino(pos(0,0),pos(0,0),T,[pos(0,0)]))).
testCamino(2) :- tablero(tablero2x2,T), camino(pos(0,0),pos(0,0),T,C), C = [pos(0,0)].
testCamino(3) :- tablero(tablero2x2,T), camino(pos(0,0),pos(1,1),T, C1), C1 = [pos(0,0),pos(0,1),pos(1,1)],
                 camino(pos(0,0),pos(1,1),T,C2), C2 = [pos(0,0),pos(1,0),pos(1,1)].
testCamino(4) :- tablero(tablero3x3,T), ocupar(pos(1,1),T), 
                 camino(pos(0,0),pos(2,2),T,C1), C1 = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2)],
                 camino(pos(0,0),pos(2,2),T,C2), C2 = [pos(0, 0), pos(0, 1), pos(0, 2), pos(1, 2), pos(2, 2)].

%% Tests camino2
%% Test el primer camino que devuelve va a ser menor a todos los demas
testCamino(5) :- tablero(tablero2x2,T), camino2(pos(0,0),pos(0,1),T,C1),length(C1,N1),
                 not((camino(pos(0,0),pos(0,1),T,C2), length(C2,N2), N2 < N1)),!.

testCamino(6) :- tablero(tablero3x3,T), ocupar(pos(2,0),T), camino2(pos(1,1),pos(0,1),T,C1),length(C1,N1),
                 not((camino(pos(0,0),pos(0,1),T,C2), length(C2,N2), N2 < N1)),!.
                 

cantidadTestsCaminoOptimo(4).
%% Test instancia un camino minimo correcto
testCaminoOptimo(1) :- tablero(tablero3x3,T), ocupar(pos(1,1),T), 
                       caminoOptimo(pos(0,0),pos(2,2),T,C1), C1 = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2)],
                       caminoOptimo(pos(0,0),pos(2,2),T,C2), C2 = [pos(0, 0), pos(0, 1), pos(0, 2), pos(1, 2), pos(2, 2)].

%% Test el camino instanciado es el camino mas corto posible                       
testCaminoOptimo(2) :- tablero(tablero3x3,T),caminoOptimo(pos(0,0),pos(1,1),T,C1), length(C1,N1), N1 is 3,
                       not((camino(pos(0,0),pos(1,1),T,C2), length(C2,N2), N2 < N1)).
testCaminoOptimo(3) :- tablero(tablero3x3,T),caminoOptimo(pos(0,0),pos(2,2),T,C1), length(C1,N1), N1 is 5,
                       not((camino(pos(0,0),pos(2,2),T,C2), length(C2,N2), N2 < N1)).
testCaminoOptimo(4) :- tablero(tablero5x5,T), caminoOptimo(pos(2,3),pos(2,4),T,C1), length(C1,N1), N1 is 2,
                       not((camino(pos(2,3),pos(2,4),T,C2), length(C2,N2), N2 < N1)).

cantidadTestsCaminoDual(3).
%% Test No hay camino dual.
testCaminoDual(1) :- not((tablero(tablero2x2,T1), tablero(tablero2x2,T2), ocupar(pos(1,0),T2), 
                     caminoDual(pos(0,0),pos(1,1),T1,T2,[pos(0,0),pos(1,0),pos(1,1)]))).
%% Test camino Dual presente                     
testCaminoDual(2) :- tablero(tablero2x2,T1), ocupar(pos(1,0),T1), tablero(tablero2x2,T2), ocupar(pos(1,0),T2),
                      caminoDual(pos(0,0),pos(1,1),T1,T2,C1), C1 = [pos(0,0),pos(0,1),pos(1,1)],
                      not((caminoDual(pos(0,0),pos(1,1),T1,T2,C2),C2 \= C1)).

testCaminoDual(3) :- tablero(tablero3x3,T1), ocupar(pos(0,1),T1), ocupar(pos(1,1),T1), tablero(tablero3x3,T2), 
                    caminoDual(pos(0,0),pos(2,1),T1,T2,C1), C1 = [pos(0,0),pos(1,0),pos(2,0),pos(2,1)],
                    not((caminoDual(pos(0,0),pos(2,1),T1,T2,C2),C2 \= C1)).
                    

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