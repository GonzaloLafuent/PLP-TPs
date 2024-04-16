import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}
{-
  En ambos tipos voy a recibir una función por cada constructor.

  En el caso de los constructores bases, son funciones van de los
  parámetros a un valor de tipo b.

  En el caso de los constructores recursivos, son funciones que
  toman el resultado de la recursión sobre y sus parámetros, y
  devuelven un valor de tipo b.

  Explico foldPersonaje usando posición_personaje

  posición_personaje :: Personaje -> Posición
  posición_personaje = foldPersonaje const const id
  
  => posición_personaje (Personaje pi n) = foldPersonaje const _ _ (Personaje pi n) =
    = const pi n = (const pi) n = pi.
  
  => posición_personaje (Mueve p d) = foldPersonaje const (\r d -> siguiente_posición r d) id (Mueve p d)
    = (\r d -> siguiente_posición r d) rec d = siguiente_posición rec d.
      where rec = foldPersonaje f_p f_mv f_mr p -- Recursión sobre p.
    
  => posición_personaje (Muere p) = foldPersonaje const (\r d -> siguiente_posición r d) id (Muere p d) =
    = id rec = rec.
      where rec = foldPersonaje f_p f_mv f_mr p -- Recurisión sobre p.

  Símil con foldObjeto.

-}

foldPersonaje :: (Posición -> String -> b) -> (b -> Dirección -> b) -> (b -> b) -> Personaje -> b
foldPersonaje casoInicial casoMovio casoMuere persona = case persona of
    Personaje pos name -> casoInicial pos name
    Mueve personaAntes dir -> casoMovio (rec personaAntes) dir
    Muere personaAntes -> casoMuere (rec personaAntes)
    where rec = foldPersonaje casoInicial casoMovio casoMuere 

foldObjeto :: (Posición -> String -> b) -> (b -> Personaje -> b) -> (b -> b) -> Objeto -> b
foldObjeto casoInicial casoTomado casoDestruido objeto = case objeto of
    Objeto pos name -> casoInicial pos name
    Tomado objetoAntes persona -> casoTomado (rec objetoAntes) persona
    EsDestruido objetoAntes -> casoDestruido (rec objetoAntes)
    where rec = foldObjeto casoInicial casoTomado casoDestruido 

{-Ejercicio 2-}

{-

  Se explica con la explicación del ejercicio anterior.

-}

posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje const siguiente_posición id

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (const id) const id

{-Ejercicio 3-}

{-

  rights devuelve toda aplicacion de right a una lista de datos either. lefts funciona de forma analoga

-}

objetos_en :: Universo -> [Objeto]
objetos_en universo = rights universo

personajes_en :: Universo -> [Personaje]
personajes_en universo = lefts universo

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de n u = if not(null u) &&  está_vivo (personaje_de_nombre n u) then filter (en_posesión_de n) (objetos_en u) else []

{-Ejercicio 5-}

{-

  Recursión estructural sobre [Objeto]. Devuelvo elemento que minimiza
  distancia a p.

-}

-- Asume que hay al menos un objeto
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano p u = foldr1 f (objetos_libres_en u)
  where
    f = \o rec -> if (distancia (Left p) (Right o)) < (distancia (Left p) (Right rec)) then
        o
      else
        rec

{-Ejercicio 6-}

tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = length (filter es_una_gema (objetos_en_posesión_de "Thanos" u)) == 6

{-Ejercicio 7-}

podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u = thanosNoTieneLasGemas && (thorPuedeVencer || wandaPuedeVencer)
  where
    thanosNoTieneLasGemas = not (tiene_thanos_todas_las_gemas u)
    thorPuedeVencer = (está_el_personaje "Thor" u) && (está_el_objeto "StormBreaker" u)
    wandaPuedeVencer = (está_el_personaje "Wanda" u) && (está_el_personaje "Vision" u) && (en_posesión_de "Vision" (objeto_de_nombre "Gema de la Mente" u))

{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

{-Personajes-}
phil = Personaje (0,0) "Phil"
thor = Personaje (0,0) "Thor"
capitanAmerica = Personaje (0,0) "Capitan America"
wanda = Personaje (0,0) "Wanda"
vision = Personaje (0,0) "Vision"
ironMan = Personaje (0,0) "Iron Man"
thanos = Personaje (0,0) "Thanos"

{-Objetos-}
mjölnir = Objeto (3,3) "Mjölnir"
escudo = Objeto (3,3) "Escudo"
casco = Objeto (8,9) "Casco"
espada = Objeto (2,1) "Espada"
arco = Objeto (7,5) "Arco"

stormBreaker = Objeto (3,1) "StormBreaker"

{-Gemas-}
gemaDeLamente = Objeto (5,4) "Gema de la Mente"
gemaDeLaRealidad  = Objeto (4,2) "Gema de la Realidad"
gemaDeEspacio = Objeto (5,1) "Gema de Espacio"
gemaDePoder = Objeto (2,2) "Gema de Poder"
gemaDeTiempo = Objeto (4,4) "Gema de tiempo"
gemaDeAlma = Objeto (3,3) "Gema de Alma"

{-Universos-}
universoVacio = universo_con [] []
universoThanosTieneTodasLasGemas = universo_con [
                                                  thanos,
                                                  wanda,
                                                  vision,
                                                  ironMan
                                                  ] 
                                                [
                                                  (Tomado stormBreaker thor),
                                                  (Tomado gemaDeAlma thanos),
                                                  (Tomado gemaDeEspacio thanos),
                                                  (Tomado gemaDeLaRealidad thanos),
                                                  (Tomado gemaDeLamente thanos),
                                                  (Tomado gemaDePoder thanos),
                                                  (Tomado gemaDeTiempo thanos),
                                                  escudo,
                                                  arco,
                                                  mjölnir
                                                ]

universoThanosMuerto = universo_con [
                                                  (Muere thanos),
                                                  wanda,
                                                  vision,
                                                  ironMan
                                                  ] 
                                                [
                                                  (Tomado stormBreaker thor),
                                                  (Tomado gemaDeAlma thanos),
                                                  (Tomado gemaDeEspacio thanos),
                                                  (Tomado gemaDeLaRealidad thanos),
                                                  (Tomado gemaDeLamente thanos),
                                                  (Tomado gemaDePoder thanos),
                                                  (Tomado gemaDeTiempo thanos),
                                                  escudo,
                                                  arco,
                                                  mjölnir
                                                ]

universoThanosNoTieneTodasLasGemas = universo_con [
                                                  thanos,
                                                  wanda,
                                                  vision,
                                                  ironMan
                                                  ] 
                                                [
                                                  (Tomado escudo ironMan),
                                                  (Tomado gemaDeAlma thanos),
                                                  (Tomado gemaDeEspacio thanos),
                                                  (Tomado gemaDeLaRealidad thanos),
                                                  (Tomado gemaDeLamente thanos),
                                                  casco,
                                                  mjölnir,
                                                  espada
                                                ]

universoGananPorThor = universo_con [
                              thanos,
                              thor  
                            ] 
                            [
                              (Tomado stormBreaker thor),
                              mjölnir
                            ]
universoGananPorWanda = universo_con [
                                vision,
                                wanda,
                                thanos
                             ]
                             [
                                (Tomado gemaDeLamente vision),
                                (Tomado gemaDeLaRealidad thanos),
                                mjölnir
                             ]                             

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1                                                              -- Caso de test 2 - resultado esperado
  
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  --Test Posicion inicial
  posición_personaje thor       
    ~=? (0,0)                     
  ,
  --Test el orden de los movimientos no afecta la posicion final
  posición_personaje (Mueve (Mueve (Mueve thor Norte) Este) Norte)
    ~=? (1,2)
  ,
  posición_personaje (Mueve (Mueve (Mueve thor Este) Norte) Norte)
    ~=? (1,2)
  ,
  posición_personaje (Mueve (Mueve (Mueve thor Norte) Norte) Este)
    ~=? (1,2)
  ,
  --Test pocion al morir
  posición_personaje (Muere thor)
    ~=? (0,0)
  ,
  posición_personaje (Mueve (Mueve (Muere thor) Este) Este) 
    ~=? (2,0)
  , 
  --Test movimientos a seguidos a una misma direccion    
  posición_personaje (Mueve (Mueve (Mueve thor Norte) Norte) Norte)
    ~=? (0,3)
  ,   
  posición_personaje (Mueve (Mueve (Mueve thor Sur) Sur) Sur)
    ~=? (0,-3)
   ,   
  posición_personaje (Mueve (Mueve thor Oeste) Oeste) 
    ~=? (-2,0)
  ,  
  posición_personaje (Mueve (Mueve thor Este) Este) 
    ~=? (2,0)         
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  {-Test objetos_en-}
  --Caso universo sin todas las gemas
  objetos_en universoThanosNoTieneTodasLasGemas 
    ~=? [
      (Tomado escudo ironMan),
      (Tomado gemaDeAlma thanos),
      (Tomado gemaDeEspacio thanos),
      (Tomado gemaDeLaRealidad thanos),
      (Tomado gemaDeLamente thanos),
      casco,
      mjölnir,
      espada
    ]
  ,
  --Caso el universo con todas las gemas
  objetos_en universoThanosTieneTodasLasGemas                
    ~=? [
          (Tomado stormBreaker thor),
          (Tomado gemaDeAlma thanos),
          (Tomado gemaDeEspacio thanos),
          (Tomado gemaDeLaRealidad thanos),
          (Tomado gemaDeLamente thanos),
          (Tomado gemaDePoder thanos),
          (Tomado gemaDeTiempo thanos),
          escudo,
          arco,
          mjölnir
        ]                               
  ,
  --Caso con menos objetos
  objetos_en universoGananPorThor 
    ~=? [(Tomado stormBreaker thor),mjölnir]
  ,
  objetos_en universoGananPorWanda 
    ~=? [(Tomado gemaDeLamente vision),(Tomado gemaDeLaRealidad thanos),mjölnir] 
  ,
  --Caso universo sin objetos
  objetos_en universoVacio  
    ~=? []
  {-Test personajes_en-}
  ,
  personajes_en universoThanosTieneTodasLasGemas
    ~=? [thanos,wanda,vision,ironMan]
  ,
  personajes_en universoThanosNoTieneTodasLasGemas
    ~=? [thanos,wanda,vision,ironMan]
  ,
  personajes_en universoGananPorThor
    ~=? [thanos,thor]  
  ,
  personajes_en universoGananPorWanda
    ~=? [vision,wanda,thanos]
  ,
  --Caso el universo sin personajes
  personajes_en universoVacio
    ~=? []
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  --Caso thanos posee todas las gemas
  objetos_en_posesión_de "Thanos" universoThanosTieneTodasLasGemas            
    ~=? [(Tomado gemaDeAlma thanos),
         (Tomado gemaDeEspacio thanos),
         (Tomado gemaDeLaRealidad thanos),
         (Tomado gemaDeLamente thanos),
         (Tomado gemaDePoder thanos),
         (Tomado gemaDeTiempo thanos)]                                                      
  ,
  --Caso Thor posee el stormBreaker
  objetos_en_posesión_de "Thor" universoGananPorThor
    ~=? [(Tomado stormBreaker thor)]
  ,
  --Caso vision posee la gema de la mente
  objetos_en_posesión_de "Vision" universoGananPorWanda
    ~=? [(Tomado gemaDeLamente vision)]
  ,
  --Caso el personaje no posee objetos  
  objetos_en_posesión_de "IronMan" universoThanosTieneTodasLasGemas      
    ~=? []
  ,  
  --Caso el universo esta vacio de objetos y personajes
  objetos_en_posesión_de "Thor" universoVacio
    ~=? []   
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  --Caso objeto mas cercano desde la pocision inicial
  objeto_libre_mas_cercano ironMan universoThanosTieneTodasLasGemas
    ~=? mjölnir
  ,
  --Caso objeto mas cercano desplazando al personaje
  objeto_libre_mas_cercano (Mueve(Mueve ironMan Norte)Este) universoThanosNoTieneTodasLasGemas
    ~=? espada

  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  --Caso Thanos posee las 6 gemas
  tiene_thanos_todas_las_gemas universoThanosNoTieneTodasLasGemas       
    ~=? False                                                           
  ,
  --Caso Thanos no posee ninguna gema
  tiene_thanos_todas_las_gemas universoThanosTieneTodasLasGemas
    ~=? True
  ,
  --Caso No hay ningun personaje ni objeto
  tiene_thanos_todas_las_gemas universoVacio
    ~=? False
  ,
  --Caso Thanos posee alguna gema
  tiene_thanos_todas_las_gemas universoGananPorThor
    ~=? False    
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  --Caso Thanos posee todas las gemas
  podemos_ganarle_a_thanos universoThanosTieneTodasLasGemas        
    ~=? False                                          
  ,
  --Caso Thanos no posee todas las gemas y thor posee el stormBreaker
  podemos_ganarle_a_thanos universoGananPorThor
    ~=? True
  ,
  --Caso Thanos no posee todas las gemas y esta wanda y vision posee la gema de la mente
  podemos_ganarle_a_thanos universoGananPorWanda
    ~=? True
  ,
  --Caso si el universo es vacio
  podemos_ganarle_a_thanos universoVacio
    ~=? False      
  ]
