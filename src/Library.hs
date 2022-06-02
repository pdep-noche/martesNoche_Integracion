module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- 1
data Pelicula = Pelicula {nombreP :: String, genero ::String, duracion :: Number, origen :: String} deriving (Show, Eq)
psicosis = Pelicula "Psicosis" "Terror" 109 "Estados Unidos"
perfumeDeMujer= Pelicula "Perfume de Mujer" "Drama" 150  "Estados Unidos"
elSaborDeLasCervezas = Pelicula "El sabor de las cervezas"  "Drama" 95 "Iran"
lasTortugasTambienVuelan = Pelicula "Las tortugas también vuelan" "Drama" 103 "Iran"

data Usuario = Usuario {nombre :: String, categoria :: String, edad :: Number, paisResidencia :: String, peliculas :: [Pelicula], estadoSalud :: Number} deriving Show
juan = Usuario "juan" "estandar" 23  "Argentina" [perfumeDeMujer] 60
ana = Usuario "ana" "basica"  25 "Argentina" [elSaborDeLasCervezas,lasTortugasTambienVuelan] 70

peliculasEmpresa = [psicosis, perfumeDeMujer, elSaborDeLasCervezas, lasTortugasTambienVuelan]
-- 2
ver :: Pelicula -> Usuario -> Usuario
ver pelicula usuario = usuario { peliculas = peliculas usuario ++ [pelicula]}

-- 3
premiarUsuariosInternacionalesFieles :: [Usuario] -> [Usuario]
premiarUsuariosInternacionalesFieles usuarios = map premiarSiEsFiel usuarios


premiarSiEsFiel :: Usuario -> Usuario
premiarSiEsFiel usuario | cumpleCondiciones usuario = subirCategoria usuario
                        | otherwise = usuario

cumpleCondiciones :: Usuario -> Bool
cumpleCondiciones usuario = (>20).length.(peliculasQueNosSeanDe "Estados Unidos").peliculas $ usuario


peliculasQueNosSeanDe :: String -> [Pelicula] -> [Pelicula]
peliculasQueNosSeanDe pais peliculas = filter ((pais /=).origen) peliculas 

subirCategoria :: Usuario -> Usuario
subirCategoria usuario = usuario {categoria = (nuevaCategoria.categoria) usuario}

nuevaCategoria :: String -> String
nuevaCategoria "basica" = "estandar"
nuevaCategoria _  = "premium"


--4
type CriterioBusqueda = Pelicula -> Bool

teQuedasteCorto :: CriterioBusqueda
teQuedasteCorto pelicula = (<35).duracion $ pelicula

cuestionDeGenero :: [String] -> CriterioBusqueda
cuestionDeGenero generos pelicula =  elem (genero pelicula) generos

cuestionDeGenero' :: [String] -> CriterioBusqueda
cuestionDeGenero' generos pelicula = any (==(genero pelicula)) generos


deDondeSaliste :: String -> CriterioBusqueda
deDondeSaliste unOrigen pelicula = (== unOrigen).origen $ pelicula


vaPorEseLado :: (Eq t) => Pelicula -> (Pelicula -> t) -> CriterioBusqueda
vaPorEseLado pelicula caracteristica otraPelicula = (caracteristica pelicula) == (caracteristica otraPelicula)

{-
*Spec Library Spec> vaPorEseLado psicosis origen perfumeDeMujer
True
-}

--5
busquedaPeliculas :: Usuario -> [CriterioBusqueda] -> [Pelicula] -> [Pelicula]
busquedaPeliculas usuario criterios peliculas = take 3. filter (esRecomendablePara usuario criterios) $ peliculas

esRecomendablePara :: Usuario -> [CriterioBusqueda] -> Pelicula -> Bool
esRecomendablePara usuario criterios pelicula = cumpleCriterios pelicula criterios && (not.vio pelicula) usuario

vio::Pelicula -> Usuario -> Bool
vio pelicula usuario = elem pelicula (peliculas usuario)

cumpleCriterios :: Pelicula -> [CriterioBusqueda] -> Bool
cumpleCriterios pelicula criterios = all ($ pelicula)  criterios

{-
 busquedaPeliculas juan [deDondeSaliste "Iran", cuestionDeGenero ["Drama", "Comedia"], (not.teQuedasteCorto)] peliculasEmpresa
 -}

 --- Segunda Parte
--1
data Capitulo = Capitulo {nombreC :: String, generoS :: String, duracionC :: Number, origenS :: String, afecta :: (Usuario -> Usuario)} deriving Show


 --2
consumen :: Usuario -> Capitulo -> Usuario
consumen usuario capitulo = (afecta capitulo) usuario

--3

capitulo = Capitulo "Piloto - Dr House" "Drama" 30 "Estados Unidos" (\usuario -> usuario {estadoSalud = (estadoSalud usuario) - 20})

-- 4
type Serie = [Capitulo]

maraton :: Usuario -> Serie -> Usuario
maraton usuario serie = foldl  consumen  usuario serie

serieInfinita = cycle [capitulo]

serieInfinta' = repeat capitulo
{-
*Spec Library Spec> maraton juan (take 5 serieInfinita)
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisResidencia = "Argentina", peliculas = [Pelicula {nombreP = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"}], estadoSalud = -40}
-}