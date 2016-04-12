module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

split :: Eq a => a -> [a] -> [[a]]
split a = foldr (\x (z:zs) -> if (x==a) then []:z:zs else (x:z):zs) [[]]

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras xs = sumaLongitudes(split ' ' xs) / (genericLength (split ' ' xs))
sumaLongitudes = foldr (\x sumador -> genericLength x + sumador) 0

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = cantidadDeApariciones (elementosSinRepetir xs) xs

repeticionesPromedio :: Extractor
repeticionesPromedio xs = (fromIntegral $ length $ split ' ' xs) / (fromIntegral $ length $ elementosSinRepetir $ split ' ' xs)

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = map (\t -> (\x -> (fromIntegral (apariciones t x))/(fromIntegral (length x)) )) tokens

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor textos extractor = (/maximoValor textos extractor).extractor

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures extractores textos = foldr (\texto instancias -> (aplicarExtractores texto (normalizarExtractores textos extractores)):instancias) [] textos

distEuclideana :: Medida
distEuclideana = (\p q -> sqrt (sum (zipWith (\pv qv -> (pv-qv)*(pv-qv)) p q)))

distCoseno :: Medida
distCoseno = (\p q -> (sumProductoEscalar p q) / ((sqrt (sumProductoEscalar p p))*(sqrt (sumProductoEscalar p p))) )

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas norma = (\valor -> snd (mejor (cuentas (kMenores k datos etiquetas norma valor))))

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = (fromIntegral $ contarIguales $ zip xs ys) / (fromIntegral $ length xs) 

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = (fst(unzip(dameTrain (take ((calcularTamano datos n) * n) (zip datos etiquetas)) p (calcularTamano datos n))),
                        fst (unzip(dameVal (take ((calcularTamano datos n) * n) (zip datos etiquetas)) p (calcularTamano datos n))), 
                        snd(unzip(dameTrain (take ((calcularTamano datos n) * n) (zip datos etiquetas)) p (calcularTamano datos n))),
                        snd (unzip(dameVal (take ((calcularTamano datos n) * n) (zip datos etiquetas))  p (calcularTamano datos n))))


nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = (sum accuracies) / (fromIntegral n)
    where accuracyPorParticion particionDeValidacion = let datosSeparados = separarDatos datos etiquetas n particionDeValidacion
                                                           modeloK15 = (knn 15 (primero datosSeparados) (tercero datosSeparados) distEuclideana)
                                                           etiquetasObtenidas = foldr(\instancia acum -> (modeloK15 instancia):acum) []
                                                           in accuracy (cuarto datosSeparados) (etiquetasObtenidas (segundo datosSeparados))
          accuracies = [accuracyPorParticion x | x<-[1..n]]


-- *************** Funciones auxiliares ********************

cantidadDeApariciones::Eq a => [a]->([a]->[(Int,a)])
cantidadDeApariciones = foldr (\x contar -> (\ys->((repeticiones x ys),x):contar ys)) (\ys->[])

repeticiones::Eq a =>a->[a]->Int
repeticiones x = foldr (\y contar -> if x==y then (1+) contar else contar) 0

elementosSinRepetir :: Eq a => [a] -> [a]
elementosSinRepetir xs = (foldr(\x recu->(\ys -> if repeticiones x (tail ys) == 0 then x:recu (tail ys) else recu (tail ys))) (\ys->[]) xs) xs

apariciones :: Eq a => a -> [a] -> Int
apariciones a = foldr (\x xs -> if x==a then (1+xs) else xs) 0

maximoValor :: [Texto] -> Extractor -> Feature
maximoValor textos extractor = abs $ maximoAbsoluto $ ejecutarExtractor textos extractor

maximoAbsoluto :: [Feature] -> Feature
maximoAbsoluto = foldr (\x buscarMax -> if abs x >= abs buscarMax then abs x else abs buscarMax) 0

ejecutarExtractor:: [Texto] -> Extractor -> [Feature]
ejecutarExtractor textos extractor = map extractor textos

aplicarExtractores :: Texto -> [Extractor] -> Instancia
aplicarExtractores texto extractores = map (\extractor -> extractor texto) extractores

normalizarExtractores :: [Texto] -> [Extractor] -> [Extractor]
normalizarExtractores textos extractores = map (\ext -> normalizarExtractor textos ext) extractores 

-- Para ejercicio 8

sumProductoEscalar :: Instancia -> Instancia -> Feature
sumProductoEscalar p q = sum (zipWith (*) p q)


-- Para ejercicio 9

kMenores :: Int -> Datos -> [Etiqueta] -> Medida -> Instancia -> [(Instancia,Etiqueta)]
kMenores k datos etiquetas norma valor = take k (sortBy 
    (\a b -> (if ((norma (fst a) valor) < (norma (fst b) valor)) then LT else GT )) (zip datos etiquetas))

mejor :: [(Int,a)] -> a
mejor xs = snd (maximumBy (\a b -> if (fst a)<(fst b) then LT else GT) xs)

-- Para ejercicio 10

contarIguales ::Eq a =>  [(a,a)] -> Int
contarIguales = foldr(\x acum -> if(fst(x) == snd(x)) then acum+1 else acum) 0 

-- Para ejercicio 11

dameTrain:: [(Instancia,Etiqueta)] -> Int -> Int -> [(Instancia,Etiqueta)]
dameTrain xs p tamano = (take (tamano * (p-1)) xs) ++ (drop (p*tamano) xs )

dameVal:: [(Instancia,Etiqueta)] -> Int -> Int -> [(Instancia,Etiqueta)]
dameVal xs p tamano = take tamano (drop ((p-1)*tamano) xs)

calcularTamano:: [a] -> Int -> Int
calcularTamano xs n = (length xs) `quot` n

-- Para ejercicio 12


primero :: (a,b,c,d) -> a
primero (x,y,z,w) = x

segundo :: (a,b,c,d) -> b
segundo (x,y,z,w) = y

tercero :: (a,b,c,d) -> c
tercero (x,y,z,w) = z

cuarto :: (a,b,c,d) -> d
cuarto (x,y,z,w) = w