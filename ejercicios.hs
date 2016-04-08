import Data.List
import Data.List.Split
--1)TODO
--split::Eq a => a -> [a] -> [[a]]
splitAux c = splitOn [c]

--2)TODO cambiar splitAux por split; devuelve double en vez de float
--NOTA: No use la funcion sugerida 'mean'
--longitudPromedioPalabras xs = sumaLongitudes(split ' ' xs) / (length (split ' ' xs))
longitudPromedioPalabras xs = sumaLongitudes(splitAux ' ' xs) / (genericLength (splitAux ' ' xs))
sumaLongitudes = foldr (\x sumador -> genericLength x + sumador) 0

--3)
--cuentas xs = cantidadDeApariciones (elementosSinRepetir xs) xs
--cantidadDeApariciones toma la lista de elementos sin repetir y la lista original. Luego cuenta las repeticiones 
--de los elementos sin repetir en la lista original.
--elementosSinRepetir toma la lista y devuelve sus elementos sin repetir

cuentas xs = cantidadDeApariciones (elementosSinRepetir xs) xs

cantidadDeApariciones::Eq a => [a]->([a]->[(Int,a)])
cantidadDeApariciones = foldr (\x contar -> (\ys->((repeticiones x ys),x):contar ys)) (\ys->[])

repeticiones::Eq a =>a->[a]->Int
repeticiones x = foldr (\y contar -> if x==y then (1+) contar else contar) 0

elementosSinRepetir xs = (foldr(\x recu->(\ys -> if repeticiones x (tail ys) == 0 then x:recu (tail ys) else recu (tail ys))) (\ys->[]) xs) xs

--4)TODO remplazar splitAux por split del ejercicio 1
--1° separar las palabras (ej1)
--2° ver cuantas palabras hay (length 1°)
--3° dividir por la cantidad de palabras sin repetir (length de elementosSinRepetir de 1° o length de cuentas de 1°)
repeticionesPromedio xs = (fromIntegral $ length $ splitAux ' ' xs) / (fromIntegral $ length $ elementosSinRepetir $ splitAux ' ' xs)

--5)¿? TODO

--6)
--1° Correr el extractor para todos los textos
--2°Encontrar al valor cuyo valor absoluto sea maximo
--3°Devolver un extractor que al final divida por el valor encontrado en 2°
--(/maximo del punto 2).(extractor)
normalizarExtractor textos extractor= (/maximoValor textos extractor).extractor

maximoValor textos extractor = abs $ maximoAbsoluto $ ejecutarExtractor textos extractor

maximoAbsoluto = foldr (\x buscarMax -> if abs x >= abs buscarMax then abs x else abs buscarMax) 0

ejecutarExtractor textos extractor = map extractor textos