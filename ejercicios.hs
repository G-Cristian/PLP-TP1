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

--3)TODO elementosSinRepetir
--cuentas xs = cantidadDeApariciones (elementosSinRepetir xs) xs
--cantidadDeApariciones toma la lista de elementos sin repetir y la lista original. Luego cuenta las repeticiones 
--de los elementos sin repetir en la lista original.
--elementosSinRepetir toma la lista y devuelve sus elementos sin repetir

--cuentas xs = cantidadDeApariciones (elementosSinRepetir xs) xs
cantidadDeApariciones::Eq a => [a]->([a]->[(Int,a)])
cantidadDeApariciones = foldr (\x contar -> (\ys->((repeticiones x ys),x):contar ys)) (\ys->[])

repeticiones::Eq a =>a->[a]->Int
repeticiones x = foldr (\y contar -> if x==y then (1+) contar else contar) 0