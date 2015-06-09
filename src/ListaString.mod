(* 4623178 *)
IMPLEMENTATION MODULE ListaString;
(*******************************************************************************
Modulo de definicion de ListaString.

Es una lista no acotada de elementos de tipo TString.
Mantiene de manera implicita la posicion actual.

Laboratorio de Programacion 2.
InCo-FI-UDELAR
*******************************************************************************)

FROM Utils IMPORT TString;
FROM Strings IMPORT CompareResults, Compare;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

TYPE
	ListaString = POINTER TO TipoLista;
	TipoLista = RECORD
	              inicio, final, actual : Posicion;
	              cantidad: CARDINAL;
	           END;
	Nodo = RECORD
	           elemento : TString;
	           anterior, siguiente: Posicion;
	       END;
	Posicion = POINTER TO Nodo; 

(*********************)
(*** CONSTRUCTORES ***)
(*********************)

PROCEDURE CrearLista (): ListaString;
(* Devuelve la lista vacia (sin elementos) 
   La posicion actual no es valida.
   El tiempo de ejecucion es O(1). *)
VAR lista : ListaString;
BEGIN
   
	NEW(lista);
	lista^.cantidad := 0;
	lista^.inicio := NIL;
	lista^.final := NIL;
	lista^.actual := NIL;

END CrearLista;
   
PROCEDURE CopiaLista (l: ListaString): ListaString;
(* Devuelve una copia de 'l'. La lista resultado no comparte memoria con 'l'.
   No se modifica la posicion actual.
   En la lista resultado la posicion actual no es valida.   
   El tiempo de ejecucion es O(n), siendo n = CantidadLista (l). *)
VAR 
	i : CARDINAL;
	nueva : ListaString;
	nodo : Posicion;
BEGIN
   
	nueva := CrearLista();
	nueva^.cantidad := l^.cantidad;
	actual := l^.inicio;
	WHILE actual # NIL DO
		NEW(nodo);
		IF EsVaciaLista(nueva) THEN
			nueva^.inicio := nodo;
			nueva^.actual := nodo;
		ELSE
			nueva^.actual^.siguiente := nodo;
			nueva^.actual^.siguiente^.anterior := nueva^.actual;
		END;
		nodo^.elemento := actual^.elemento;
		actual := actual^.siguiente;
		nueva^.actual := nueva^.actual^.siguiente;
		IF actual = NIL THEN
			nueva^.final := nodo;
			nodo^.siguiente := NIL;
		END;
		nodo := NIL;
	END;
	nueva^.actual := NIL;
	RETURN nueva;

END CopiaLista;

PROCEDURE InsertarEnLista (txt: TString; VAR l: ListaString);
(* Inserta el texto 'txt' al final de la lista 'l'.
   No se modifica la posicion actual.
   El tiempo de ejecucion es O(1). *)
VAR nodo : Posicion;
BEGIN

	INC(l^.cantidad);
	NEW(nodo);
	nodo^.elemento := txt;
	IF EsVaciaLista(l) THEN
		l^.inicio := nodo;
	ELSE
		l^.final^.siguiente := nodo;
	END;
	l^.final := nodo;

END InsertarEnLista;

PROCEDURE PartirLista (VAR l: ListaString): ListaString;
(* Precondicion: CantidadLista (l) > 1.
   Parte 'l' en dos mitades. Los primeros n DIV 2 elementos quedan en 'l'
   y el resto en la lista que se devuelve, siendo n = CantidadLista (l).
   Al terminar, en ambas listas las posiciones actuales son no validas.
   El tiempo de ejecucion es O(n). *) 
VAR 
	i, mitad : CARDINAL;
	nueva : ListaString;
	loop : Posicion;
BEGIN
   
	mitad := CantidadLista(l) DIV 2;
	nueva := CrearLista();
	loop := l^.inicio;
	FOR i := 1 TO mitad DO
		loop := loop^.siguiente;
		IF i = mitad THEN
			l^.final := loop^.anterior;
			l^.final^.siguiente := NIL;
			l^.actual := NIL;
			nueva^.inicio := loop;
			nueva^.inicio^.anterior := NIL;
			nueva^.final := l^.final;
			nueva^.actual := NIL;
		END;
	END;
	RETURN nueva;

END PartirLista;

(********************)
(*** DESTRUCTORES ***)
(********************)

PROCEDURE RemoverDeLista (VAR l: ListaString);
(* Si EsVaciaLista (l) o NOT EsPosicionValida (l) no hace nada. 
   En otro caso, remueve de 'l' el elemento que esta en la posicion actual. 
   La posicion actual se ubica en el siguiente elemento o, si estaba en
   el ultimo de 'l', deja de ser valida.
   El tiempo de ejecucion es O(1). *)
VAR actual : Posicion;
BEGIN

	IF EsPosicionValida(l) THEN
		DEC(l^.cantidad);
		actual := l^.actual;
		IF (actual = l^.inicio) AND (actual = l^.final) THEN
			l^.inicio := NIL;
			l^.final := NIL;
			l^.actual := NIL;
		ELSIF actual = l^.inicio THEN
			actual^.siguiente^.anterior := NIL;
			l^.inicio := actual^.siguiente;
			l^.actual := l^.inicio;
		ELSIF actual = l^.final THEN
			actual^.anterior^.siguiente := NIL;
			l^.final := actual^.anterior;
			l^.actual := NIL;
		ELSE
			actual^.siguiente^.anterior := actual^.anterior;
			actual^.anterior^.siguiente := actual^.siguiente;
			l^.actual := actual^.siguiente;
		END;
	END;
   
END RemoverDeLista;

PROCEDURE DestruirLista (VAR l: ListaString);
(* Libera la memoria asignada a 'l' y a todos sus elementos.
   El tiempo de ejecucion es O(n), siendo n = CantidadLista (l). *)

   PROCEDURE DestruirPosiciones (VAR p: Posicion);
   BEGIN
      IF p # NIL THEN
         DestruirPosiciones(p^.siguiente);
         DISPOSE(p);
      END;      
   END DestruirPosiciones;

BEGIN

	IF NOT EsVaciaLista(l) THEN
		DestruirPosiciones(l^.inicio);
		DISPOSE(l);
	END;

END DestruirLista;
   
(******************)
(*** PREDICADOS ***)
(******************)

PROCEDURE EsVaciaLista (l: ListaString): BOOLEAN;
(* Devuelve TRUE si 'l' es vacia o FALSE en caso contrario.
   No se modifica la posicion actual.
   El tiempo de ejecucion es O(1). *)
BEGIN

	RETURN l^.cantidad = 0;
   
END EsVaciaLista;

PROCEDURE EsPosicionValida (l: ListaString): BOOLEAN;   
(* Precondicion: NOT EsVaciaLista (l).
   Devuelve TRUE si la posicion actual de 'l' es valida (o sea, si esta en 
   alguno de los elementos de 'l') o FALSE en caso contrario.
   No se modifica la posicion actual.
   El tiempo de ejecucion es O(1). *)
BEGIN

	RETURN l^.actual # NIL;
   
END EsPosicionValida;
   
PROCEDURE EstaOrdenadaLista (l: ListaString): BOOLEAN;
(* Devuelve TRUE si 'l' esta en orden lexicografico creciente estricto (sin
   elementos repetidos) o FALSE en caso contrario.
   No se modifica la posicion actual.
   El tiempo de ejecucion es O(n), siendo n = CantidadLista (l). *)

	PROCEDURE SiguienteEsMayor(p: Posicion): BOOLEAN;
	BEGIN
		IF p^.siguiente = NIL THEN
			RETURN TRUE;
		ELSIF Compare(p^.elemento, p^.siguiente^.elemento) = less THEN
			RETURN SiguienteEsMayor(p^.siguiente);
		ELSE
			RETURN FALSE;
		END;
	END SiguienteEsMayor;

BEGIN

	RETURN SiguienteEsMayor(l^.inicio);
   
END EstaOrdenadaLista;

(*******************)
(* POSICIONAMIENTO *)
(*******************)

PROCEDURE IrInicioLista (VAR l: ListaString);
(* Precondicion: NOT EsVaciaLista (l).
   Ubica la posicion actual en el primer elemento de 'l'.
   El tiempo de ejecucion es O(1). *)
BEGIN

	l^.actual := l^.inicio;
   
END IrInicioLista;

PROCEDURE IrSiguienteLista (VAR l: ListaString);
(* Precondicion: EsPosicionValida (l).
   Ubica la posicion actual en el elemento siguiente del que estaba.
   Si la posicion actual estaba en el ultimo elemento de 'l', entonces 
   deja de ser valida.
   El tiempo de ejecucion es O(1). *)
BEGIN

	IF l^.actual = l^.final THEN
		l^.actual := NIL;
	ELSE
		l^.actual := l^.actual^.siguiente;
	END;
   
END IrInicioLista;

(******************)
(*** SELECTORES ***)
(******************)
  
PROCEDURE ActualLista (l: ListaString): TString;
(* Precondicion: EsPosicionValida (l).
   Devuelve el elemento de 'l' en el que esta la posicion actual.
   No se modifica la posicion actual.
   El tiempo de ejecucion es O(1). *)
BEGIN
   
	RETURN l^.actual^.elemento;

END ActualLista;
   
PROCEDURE CantidadLista (l: ListaString): CARDINAL;
(* Devuelve la cantidad de elementos en 'l'.
   No se modifica la posicion actual.
   El tiempo de ejecucion es O(1). *)
BEGIN
   
	RETURN l^.cantidad;

END CantidadLista;
      
(********************)
(****** SALIDA ******)
(********************)

PROCEDURE ImprimirLista (l: ListaString);
(* Imprime cada elemento de 'l' desde el primero hasta el ultimo con un espacio
   en blanco despues de cada uno. 
   No se modifica la posicion actual.
   El tiempo de ejecucion es O(n), siendo n = CantidadLista (l). *)
VAR listaLoop : Posicion;
BEGIN

	IF NOT EsVaciaLista(l) THEN
		WHILE listaLoop # NIL DO
			WriteString(listaLoop^.elemento);
			IF listaLoop^.siguiente # NIL THEN
				WriteString(" ");
			END;
			listaLoop := listaLoop^.siguiente;
		END;
	END;
   
END ImprimirLista;

END ListaString.
