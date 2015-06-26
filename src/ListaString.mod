(* 4623178 *)
IMPLEMENTATION MODULE ListaString;
(*******************************************************************************
Modulo de definicion de ListaString.

Es una lista no acotada de elementos de tipo TString.
Mantiene de manera implicita la posicion actual.

Laboratorio de Programacion 2.
InCo-FI-UDELAR
*******************************************************************************)
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM STextIO IMPORT WriteString;
FROM Strings IMPORT CompareResults, Compare;
FROM Utils IMPORT TString;

TYPE
	ListaString = POINTER TO TipoLista;
	Posicion = POINTER TO Nodo; 
	TipoLista = RECORD
	              inicio, final, actual : Posicion;
	              cantidad: CARDINAL;
	           END;
	Nodo = RECORD
	           elemento : TString;
	           anterior, siguiente: Posicion;
	       END;
(*********************)
(*** CONSTRUCTORES ***)
(*********************)

PROCEDURE CrearLista (): ListaString;
(* Devuelve la lista vacia (sin elementos) 
   La posicion actual no es valida.
   El tiempo de ejecucion es O(1). *)
BEGIN
   
	RETURN NIL;

END CrearLista;
   
PROCEDURE CopiaLista (l: ListaString): ListaString;
(* Devuelve una copia de 'l'. La lista resultado no comparte memoria con 'l'.
   No se modifica la posicion actual.
   En la lista resultado la posicion actual no es valida.   
   El tiempo de ejecucion es O(n), siendo n = CantidadLista (l). *)
VAR 
	nueva : ListaString;
	actualAux : Posicion;
BEGIN
   
   	IF EsVaciaLista(l) THEN
   		RETURN NIL;
   	ELSE
		NEW(nueva);
		actualAux := l^.actual;
		IrInicioLista(l);
		WHILE EsPosicionValida(l) DO
			InsertarEnLista(ActualLista(l), nueva);
			IrSiguienteLista(l);
		END;
		l^.actual := actualAux;
		RETURN nueva;
	END;

END CopiaLista;

PROCEDURE InsertarEnLista (txt: TString; VAR l: ListaString);
(* Inserta el texto 'txt' al final de la lista 'l'.
   No se modifica la posicion actual.
   El tiempo de ejecucion es O(1). *)
VAR nodo : Posicion;
BEGIN

	NEW(nodo);
	nodo^.elemento := txt;
	nodo^.siguiente := NIL;
	nodo^.anterior := NIL;
	IF EsVaciaLista(l) THEN
		NEW(l);
		l^.inicio := NIL;
		l^.final := NIL;
	END;
	IF (l^.inicio = NIL) THEN
		l^.cantidad := 1;
		l^.inicio := nodo;
		l^.final := nodo;
	ELSE
		INC(l^.cantidad);
		nodo^.anterior := l^.final;
		l^.final^.siguiente := nodo;
		l^.final := nodo;
	END;

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
BEGIN
   
	mitad := CantidadLista(l) DIV 2;
	NEW(nueva);
	IrInicioLista(l);
	FOR i := 1 TO mitad DO
		IrSiguienteLista(l);
	END;

	nueva^.cantidad := l^.cantidad - mitad;
	nueva^.inicio := l^.actual;
	nueva^.final := l^.final;

	l^.cantidad := mitad;
	l^.final := l^.actual^.anterior;
	l^.final^.siguiente := NIL;
	l^.actual := NIL;

	nueva^.inicio^.anterior := NIL;

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
			DISPOSE(l);
			l := NIL;
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
		DISPOSE(actual);
	END;
   
END RemoverDeLista;

PROCEDURE DestruirLista (VAR l: ListaString);
(* Libera la memoria asignada a 'l' y a todos sus elementos.
   El tiempo de ejecucion es O(n), siendo n = CantidadLista (l). *)
BEGIN

	IF NOT EsVaciaLista(l) THEN
		WHILE l^.inicio # NIL DO
			l^.actual := l^.inicio;
			l^.inicio := l^.actual^.siguiente;
			DISPOSE(l^.actual);
		END;
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

	IF l = NIL THEN
		RETURN TRUE;
	ELSE
		RETURN FALSE;
	END;

END EsVaciaLista;

PROCEDURE EsPosicionValida (l: ListaString): BOOLEAN;   
(* Precondicion: NOT EsVaciaLista (l).
   Devuelve TRUE si la posicion actual de 'l' es valida (o sea, si esta en 
   alguno de los elementos de 'l') o FALSE en caso contrario.
   No se modifica la posicion actual.
   El tiempo de ejecucion es O(1). *)
BEGIN

	IF EsVaciaLista(l) THEN
		RETURN FALSE;
	ELSIF l^.actual = NIL THEN
		RETURN FALSE;
	ELSE
		RETURN TRUE;
	END;
   
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

	IF EsVaciaLista(l) THEN
		RETURN TRUE;
	ELSE
		RETURN SiguienteEsMayor(l^.inicio);
	END;
   
END EstaOrdenadaLista;

(*******************)
(* POSICIONAMIENTO *)
(*******************)

PROCEDURE IrInicioLista (VAR l: ListaString);
(* Precondicion: NOT EsVaciaLista (l).
   Ubica la posicion actual en el primer elemento de 'l'.
   El tiempo de ejecucion es O(1). *)
BEGIN

	IF NOT EsVaciaLista(l) THEN
		l^.actual := l^.inicio;
	END;
   
END IrInicioLista;

PROCEDURE IrSiguienteLista (VAR l: ListaString);
(* Precondicion: EsPosicionValida (l).
   Ubica la posicion actual en el elemento siguiente del que estaba.
   Si la posicion actual estaba en el ultimo elemento de 'l', entonces 
   deja de ser valida.
   El tiempo de ejecucion es O(1). *)
BEGIN

	IF l^.actual^.siguiente # NIL THEN
		l^.actual := l^.actual^.siguiente;
	ELSE
		l^.actual := NIL;
	END;
   
END IrSiguienteLista;

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
   
   	IF EsVaciaLista(l) THEN
   		RETURN 0;
   	ELSE
		RETURN l^.cantidad;
	END;

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
		listaLoop := l^.inicio;
		WHILE listaLoop # NIL DO
			WriteString(listaLoop^.elemento);
			WriteString(" ");
			listaLoop := listaLoop^.siguiente;
		END;
	END;
   
END ImprimirLista;

END ListaString.
