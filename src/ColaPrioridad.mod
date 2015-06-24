(* 4623178 *)
IMPLEMENTATION MODULE ColaPrioridad;
(*******************************************************************************
Modulo de implementacion de ColaPrioridad.

Es una cola de prioridad, no acotada, de elementos de tipo TString.
Las prioridades son numeros enteros en el rango [1 .. K].

Laboratorio de Programacion 2.
InCo-FI-UDELAR
*******************************************************************************)

FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM Utils IMPORT TString;
FROM ListaString IMPORT ListaString, CrearLista, InsertarEnLista, CantidadLista, RemoverDeLista, DestruirLista, ActualLista, EsVaciaLista, IrInicioLista;

TYPE
	ColaPrioridad = POINTER TO TipoColaPrioridad;
	TipoColaPrioridad = RECORD
		listas : ARRAY [1..K] OF ListaString;
		prioridades : ARRAY [1..K] OF RangoPrioridad;
		cantidad : CARDINAL;
	END;

(*********************)
(*** CONSTRUCTORES ***)
(*********************)

PROCEDURE CrearColaPrioridad (): ColaPrioridad;
(* Devuelve una cola de prioridad vacia (sin elementos). *)
VAR c : ColaPrioridad;
BEGIN

	NEW(c);
	c^.cantidad := 0;
	RETURN c;
   
END CrearColaPrioridad;

PROCEDURE InsertarEnColaPrioridad (txt: TString; prio: RangoPrioridad; VAR c: ColaPrioridad);
(* Inserta el texto 'txt' en la cola 'c' con prioridad 'prio'.
   Si NOT PerteneceAColaPrioridad (prio, c), el tiempo de ejecucion es O(log K).
   En otro caso el tiempo de ejecucion es O(K). *)
VAR 
	i : CARDINAL;
	aux : RangoPrioridad;
BEGIN

	IF NOT PerteneceAColaPrioridad(prio, c) THEN
		c^.listas[prio] := CrearLista();
		i := c^.cantidad;
		INC(c^.cantidad);
		WHILE (i > 1) AND (c^.prioridades[i] < c^.prioridades[i/2]) DO
			aux := c^.prioridades[i];
			c^.prioridades[i] := c^.prioridades[i/2];
			c^.prioridades[i/2] := aux;
			i := i / 2;
		END;
	END;
	InsertarEnLista(txt, c^.listas[prio]);
   
END InsertarEnColaPrioridad;

(********************)
(*** DESTRUCTORES ***)
(********************)

PROCEDURE ExtraerDeMinimoColaPrioridad (VAR c: ColaPrioridad);
(* Si EsVaciaColaPrioridad (c), no hace nada.
   En otro caso, remueve de 'c' el elemento con menor prioridad. 
   Si hay mas de un elemento con minima prioridad, remueve el que se inserto 
   primero y el tiempo de ejecucion es O(1).
   En otro caso (si hay solo uno con prioridad minima) el tiempo de ejecucion es 
   O(log K).   
*)
VAR	
	i, j : CARDINAL;
	min, aux : RangoPrioridad;
	continue : BOOLEAN;
BEGIN

	IF NOT EsVaciaColaPrioridad(c) THEN
		min := c^.prioridades[1];
		IF CantidadLista(c^.listas[min]) = 1 THEN
			DestruirLista(c^.listas[min]);
			c^.listas[min] := NIL;
			c^.prioridades[1] := c^.prioridades[c^.cantidad];
			DEC(c^.cantidad);
			continue := TRUE;
			i := 1;
			WHILE ((i * 2) <= c^.cantidad) AND continue DO
				j := i * 2;
				IF (j + 1 <= c^.cantidad) AND (c^.prioridades[j + 1] < c^.prioridades[j]) THEN
					INC(j);
				END;
				IF (c^.prioridades[i] < c^.prioridades[j]) THEN
					continue := FALSE;
				ELSE
					aux := c^.prioridades[i];
					c^.prioridades[i] := c^.prioridades[j];
					c^.prioridades[j] := aux;
					i := j;
				END;
			END;
		ELSE
			IrInicioLista(c^.listas[min]);
			RemoverDeLista(c^.listas[min]);
		END;
	END;
   
END ExtraerDeMinimoColaPrioridad;

PROCEDURE DestruirColaPrioridad (VAR c: ColaPrioridad);
(* Libera la memoria asignada a 'c' y a todos sus elementos. *)
VAR i : CARDINAL;
BEGIN

	FOR i := 1 TO c^.cantidad DO
		IF c^.prioridades[i] > 0 THEN
			DestruirLista(c^.listas[c^.prioridades[i]]);
		END;
	END;
	DISPOSE(c);
   
END DestruirColaPrioridad;

(******************)
(*** PREDICADOS ***)
(******************)

PROCEDURE EsVaciaColaPrioridad (c: ColaPrioridad): BOOLEAN;
(* Devuelve TRUE si la cola de prioridad 'c' es vacia o FALSE en caso contrario.
   El tiempo de ejecucion es O(1). *)
BEGIN

		RETURN (c = NIL) OR (c^.cantidad = 0);

END EsVaciaColaPrioridad;

PROCEDURE PerteneceAColaPrioridad (prio: RangoPrioridad; c: ColaPrioridad): BOOLEAN;
(* Devuelve TRUE si en 'c' hay algun elemento con prioridad 'prio' o FALSE en
   caso contrario.
   El tiempo de ejecucion de la busqueda es O(1). *)
BEGIN

	RETURN NOT EsVaciaLista(c^.listas[prio]);
   
END PerteneceAColaPrioridad;

(******************)
(*** SELECTORES ***)
(******************)

PROCEDURE MinimoColaPrioridad (c: ColaPrioridad): TString;
(* Precondicion: NOT EsVaciaColaPrioridad (c).
   Devuelve el elemento de la cola de prioridad 'c' que tiene menor prioridad.
   Si hay mas de un elemento con minima prioridad, devuelve el que se inserto 
   primero.
   El tiempo de ejecucion es O(1). *)
VAR 
	p : RangoPrioridad;
BEGIN

	p := c^.prioridades[1];
	IrInicioLista(c^.listas[p]);
	RETURN ActualLista(c^.listas[p]);
   
END MinimoColaPrioridad;

PROCEDURE PrioridadMinimoColaPrioridad (c: ColaPrioridad): RangoPrioridad;
(* Pre: NOT EsVaciaColaPrioridad (c).
   Devuelve la prioridad del elemento con menor prioridad. 
   El tiempo de ejecucion es O(1). *)
BEGIN

	RETURN c^.prioridades[1];
   
END PrioridadMinimoColaPrioridad;

PROCEDURE ElementosColaPrioridad (prio: RangoPrioridad; c: ColaPrioridad): ListaString;
(* Pre: PerteneceAColaPrioridad (prio, c).
   Devuelve una lista, ordenada segun el momento de insercion, de los elementos
   de 'c' con prioridad 'prio'.
   La lista devuelta puede compartir memoria con 'c'.
   El tiempo de ejecucion es O(K). *)
BEGIN
   
	RETURN c^.listas[prio];

END ElementosColaPrioridad;

END ColaPrioridad.
