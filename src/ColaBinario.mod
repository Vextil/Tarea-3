(* 4623178 *)
IMPLEMENTATION MODULE ColaBinario;
(*******************************************************************************
Modulo de implementacion de ColaBinario.

Es una cola no acotada de elementos de tipo Binario.

Laboratorio de Programacion 2.
InCo-FI-UDELAR
*******************************************************************************)
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM Binario IMPORT Binario, DestruirBinario;

TYPE
	ColaBinario = POINTER TO TipoColaBinario;
	TipoColaBinario = RECORD
		inicio, final : NodoColaBinario;
	END;
	NodoColaBinario = RECORD
		elemento : Binario;
		siguiente : NodoColaBinario;
	END;

(*********************)
(*** CONSTRUCTORES ***)
(*********************)

PROCEDURE CrearColaBinario (): ColaBinario;
(* Devuelve una cola vacia (sin elementos). *)
VAR cola : ColaBinario;
BEGIN

	NEW(cola);
	cola^.inicio := NIL;
	cola^.final := NIL;
	RETURN cola;
	
END CrearColaBinario;

PROCEDURE EncolarBinario (arbol: Binario; VAR c: ColaBinario);
(* Encola el arbol binario de busqueda 'arbol' en la cola 'c'. *)
VAR cNuevo : NodoColaBinario;
BEGIN

	NEW(cNuevo);
	cNuevo^.elemento := arbol;
	cNuevo^.siguiente := NIL;
	IF EsVaciaColaBinario(c) THEN
		c^.inicio := cNuevo;
		c^.final := cNuevo;
	ELSE
		c^.final^.siguiente := cNuevo;
		c^.final := cNuevo;
	END;
	
END EncolarBinario;

(********************)
(*** DESTRUCTORES ***)
(********************)

PROCEDURE DesencolarBinario (VAR c: ColaBinario);
(* Si EsVaciaColaBinario (c) no hace nada.
   En otro caso, desencola el primer arbol de la cola 'c'.
   Se debe liberar la memoria del nodo sin destruir el arbol removido. *)
VAR cAux : NodoColaBinario;
BEGIN

	IF NOT EsVaciaColaBinario(c) THEN
		cAux := c^.inicio;
		c^.inicio := c^.inicio^.siguiente;
		DISPOSE(cAux);
	END;
	
END DesencolarBinario;

PROCEDURE DestruirColaBinario (VAR c: ColaBinario);
(* Libera la memoria asignada a 'c' y a todos sus elementos. *)

	PROCEDURE DestruirNodos(n: NodoColaBinario);
	BEGIN
		IF n # NIL THEN
			DestruirColaBinario(n^.siguiente);
			DestruirBinario(n^.elemento);
			DISPOSE(n);
		END;
	END;

BEGIN

	IF NOT EsVaciaColaBinario(c) THEN
		DestruirNodos(c^.inicio);
		DISPOSE(c);
	END;
	
END DestruirColaBinario;

(******************)
(*** PREDICADOS ***)
(******************)
   
PROCEDURE EsVaciaColaBinario (c: ColaBinario): BOOLEAN;
(* Devuelve TRUE si la cola 'c' es vacia o FALSE en caso contrario. *)
BEGIN

	RETURN c^.inicio = NIL;
	
END EsVaciaColaBinario;
   
(******************)
(*** SELECTORES ***)
(******************)

PROCEDURE PrimeroColaBinario (c: ColaBinario): Binario;
(* Precondicion: NOT EsVaciaColaBinario(c)
   Devuelve el arbol ubicado en el primer lugar de la cola.
   No remueve de la cola el arbol devuelto. *) 
BEGIN

	RETURN c^.inicio^.elemento;
	
END PrimeroColaBinario;

END ColaBinario.
