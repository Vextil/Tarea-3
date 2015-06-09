(* 4623178 *)
IMPLEMENTATION MODULE Pila;
(*******************************************************************************
Modulo de implementacion de Pila.

Es una pila no acotada elementos de tipo TInfo.

Laboratorio de Programacion 2.
InCo-FI-UDELAR
*******************************************************************************)
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM Utils IMPORT TInfo, DestruirInfo;

TYPE 
	Pila = POINTER TO NodoPila;
	NodoPila = RECORD
		elemento : TInfo;
		anterior : Pila;
	END;

(*********************)
(*** CONSTRUCTORES ***)
(*********************)

PROCEDURE CrearPila (): Pila;
(* Devuelve la pila vacia (sin elementos). *)
VAR pila : Pila;
BEGIN

	NEW(pila);
	pila^.elemento := NIL;
	pila^.anterior := NIL;
   
END CrearPila;

PROCEDURE Apilar (info: TInfo; VAR p: Pila);
(* Apila el elemento 'info' en la pila 'p', quedando en el tope de la misma. *)
VAR pNuevo: Pila;
BEGIN
   
	IF EsVaciaPila(p) THEN
		p^.elemento := info;
	ELSE
		NEW(pNuevo);
		pNuevo^.elemento := info;
		pNuevo^.anterior := p;
		p := pNuevo;
	END;

END Apilar;

(********************)
(*** DESTRUCTORES ***)
(********************)

PROCEDURE Desapilar (VAR p: Pila);
(* Si EsVaciaPila (p), no hace nada.
   En otro caso, desapila el elemento ubicando en el tope de 'p'.
   Se debe liberar la memoria del nodo sin destruir el elemento removido. *)
VAR pAux : Pila;
BEGIN

	IF NOT EsVaciaPila(p) THEN
		IF p^.anterior = NIL THEN
			p^.elemento := NIL;
		ELSE
			pAux := p;
			p := p^.anterior;
			DISPOSE(pAux);
		END;
	END;
   
END Desapilar;

PROCEDURE DestruirPila (VAR p: Pila);
(* Libera la memoria asignada a 'p' a todos sus elementos. *)
BEGIN

	IF NOT EsVaciaPila(p) THEN
		IF p^.anterior # NIL THEN
			DestruirPila(p^.anterior);
		END;
		DestruirInfo(p^.elemento)
		DISPOSE(p);
	END;
   
END DestruirPila;

PROCEDURE EsVaciaPila (p: Pila): BOOLEAN;
(* Devuelve TRUE si la pila 'p' es vacia o FALSE en caso contrario. *)
BEGIN

	RETURN ((p^.elemento = NIL) AND (p^.anterior = NIL));
   
END EsVaciaPila;

PROCEDURE Tope (p: Pila): TInfo;
(* Precondicion: NOT EsVaciaPila (p).
   Devuelve el elemento ubicado en el tope de la pila 'p'. *)
BEGIN

	RETURN p^.elemento;
   
END Tope;

END Pila.
