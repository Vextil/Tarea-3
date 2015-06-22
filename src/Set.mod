(* 4623178 *)
IMPLEMENTATION MODULE Set;
(*******************************************************************************
Modulo de definicion de Set.

Conjunto de strings.

Laboratorio de Programacion 2.
InCo-FIng-UDELAR
*******************************************************************************)

FROM Storage     IMPORT ALLOCATE, DEALLOCATE;
FROM Utils       IMPORT TString, CrearInfo;
FROM Strings     IMPORT CompareResults, Compare;
FROM ListaString IMPORT ListaString, IrInicioLista, EsPosicionValida, IrSiguienteLista, ActualLista, InsertarEnLista, EsVaciaLista, RemoverDeLista;
FROM Binario     IMPORT Binario, Balanceado, CantidadBinario, DestruirBinario, CopiaBinario, InsertarBinario, CrearHoja, Linealizacion, BuscarABB, RemoverDeBinario;

TYPE	
	Set = POINTER TO TipoSet;
	TipoSet = RECORD
		arbol : Binario;
		cantidad : CARDINAL;
	END;


(*********************)
(*** CONSTRUCTORES ***)
(*********************)

PROCEDURE CrearSet (): Set;
(* Devuelve un conjunto vacio. *)
VAR set : Set;
BEGIN

	NEW(set);
	set^.cantidad := 0;
	RETURN set;
   
END CrearSet;

PROCEDURE ConstruirSet (l: ListaString): Set;
(* Devuelve un conjunto con los mismos elementos que estan en 'l'. *)
VAR	set : Set;
BEGIN
   
	set := CrearSet();
	IrInicioLista(l);
	set^.arbol := CrearHoja(CrearInfo(0, ActualLista(l)));
	RemoverDeLista(l);
	WHILE NOT EsVaciaLista(l) DO
		InsertarBinario(CrearInfo(0, ActualLista(l)), set^.arbol);
		RemoverDeLista(l);
		INC(set^.cantidad);
	END;
	RETURN set;

END ConstruirSet;

PROCEDURE CopiaSet (S: Set): Set;
(* Devuelve un conjunto con loe mismos elementos que 'S. 
   El conjunto resultado no comparte memoria con 'S'. *)
VAR nuevo : Set;
BEGIN

	NEW(nuevo);
	nuevo^.arbol := CopiaBinario(S^.arbol);
	nuevo^.cantidad := S^.cantidad;
   
END CopiaSet;

PROCEDURE InsertarEnSet (str: TString; VAR set: Set);
(* Inserta 'str' en 'set'. Si ya estaba, no hace nada. *)
BEGIN

	IF set^.cantidad = 0 THEN
		set^.arbol := CrearHoja(CrearInfo(0, str));
	ELSE
		InsertarBinario(CrearInfo(0, str), set^.arbol);
	END;
   	set^.cantidad := CantidadBinario(set^.arbol);

END InsertarEnSet;

PROCEDURE Union (A, B: Set): Set;
(* Devuelve un conjunto con los elementos que estan en 'A' o en 'B'. 
   El conjunto devuelto no comparte memoria ni con A ni con B.
   El tiempo de ejecucion es O(nA + nB + n.log n) en peor caso, siendo nA y nB 
   la cantidad de elementos de A y B respectivamente y n la del conjunto 
   resultado.
   En el conjunto resultado se debe poder ejecutar Pertenece en tiempo O(log n),
   en peor caso. *)

	PROCEDURE Insertar(a, b: ListaString; VAR cantidad: CARDINAL);
	(* Inserta elemento actual de 'A' en 'B' y lo elimina de 'A' *)
	BEGIN

		InsertarEnLista(ActualLista(a), b);
		RemoverDeLista(a);
		INC(cantidad);

	END;

	PROCEDURE UnirListasSinRepeticiones(a, b: ListaString; VAR cantidad: CARDINAL): ListaString;
	(* Une dos listas dejando solo los elementos que estan en 'A' o en 'B' *)
	VAR 
		comparacion : CompareResults;
		c : ListaString;
	BEGIN

		c := CrearLista();
		cantidad := 0;
		WHILE EsPosicionValida(a) OR EsPosicionValida(b) DO
			IF EsPosicionValida(a) AND EsPosicionValida(b) THEN
	   			comparacion := Compare(ActualLista(a), ActualLista(b));
	   			IF comparacion = less THEN
	   				Insertar(a, c, cantidad);
	   			ELSIF comparacion = greater THEN
	   				Insertar(b, c, cantidad);
	   			ELSIF comparacion = equal THEN
	   				RemoverDeLista(a);
	   				RemoverDeLista(b);
	   			END;
	   		ELSIF EsPosicionValida(a) THEN
	   			Insertar(a, c, cantidad);
	   		ELSIF EsPosicionValida(b) THEN
	   			Insertar(b, c, cantidad);
	   		END;
		END;
		RETURN c;
	END;

VAR	
	nuevo : Set;
	nuevaLista : ListaString;
	cantidad : CARDINAL;
BEGIN

	listaA := Linealizacion(A^.arbol);
	listaB := Linealizacion(B^.arbol);
	nuevaLista := UnirListasSinRepeticiones(listaA, listaB, cantidad);
	nuevo := CrearSet();
	nuevo^.arbol := Balanceado(nuevaLista);
	nuevo^.cantidad := cantidad;
   
END Union;

PROCEDURE Interseccion (A, B: Set): Set;
(* Devuelve un conjunto con los elementos que estan en 'A' y en 'B'.
   El conjunto devuelto no comparte memoria ni con A ni con B.
   El tiempo de ejecucion es O(nA + nB + n.log n) en peor caso, siendo nA y nB 
   la cantidad de elementos de A y B respectivamente y n la del conjunto 
   resultado.
   En el conjunto resultado se debe poder ejecutar Pertenece en tiempo O(log n),
   en peor caso. *)

	PROCEDURE Insertar(a, b: ListaString; VAR cantidad: CARDINAL);
	(* Inserta elemento actual de 'A' en 'B' y lo elimina de 'A' *)
	BEGIN

		InsertarEnLista(ActualLista(a), b);
		RemoverDeLista(a);
		INC(cantidad);

	END;

	PROCEDURE UnirListasSinRepeticiones(a, b: ListaString; VAR cantidad: CARDINAL): ListaString;
	(* Une dos listas dejando solo los elementos que estan en 'A' y en 'B' *)
	VAR 
		comparacion : CompareResults;
		c : ListaString;
	BEGIN

		c := CrearLista();
		cantidad := 0;
		WHILE EsPosicionValida(a) OR EsPosicionValida(b) DO
			IF EsPosicionValida(a) AND EsPosicionValida(b) THEN
	   			comparacion := Compare(ActualLista(a), ActualLista(b));
	   			IF comparacion = less THEN
	   				RemoverDeLista(a);
	   			ELSIF comparacion = greater THEN
	   				RemoverDeLista(b);
	   			ELSIF comparacion = equal THEN
	   				Insertar(a, c, cantidad);
	   			END;
	   		ELSIF EsPosicionValida(a) THEN
	   			Insertar(a, c, cantidad);
	   		ELSIF EsPosicionValida(b) THEN
	   			Insertar(b, c, cantidad);
	   		END;
		END;
		RETURN c;
	END;

VAR	
	nuevo : Set;
	nuevaLista : ListaString;
	cantidad : CARDINAL;
BEGIN

	listaA := Linealizacion(A^.arbol);
	listaB := Linealizacion(B^.arbol);
	nuevaLista := UnirListasSinRepeticiones(listaA, listaB, cantidad);
	nuevo := CrearSet();
	nuevo^.arbol := Balanceado(nuevaLista);
	nuevo^.cantidad := cantidad;

END Interseccion;

PROCEDURE Diferencia (A, B: Set): Set;
(* Devuelve un conjunto con los elementos que estan en 'A' y no en 'B'.
   El conjunto devuelto no comparte memoria ni con A ni con B.
   El tiempo de ejecucion es O(nA + nB + n.log n) en peor caso, siendo nA y nB 
   la cantidad de elementos de A y B respectivamente y n la del conjunto 
   resultado.
   En el conjunto resultado se debe poder ejecutar Pertenece en tiempo O(log n),
   en peor caso. *)

	PROCEDURE Insertar(a, b: ListaString; VAR cantidad: CARDINAL);
	(* Inserta elemento actual de 'A' en 'B' y lo elimina de 'A' *)
	BEGIN

		InsertarEnLista(ActualLista(a), b);
		RemoverDeLista(a);
		INC(cantidad);

	END;

	PROCEDURE UnirListasSinRepeticiones(a, b: ListaString; VAR cantidad: CARDINAL): ListaString;
	(* Une dos listas dejando solo los elementos que estan en 'A' y no en 'B' *)
	VAR 
		comparacion : CompareResults;
		c : ListaString;
	BEGIN

		c := CrearLista();
		cantidad := 0;
		WHILE EsPosicionValida(a) OR EsPosicionValida(b) DO
			IF EsPosicionValida(a) AND EsPosicionValida(b) THEN
	   			comparacion := Compare(ActualLista(a), ActualLista(b));
	   			IF comparacion = less THEN
	   				Insertar(a, c, cantidad);
	   			ELSIF comparacion = greater THEN
	   				RemoverDeLista(b);
	   			ELSIF comparacion = equal THEN
	   				RemoverDeLista(a);
	   				RemoverDeLista(b);
	   			END;
	   		ELSIF EsPosicionValida(a) THEN
	   			Insertar(a, c, cantidad);
	   		ELSIF EsPosicionValida(b) THEN
	   			Insertar(b, c, cantidad);
	   		END;
		END;
		RETURN c;
	END;

VAR	
	nuevo : Set;
	nuevaLista : ListaString;
	cantidad : CARDINAL;
BEGIN

	listaA := Linealizacion(A^.arbol);
	listaB := Linealizacion(B^.arbol);
	nuevaLista := UnirListasSinRepeticiones(listaA, listaB, cantidad);
	nuevo := CrearSet();
	nuevo^.arbol := Balanceado(nuevaLista);
	nuevo^.cantidad := cantidad;

END Diferencia;

(********************)
(*** DESTRUCTORES ***)
(********************)

PROCEDURE RemoverDeSet (str: TString; VAR set: Set);
(* Remueve 'str' de 'set'. Si no estaba, no hace nada. *)
BEGIN

	IF set^.cantidad # 0 AND BuscarABB(str, set^.arbol) THEN
   		IF set^.cantidad = 1 THEN
   			DestruirBinario(set^.arbol);
   		ELSE
   			RemoverDeBinario(str, set^.arbol);
   		END;
   		DEC(set^.cantidad);
   	END;

END RemoverDeSet;

PROCEDURE DestruirSet (VAR set: Set);
(* Libera la memoria reservada por 'set'. *)
BEGIN

	IF set^.cantidad # 0 THEN
		DestruirBinario(set^.arbol);
	END;
	DISPOSE(set);
   
END DestruirSet;
   
(******************)
(*** PREDICADOS ***)
(******************)

PROCEDURE PerteneceASet (str: TString; set: Set): BOOLEAN;
(* Devuelve TRUE si 'str' es un elemento de 'set', FALSE en otro caso. *)
BEGIN
   
	IF set^.cantidad = 0 THEN
		RETURN FALSE;
	ELSE
		RETURN BuscarABB(str, set^.arbol);
	END;

END PerteneceASet;

PROCEDURE EsVacioSet (set: Set): BOOLEAN;
(* Devuelve TRUE si en 'set' no hay elementos, FALSE en otro caso. *)
BEGIN

	RETURN set^.cantidad = 0;
   
END EsVacioSet;


END Set.
