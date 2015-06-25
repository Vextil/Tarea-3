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
FROM ListaString IMPORT ListaString, IrInicioLista, IrSiguienteLista, EsPosicionValida, CrearLista, ActualLista, InsertarEnLista, EsVaciaLista, RemoverDeLista;
FROM Binario     IMPORT Binario, BoolBinario, Balanceado, CantidadBinario, DestruirBinario, CopiaBinario, InsertarEnBinario, CrearHoja, Linealizacion, BuscarABB, RemoverDeBinario;

TYPE	
	Set = POINTER TO TipoSet;
	TipoSet = RECORD
		arbol : Binario;
		vacio : BOOLEAN;
	END;


(*********************)
(*** CONSTRUCTORES ***)
(*********************)

PROCEDURE CrearSet (): Set;
(* Devuelve un conjunto vacio. *)
VAR set : Set;
BEGIN

	NEW(set);
	set^.vacio := TRUE;
	RETURN set;
   
END CrearSet;

PROCEDURE ConstruirSet (l: ListaString): Set;
(* Devuelve un conjunto con los mismos elementos que estan en 'l'. *)
VAR	set : Set;
BEGIN
   
	set := CrearSet();
	IrInicioLista(l);
	WHILE EsPosicionValida(l) DO
		IF set^.vacio THEN
			set^.arbol := CrearHoja(CrearInfo(0, ActualLista(l)));
			set^.vacio := FALSE;
		ELSE
			InsertarEnBinario(CrearInfo(0, ActualLista(l)), set^.arbol);
		END;
		IrSiguienteLista(l);
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
	nuevo^.vacio := FALSE;
   	RETURN nuevo;

END CopiaSet;

PROCEDURE InsertarEnSet (str: TString; VAR set: Set);
(* Inserta 'str' en 'set'. Si ya estaba, no hace nada. *)
BEGIN

	IF set^.vacio THEN
		set^.arbol := CrearHoja(CrearInfo(0, str));
		set^.vacio := FALSE;
	ELSE
		InsertarEnBinario(CrearInfo(0, str), set^.arbol);
	END;

END InsertarEnSet;

PROCEDURE Union (A, B: Set): Set;
(* Devuelve un conjunto con los elementos que estan en 'A' o en 'B'. 
   El conjunto devuelto no comparte memoria ni con A ni con B.
   El tiempo de ejecucion es O(nA + nB + n.log n) en peor caso, siendo nA y nB 
   la cantidad de elementos de A y B respectivamente y n la del conjunto 
   resultado.
   En el conjunto resultado se debe poder ejecutar Pertenece en tiempo O(log n),
   en peor caso. *)

	PROCEDURE Insertar(VAR a, b: ListaString);
	(* Inserta elemento actual de 'A' en 'B' y lo elimina de 'A' *)
	BEGIN

		InsertarEnLista(ActualLista(a), b);
		RemoverDeLista(a);

	END Insertar;

	PROCEDURE UnirListasSinRepeticiones(VAR a, b: ListaString): ListaString;
	(* Une dos listas dejando solo los elementos que estan en 'A' o en 'B' *)
	VAR 
		comparacion : CompareResults;
		c : ListaString;
	BEGIN

		c := CrearLista();
		WHILE EsPosicionValida(a) OR EsPosicionValida(b) DO
			IF EsPosicionValida(a) AND EsPosicionValida(b) THEN
	   			comparacion := Compare(ActualLista(a), ActualLista(b));
	   			IF comparacion = less THEN
	   				Insertar(a, c);
	   			ELSIF comparacion = greater THEN
	   				Insertar(b, c);
	   			ELSIF comparacion = equal THEN
	   				RemoverDeLista(a);
	   				RemoverDeLista(b);
	   			END;
	   		ELSIF EsPosicionValida(a) THEN
	   			Insertar(a, c);
	   		ELSIF EsPosicionValida(b) THEN
	   			Insertar(b, c);
	   		END;
		END;
		RETURN c;

	END UnirListasSinRepeticiones;

VAR	
	nuevo : Set;
	nuevaLista, listaA, listaB : ListaString;
BEGIN

	listaA := Linealizacion(A^.arbol);
	listaB := Linealizacion(B^.arbol);
	nuevaLista := UnirListasSinRepeticiones(listaA, listaB);
	nuevo := CrearSet();
	IF NOT EsVaciaLista(nuevaLista) THEN
		nuevo^.arbol := Balanceado(nuevaLista);
		nuevo^.vacio := FALSE;
	END;
   	RETURN nuevo;

END Union;

PROCEDURE Interseccion (A, B: Set): Set;
(* Devuelve un conjunto con los elementos que estan en 'A' y en 'B'.
   El conjunto devuelto no comparte memoria ni con A ni con B.
   El tiempo de ejecucion es O(nA + nB + n.log n) en peor caso, siendo nA y nB 
   la cantidad de elementos de A y B respectivamente y n la del conjunto 
   resultado.
   En el conjunto resultado se debe poder ejecutar Pertenece en tiempo O(log n),
   en peor caso. *)

	PROCEDURE Insertar(VAR a, b: ListaString);
	(* Inserta elemento actual de 'A' en 'B' y lo elimina de 'A' *)
	BEGIN

		InsertarEnLista(ActualLista(a), b);
		RemoverDeLista(a);

	END Insertar;

	PROCEDURE UnirListasSinRepeticiones(VAR a, b: ListaString): ListaString;
	(* Une dos listas dejando solo los elementos que estan en 'A' y en 'B' *)
	VAR 
		comparacion : CompareResults;
		c : ListaString;
	BEGIN

		c := CrearLista();
		WHILE EsPosicionValida(a) AND EsPosicionValida(b) DO
   			comparacion := Compare(ActualLista(a), ActualLista(b));
   			IF comparacion = less THEN
   				RemoverDeLista(a);
   			ELSIF comparacion = greater THEN
   				RemoverDeLista(b);
   			ELSIF comparacion = equal THEN
   				Insertar(a, c);
   				RemoverDeLista(b);
   			END;
		END;
		RETURN c;

	END UnirListasSinRepeticiones;

VAR	
	nuevo : Set;
	nuevaLista, listaA, listaB : ListaString;
BEGIN

	listaA := Linealizacion(A^.arbol);
	listaB := Linealizacion(B^.arbol);
	nuevaLista := UnirListasSinRepeticiones(listaA, listaB);
	nuevo := CrearSet();
	IF NOT EsVaciaLista(nuevaLista) THEN
		nuevo^.arbol := Balanceado(nuevaLista);
		nuevo^.vacio := FALSE;
	END;
   	RETURN nuevo;

END Interseccion;

PROCEDURE Diferencia (A, B: Set): Set;
(* Devuelve un conjunto con los elementos que estan en 'A' y no en 'B'.
   El conjunto devuelto no comparte memoria ni con A ni con B.
   El tiempo de ejecucion es O(nA + nB + n.log n) en peor caso, siendo nA y nB 
   la cantidad de elementos de A y B respectivamente y n la del conjunto 
   resultado.
   En el conjunto resultado se debe poder ejecutar Pertenece en tiempo O(log n),
   en peor caso. *)

	PROCEDURE Insertar(VAR a, b: ListaString);
	(* Inserta elemento actual de 'A' en 'B' y lo elimina de 'A' *)
	BEGIN

		InsertarEnLista(ActualLista(a), b);
		RemoverDeLista(a);

	END Insertar;

	PROCEDURE UnirListasSinRepeticiones(VAR a, b: ListaString): ListaString;
	(* Une dos listas dejando solo los elementos que estan en 'A' y no en 'B' *)
	VAR 
		comparacion : CompareResults;
		c : ListaString;
	BEGIN

		c := CrearLista();
		WHILE EsPosicionValida(a) DO
			IF EsPosicionValida(b) THEN
	   			comparacion := Compare(ActualLista(a), ActualLista(b));
	   			IF comparacion = less THEN
	   				Insertar(a, c);
	   			ELSIF comparacion = greater THEN
	   				RemoverDeLista(b);
	   			ELSIF comparacion = equal THEN
	   				RemoverDeLista(a);
	   				RemoverDeLista(b);
	   			END;
	   		ELSE
	   			Insertar(a, c);
	   		END;
		END;
		RETURN c;

	END UnirListasSinRepeticiones;

VAR	
	nuevo : Set;
	nuevaLista, listaA, listaB : ListaString;
BEGIN

	listaA := Linealizacion(A^.arbol);
	listaB := Linealizacion(B^.arbol);
	nuevaLista := UnirListasSinRepeticiones(listaA, listaB);
	nuevo := CrearSet();
	IF NOT EsVaciaLista(nuevaLista) THEN
		nuevo^.arbol := Balanceado(nuevaLista);
		nuevo^.vacio := FALSE;
	END;
   	RETURN nuevo;

END Diferencia;

(********************)
(*** DESTRUCTORES ***)
(********************)

PROCEDURE RemoverDeSet (str: TString; VAR set: Set);
(* Remueve 'str' de 'set'. Si no estaba, no hace nada. *)
VAR 
	buscar : BoolBinario;
	cantidad : CARDINAL;
BEGIN

	IF set^.vacio = FALSE THEN
		buscar := BuscarABB(str, set^.arbol);
		IF buscar.hayBinario THEN
			cantidad := CantidadBinario(set^.arbol);
	   		IF cantidad = 1 THEN
	   			DestruirBinario(set^.arbol);
	   			set^.vacio := TRUE;
	   		ELSE
	   			RemoverDeBinario(str, set^.arbol);
	   		END;
	   	END;
   	END;

END RemoverDeSet;

PROCEDURE DestruirSet (VAR set: Set);
(* Libera la memoria reservada por 'set'. *)
BEGIN

	IF NOT set^.vacio THEN
		DestruirBinario(set^.arbol);
	END;
	DISPOSE(set);
   
END DestruirSet;
   
(******************)
(*** PREDICADOS ***)
(******************)

PROCEDURE PerteneceASet (str: TString; set: Set): BOOLEAN;
(* Devuelve TRUE si 'str' es un elemento de 'set', FALSE en otro caso. *)
VAR buscar : BoolBinario;
BEGIN
   
	IF set^.vacio THEN
		RETURN FALSE;
	ELSE
		buscar := BuscarABB(str, set^.arbol);
		RETURN buscar.hayBinario;
	END;

END PerteneceASet;

PROCEDURE EsVacioSet (set: Set): BOOLEAN;
(* Devuelve TRUE si en 'set' no hay elementos, FALSE en otro caso. *)
BEGIN

	RETURN set^.vacio;
   
END EsVacioSet;


END Set.
