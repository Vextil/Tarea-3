(* 4623178 *)
IMPLEMENTATION MODULE Set;
(*******************************************************************************
Modulo de definicion de Set.

Conjunto de strings.

Laboratorio de Programacion 2.
InCo-FIng-UDELAR
*******************************************************************************)

FROM Utils IMPORT TString, CrearInfo;
FROM ListaString IMPORT ListaString, IrInicioLista, IrSiguienteLista, ActualLista, CantidadLista, PartirLista;

TYPE	
	Set = POINTER TO TipoSet;
	TipoSet = RECORD
		cantidad : CARDINAL;
		arbol : Arbol;
	END;
	Arbol = POINTER TO Nodo;
	Nodo = RECORD
		elemento : TString;
		izquierdo, derecho : Arbol;
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
	NEW(set^.arbol);
	RETURN set;
   
END CrearSet;

PROCEDURE ListaArbol (VAR l: ListaString): Arbol;
VAR 
	a: Arbol;
	l2: ListaString;
BEGIN

	NEW(a);
	IF CantidadLista(l) > 1 THEN
		l2 := PartirLista(l);
	ELSE
		l2 := l;
	END;
	IrInicioLista(l2);
	a^.elemento := ActualLista(l2);
	RemoverDeLista(l2);
	IF NOT EsVaciaLista(l) THEN
		a^.izquierdo := ListaArbol(l);
		DestruirLista(l);
	END;
	IF NOT EsVaciaLista(l2) THEN
		a^.derecho := ListaArbol(l2);
		DestruirLista(l2);
	END;
	RETURN a;

END Balanceado;

PROCEDURE ConstruirSet (l: ListaString): Set;
(* Devuelve un conjunto con los mismos elementos que estan en 'l'. *)
VAR	set : Set;
BEGIN
   
	set := CrearSet();
	set^.cantidad := CantidadLista(l);
	set^.arbol := Balanceado(l);
	RETURN set;

END ConstruirSet;

PROCEDURE CopiaArbol (a: Arbol): Arbol;
(* Devuelve una copia de 'a'. *)
VAR nuevo : Arbol;
BEGIN

	NEW(nuevo);
	nuevo^.elemento := a^.elemento;
	IF a^.izquierdo # NIL THEN
		nuevo^.izquierdo := CopiaArbol(a^.izquierdo);
	END;
	IF a^.derecho # NIL THEN
		nuevo^.derecho := CopiaArbol(a^.derecho);
	END;
	RETURN nuevo;
   
END CopiaArbol;

PROCEDURE CopiaSet (S: Set): Set;
(* Devuelve un conjunto con loe mismos elementos que 'S. 
   El conjunto resultado no comparte memoria con 'S'. *)
VAR nuevo : Set;
BEGIN

	NEW(nuevo);
	nuevo^.arbol := CopiaArbol(S^.arbol);
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

	PROCEDURE UnirListasSinRepeticiones(a, b: ListaString; l1, l2: CARDINAL): ListaString;
	BEGIN

		FOR i := 1 TO n DO

			FOR

		END;
		
	END;

VAR	
	nuevo : Set;
	nuevaLista : ListaString;
	largo : CARDINAL;
BEGIN

	listaA := Linealizacion(A^.arbol);
	listaB := Linealizacion(B^.arbol);
	IrInicioLista(listaA);
	IrInicioLista(listaB);
	UnirListasSinRepeticiones(listaA, listaB, A^.cantidad, B^.cantidad);
	nuevo := CrearSet();
	nuevo^.arbol := Balanceado(nuevaLista);
	nuevo^.cantidad := largo;
   
END Union;

PROCEDURE Interseccion (A, B: Set): Set;
(* Devuelve un conjunto con los elementos que estan en 'A' y en 'B'.
   El conjunto devuelto no comparte memoria ni con A ni con B.
   El tiempo de ejecucion es O(nA + nB + n.log n) en peor caso, siendo nA y nB 
   la cantidad de elementos de A y B respectivamente y n la del conjunto 
   resultado.
   En el conjunto resultado se debe poder ejecutar Pertenece en tiempo O(log n),
   en peor caso. *)
BEGIN
   
END Interseccion;

PROCEDURE Diferencia (A, B: Set): Set;
(* Devuelve un conjunto con los elementos que estan en 'A' y no en 'B'.
   El conjunto devuelto no comparte memoria ni con A ni con B.
   El tiempo de ejecucion es O(nA + nB + n.log n) en peor caso, siendo nA y nB 
   la cantidad de elementos de A y B respectivamente y n la del conjunto 
   resultado.
   En el conjunto resultado se debe poder ejecutar Pertenece en tiempo O(log n),
   en peor caso. *)
BEGIN
   
END Diferencia;

(********************)
(*** DESTRUCTORES ***)
(********************)

PROCEDURE RemoverDeSet (str: TString; VAR set: Set);
(* Remueve 'str' de 'set'. Si no estaba, no hace nada. *)
BEGIN
   
END RemoverDeSet;

PROCEDURE DestruirSet (VAR set: Set);
(* Libera la memoria reservada por 'set'. *)
BEGIN
   
END DestruirSet;
   
(******************)
(*** PREDICADOS ***)
(******************)

PROCEDURE PerteneceASet (str: TString; set: Set): BOOLEAN;
(* Devuelve TRUE si 'str' es un elemento de 'set', FALSE en otro caso. *)
BEGIN
   
END PerteneceASet;

PROCEDURE EsVacioSet (set: Set): BOOLEAN;
(* Devuelve TRUE si en 'set' no hay elementos, FALSE en otro caso. *)
BEGIN
   
END EsVacioSet;


END Set.
