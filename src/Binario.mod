(* 4623178 *)
IMPLEMENTATION MODULE Binario;
(*******************************************************************************
Modulo de implementacion de Binario.

Es un arbol binario de busqueda de elementos de tipo TInfo, ordenado segun el
dato de tipo texto.
No hay arbol vacio.

Laboratorio de Programacion 2.
InCo-FI-UDELAR
*******************************************************************************)
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM STextIO IMPORT WriteString, WriteLn;
FROM Strings IMPORT CompareResults, Compare, Append;

FROM Utils IMPORT TInfo, TString, CrearInfo, InfoAString, TCritFiltro;
FROM ListaString IMPORT ListaString, CrearLista, InsertarEnLista, PartirLista, IrInicioLista, DestruirLista;

TYPE Binario = POINTER TO Nodo;
     Nodo = RECORD
               elemento : TInfo;
               padre, izquierdo, derecho : Binario;
            END;

(*********************)
(*** CONSTRUCTORES ***)
(*********************)

PROCEDURE CrearHoja (i: TInfo): Binario;
(* Crea un arbol formado unicamente por una hoja, cuyo elemento es 'i'. *)
VAR hoja : Binario;
BEGIN

   NEW(hoja);
   hoja^.elemento := i;
   hoja^.padre := NIL;
   hoja^.izquierdo := NIL;
   hoja^.derecho := NIL;
   RETURN hoja;
   
END CrearHoja;

PROCEDURE CopiaBinario (a: Binario): Binario;
(* Devuelve una copia de 'a'.
   Esto significa que el arbol resultado debe tener los mismos elementos y
   relaciones padre-hijo que 'a'. 
   El arbol resultado no comparte memoria con 'a'.
   El tiempo de ejecucion es O(n), siendo n = CantidadBinario (a). *)

	PROCEDURE RecrearNodo(nodo: Binario): Binario;
	VAR nuevo : Binario;
	BEGIN
		nuevo := CrearHoja(CrearInfo(NumeroInfo(nodo^.elemento), TextoInfo(nodo^.elemento)));
		RETURN nuevo;
	END RecrearNodo;

VAR nuevo : Binario;
BEGIN

	nuevo := RecrearNodo(a);
	IF a^.izquierdo # NIL THEN
		nuevo^.izquierdo := CopiaBinario(a^.izquierdo);
		nuevo^.izquierdo^.padre := nuevo;
	END;
	IF a^.derecho # NIL THEN
		nuevo^.derecho := CopiaBinario(a^.derecho);
		nuevo^.derecho^.padre := nuevo;
	END;
	RETURN nuevo;
   
END CopiaBinario;

PROCEDURE Balanceado (VAR l: ListaString): Binario;
(* Precondicion: NOT EsVaciaLista (l) y EstaOrdenadaLista (l).
   Devuelve un arbol con un nodo por cada elemento de 'l'. El valor numerico del
   nodo es 0 y el valor de texto es igual al elemento de 'l'.
   El arbol resultado debe estar balanceado.   
   Un arbol esta balanceado si en cada nodo que no es hoja, la cantidad
   de elementos de su subarbol izquierdo es igual a, o 1 mas que, la cantidad 
   de elementos de su subarbol derecho.
   Al finalizar debe haberse liberado toda la memoria asignada a 'l'.    
   El tiempo de ejecucion es O(n . log n), siendo n = CantidadLista (l)
   (ver letra). *)
VAR 
	a: Binario;
	l2: ListaString;
BEGIN

	a := CrearHoja();
	IF CantidadLista(l) > 1 THEN
		l2 := PartirLista(l);
	ELSE
		l2 := l;
	END;
	IrInicioLista(l2);
	a^.elemento := CrearInfo(0, ActualLista(l2));
	RemoverDeLista(l2);
	IF NOT EsVaciaLista(l) THEN
		a^.izquierdo := Balanceado(l);
		a^.izquierdo^.padre := a;
		DestruirLista(l);
	END;
	IF NOT EsVaciaLista(l2) THEN
		a^.derecho := Balanceado(l2);
		a^.derecho^.padre := a;
		DestruirLista(l2);
	END;
	RETURN a;
   
END Balanceado;

PROCEDURE InsertarBinario (i: TInfo; VAR a: Binario);
(* Inserta 'i' en 'a' respetando el orden del arbol.
   Si en 'a' ya hay un nodo cuyo dato de texto es igual a TextoInfo (i), no se
   hace nada. *)
VAR 
   binarioLoop : Binario;
   continue : BOOLEAN;
   comparacion : CompareResults;
BEGIN

   binarioLoop := a;
   continue := TRUE;
   WHILE continue DO 
   		comparacion := Compare(TextoInfo(i), TextoInfo(binarioLoop^.elemento));
		IF comparacion = greater THEN
            IF NOT TieneHijoDerecho(binarioLoop) THEN
               binarioLoop^.derecho := CrearHoja(i);
               binarioLoop^.derecho^.padre := binarioLoop;
               continue := FALSE;
            ELSE
               binarioLoop := Derecho(binarioLoop);
            END;
        ELSIF comparacion = less THEN
            IF NOT TieneHijoIzquierdo(binarioLoop) THEN
               binarioLoop^.izquierdo := CrearHoja(i);
               binarioLoop^.izquierdo^.padre := binarioLoop;
               continue := FALSE;
            ELSE
               binarioLoop := Izquierdo(binarioLoop);
            END;
		ELSE  
			continue := FALSE;
		END;
   END;

END InsertarBinario;

PROCEDURE Filtrar (clave: CARDINAL; criterio: TCritFiltro; a: Binario): BoolBinario;
(* Si ningun nodo de 'a' cumple la condicion "clave criterio NumeroInfo (nodo)",
   devuelve un elemento cuyo discriminador 'hayBinario' es FALSE.
   En otro caso, devuelve un arbol con los nodos que cumplen la condicion,
   y 'hayBinario' es TRUE.
   En general, en el arbol devuelto se debe mantener las relaciones
   ancestro-descendendiente que hay en 'a'. La excepcion se da cuando un nodo
   no cumple la condicion y tiene descendientes tanto por izquierda como por
   derecha que la cumplen.  En ese caso, para mantener ordenado el
   arbol a devolver se siguen los mismos criterios que en RemoverABB.
   (ver ejemplos en LetraTarea2.pdf)
   El arbol devuelto no comparte memoria con 'a'. *)

	PROCEDURE Filtro(clave: CARDINAL; criterio: TCritFiltro; VAR a, nuevo: Binario): BOOLEAN;
	VAR filtro: BOOLEAN;
	BEGIN
		filtro := FALSE;
		IF TieneHijoDerecho(a) AND Filtro(clave, criterio, a^.derecho, nuevo) THEN filtro := TRUE; END;
		IF TieneHijoIzquierdo(a) AND Filtro(clave, criterio, a^.izquierdo, nuevo) THEN filtro := TRUE; END;
		IF ((criterio = FltMayor) AND (clave > NumeroInfo(a^.elemento)))
		OR ((criterio = FltMenor) AND (clave < NumeroInfo(a^.elemento)))
		OR ((criterio = FltIgual) AND (clave = NumeroInfo(a^.elemento))) THEN
			InsertarEnBinario(a^.elemento, nuevo);
			filtro := TRUE;
		END;
		RETURN filtro;
	END Filtro;

VAR 
   nuevo : Binario;
   resultado : BoolBinario;
BEGIN

	nuevo := CrearHoja();
	IF NOT EsHoja(a) AND Filtro(clave, criterio, a, nuevo) THEN
		resultado.hayBinario := TRUE;
		resultado.arbol := nuevo;
	ELSE
		DestruirBinario(nuevo);
		resultado.hayBinario := FALSE;
	END;
	RETURN resultado;

END Filtrar;
    
(********************)
(*** DESTRUCTORES ***)
(********************)

PROCEDURE RemoverDeBinario (txt: TString; VAR a: Binario);
(* Precondicion: si EsHoja (a), el dato de texto de su elemento no es 'txt'.
   Remueve de 'a' el nodo con el elemento cuyo dato de texto es 'txt'.
   Si ninguno de los nodos de 'a' tiene un elemento cuyo dato de texto sea igual
   a 'txt', no hace nada.
   Si el nodo a eliminar tiene subarboles izquierdo y derecho, debe ser
   sustituido por el mayor del subarbol izquierdo, segun la propiedad de orden
   definida.
   Libera la memoria del nodo y del elemento. *)

   PROCEDURE RemoverNodo (VAR a: Binario; esDerecho: BOOLEAN);
	VAR aNuevo, aMenor: Binario;
	BEGIN
		aNuevo := NIL;
		IF TieneHijoDerecho(a) AND TieneHijoIzquierdo(a) THEN
			(* Busco el nodo para poner en lugar de "borrar" *)
			aNuevo := a^.izquierdo;
			WHILE TieneHijoDerecho(aNuevo) DO
				aNuevo := aNuevo^.derecho;
			END;
			(* Separo "aNuevo" de el arbol *)
			aNuevo^.padre^.derecho := NIL;
			aNuevo^.padre := NIL;
			(* Busco el menor de "aNuevo" *)
			aMenor := aNuevo;
			WHILE TieneHijoIzquierdo(aMenor) DO
				aMenor := aMenor^.izquierdo;
			END;
			(* Muevo la rama izquierda de "a" a la mas izquierda de "aNuevo" *)
			aMenor^.izquierdo := a^.izquierdo;
			a^.izquierdo^.padre := aMenor;
			(* Muevo la rama derecha de "a" a la derecha de "aNuevo" *)
			aNuevo^.derecho := a^.derecho;
			aNuevo^.derecho^.padre := aNuevo;
		ELSIF TieneHijoDerecho(a) THEN
			aNuevo := a^.derecho;
		ELSIF TieneHijoIzquierdo(a) THEN
			aNuevo := a^.izquierdo;
		END;
		IF TieneHijoDerecho(a) OR TieneHijoIzquierdo(a) THEN
			(* Agrego a "aNuevo" en lugar de "a" *)
			aNuevo^.padre := a^.padre;
			IF NOT (aNuevo^.padre = NIL) THEN
				IF esDerecho THEN
					aNuevo^.padre^.derecho := aNuevo;
				ELSE
					aNuevo^.padre^.izquierdo := aNuevo;
				END;
			END;
		END;
		DestruirInfo(a^.elemento);
		DISPOSE(a);
		(* Fin *)
		a := aNuevo;
	END RemoverNodo;

   PROCEDURE Iterar (txt: TString; a: Binario; esDerecho: BOOLEAN);
   BEGIN
      IF NOT (a = NIL) THEN
         CASE Compare(txt, TextoInfo(a^.elemento)) OF
            greater: Iterar(txt, a^.derecho, TRUE); |
            less: Iterar(txt, a^.izquierdo, FALSE); |
            equal: RemoverNodo(a, esDerecho); 
         END;
      END;
   END Iterar;

BEGIN

   IF NOT EsHoja(a) THEN
      CASE Compare(txt, TextoInfo(a^.elemento)) OF
         equal: DestruirBinario(a);
      END;
   ELSE
      Iterar(txt, a, FALSE);
   END;

END RemoverDeBinario;

PROCEDURE DestruirBinario (VAR a: Binario);
(* Libera la memoria asignada a 'a' y todos sus elementos. *)
BEGIN

   IF a # NIL THEN
      IF TieneHijoIzquierdo(a) THEN
         DestruirBinario(a^.izquierdo);
      END;
      IF TieneHijoDerecho(a) THEN
         DestruirBinario(a^.derecho);
      END;
      DestruirInfo(a^.elemento);
      DISPOSE(a);
   END;
   
END DestruirBinario;

(******************)
(*** PREDICADOS ***)
(******************)

PROCEDURE TieneHijoIzquierdo (a: Binario): BOOLEAN;
(* Devuelve TRUE si la raiz de 'a' tiene subarbol izquierdo o FALSE en caso
   contrario. *)
BEGIN

   IF a^.izquierdo = NIL THEN
      RETURN FALSE;
   ELSE
      RETURN TRUE;
   END;
   
END TieneHijoIzquierdo;

PROCEDURE TieneHijoDerecho (a: Binario): BOOLEAN;
(* Devuelve TRUE si la raiz de 'a' tiene subarbol derecho o FALSE en caso
   contrario. *)
BEGIN

   IF a^.derecho = NIL THEN
      RETURN FALSE;
   ELSE
      RETURN TRUE;
   END;
   
END TieneHijoDerecho;

PROCEDURE EsHoja (a: Binario): BOOLEAN;
(* Devuelve TRUE si 'a' esta formado unicamente por una hoja o FALSE en caso
   contrario. *)
BEGIN

   IF NOT TieneHijoDerecho(a) AND NOT TieneHijoIzquierdo(a) THEN
      RETURN TRUE;
   END;

   RETURN FALSE;
   
END EsHoja;
 
(******************)
(*** SELECTORES ***)
(******************)
   
PROCEDURE RaizBinario (a: Binario): TInfo;
(* Devuelve el elemento del nodo que esta en la raiz de 'a'. *)
BEGIN

   RETURN a^.elemento;
   
END RaizBinario;

PROCEDURE Izquierdo (a: Binario): Binario;
(* Precondicion: TieneHijoIzquierdo (a).
   Devuelve el subarbol izquierdo del nodo raiz de 'a'. *)
BEGIN
   
   RETURN a^.izquierdo;

END Izquierdo;

PROCEDURE Derecho (a: Binario): Binario;
(* Precondicion: TieneHijoDerecho (a).
   Devuelve el subarbol derecho del nodo raiz de 'a'. *)
BEGIN

   RETURN a^.derecho;
   
END Derecho;

PROCEDURE AlturaBinario (a: Binario): CARDINAL;
(* Devuelve la altura de 'a'.
   La altura de una hoja es 1. *)
VAR alturaDerecho, alturaIzquierdo, resultado : CARDINAL;
BEGIN

   resultado := 0;
   alturaDerecho := 0;
   alturaIzquierdo := 0;
   IF NOT EsHoja(a) THEN
      IF TieneHijoDerecho(a) THEN
         alturaDerecho := AlturaBinario(a^.derecho);
      END;
      IF TieneHijoIzquierdo(a) THEN
         alturaIzquierdo := AlturaBinario(a^.izquierdo);
      END;
      IF alturaDerecho > alturaIzquierdo THEN
         resultado := alturaDerecho;
      ELSE
         resultado := alturaIzquierdo;
      END;
   END;

   RETURN resultado + 1;
   
END AlturaBinario;

PROCEDURE CantidadBinario (a: Binario): CARDINAL;
(* Devuelve la cantidad de nodos de 'a'. *)
VAR nodosDerecha, nodosIzquierda, resultado : CARDINAL;
BEGIN

   IF EsHoja(a) THEN
      resultado := 1;
   ELSE
      nodosDerecha := 0;
      nodosIzquierda := 0;
      IF TieneHijoDerecho(a) THEN
         nodosDerecha := CantidadBinario(a^.derecho) + 1;
      END;
      IF TieneHijoIzquierdo(a) THEN
         nodosIzquierda := CantidadBinario(a^.izquierdo) + 1;
      END;
      resultado := nodosDerecha + nodosIzquierda;
   END;

   RETURN resultado;
   
END CantidadBinario;

PROCEDURE Linealizacion (a: Binario): ListaString;
(* Devuelve una lista con los datos de texto de los nodos de 'a. 
   La lista resultado debe estar en orden lexicografico creciente.
   En la lista resultado la posicion actual debe quedar al inicio.
   El tiempo de ejecucion es O(n), siendo 'n' la cantidad de nodos de 'a'. *)

	PROCEDURE Recorrer(a: Binario; VAR l: ListaString);
	BEGIN
		InsertarEnLista(TextoInfo(a^.elemento), l);
		IF TieneHijoIzquierdo(a) THEN
			Recorrer(Izquierdo(a), l);
		END;
		IF TieneHijoDerecho(a) THEN
			Recorrer(Derecho(a), l);
		END;
	END;

VAR l: ListaString;
BEGIN

	l := CrearLista();
	Recorrer(a, l);
	IrInicioLista(l);
	RETURN l;
   
END Linealizacion;

PROCEDURE BuscarABB (txt: TString; a: Binario): BoolBinario;
(* Devuelve el subarbol que tiene como raiz al elemento cuyo dato de texto es
   'txt' y el discriminador 'hayBinario' del elemanto devuelto es TRUE.
   Si 'txt' no pertenece a 'a', 'hayBinario' es FALSE. *)

   PROCEDURE BuscarTexto (txt: TString; a: Binario; VAR b: Binario): BOOLEAN;
   BEGIN
      CASE Compare(TextoInfo(RaizBinario(a)), txt) OF
         equal: 
            b := a;
            RETURN TRUE;
      ELSE
         IF NOT EsHoja(a) THEN
            IF (TieneHijoIzquierdo(a) AND (BuscarTexto(txt, Izquierdo(a), b))) 
            OR (TieneHijoDerecho(a) AND (BuscarTexto(txt, Derecho(a), b))) THEN
               RETURN TRUE;
            END;
         END;
      END;
      RETURN FALSE;
   END BuscarTexto;

VAR resultado : BoolBinario;
   subArbol: Binario;
BEGIN

   IF BuscarTexto(txt, a, subArbol) THEN
      resultado.hayBinario := TRUE;
      resultado.arbol := subArbol;
   ELSE
      resultado.hayBinario := FALSE;
   END;
   RETURN resultado;

END BuscarABB;

(********************)
(****** SALIDA ******)
(********************)

PROCEDURE ImprimirBinario (a: Binario);
(* Imprime en orden inverso.
   La indentacion de cada nodo es su nivel. *)
   PROCEDURE ImprimirNodos (a: Binario; espacios: CARDINAL);
   VAR espaciosImpresos : CARDINAL;
   BEGIN
      espaciosImpresos := 1;
      INC(espacios);
      IF TieneHijoDerecho(a) THEN
         ImprimirNodos(a^.derecho, espacios);
      END;
      WHILE espaciosImpresos <= espacios DO
         WriteString(" ");
         INC(espaciosImpresos);
      END;
      WriteString(InfoAString(a^.elemento));
      WriteLn();
      IF TieneHijoIzquierdo(a) THEN
         ImprimirNodos(a^.izquierdo, espacios);
      END;
   END ImprimirNodos;

BEGIN

   ImprimirNodos(a, 0);

END ImprimirBinario;   
 
END Binario.
