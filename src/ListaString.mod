(* 4623178 *)
IMPLEMENTATION MODULE ListaString;

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

PROCEDURE CrearLista (): ListaString;
VAR lista : ListaString;
BEGIN
   
	NEW(lista);
	lista^.cantidad := 0;
	lista^.inicio := NIL;
	lista^.final := NIL;
	lista^.actual := NIL;

END CrearLista;
   
PROCEDURE CopiaLista (l: ListaString): ListaString;
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
  
PROCEDURE RemoverDeLista (VAR l: ListaString);
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
   
PROCEDURE EsVaciaLista (l: ListaString): BOOLEAN;
BEGIN

	RETURN l^.cantidad = 0;
   
END EsVaciaLista;

PROCEDURE EsPosicionValida (l: ListaString): BOOLEAN;   
BEGIN

	RETURN l^.actual # NIL;
   
END EsPosicionValida;
   
PROCEDURE EstaOrdenadaLista (l: ListaString): BOOLEAN;

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

PROCEDURE IrInicioLista (VAR l: ListaString);
BEGIN

	l^.actual := l^.inicio;
   
END IrInicioLista;

PROCEDURE IrSiguienteLista (VAR l: ListaString);
BEGIN

	IF l^.actual = l^.final THEN
		l^.actual := NIL;
	ELSE
		l^.actual := l^.actual^.siguiente;
	END;
   
END IrInicioLista;

PROCEDURE ActualLista (l: ListaString): TString;
BEGIN
   
	RETURN l^.actual^.elemento;

END ActualLista;
   
PROCEDURE CantidadLista (l: ListaString): CARDINAL;
BEGIN
   
	RETURN l^.cantidad;

END CantidadLista;
   
PROCEDURE ImprimirLista (l: ListaString);
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
