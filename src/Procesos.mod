IMPLEMENTATION MODULE Procesos;
(*******************************************************************************
Modulo de implementacion de Procesos.

Procesos es una coleccion no vacia de elementos de tipo Proceso organizados en un
arbol binario de busqueda.
Los elementos de tipo Proceso tienen un dato identificador de tipo texto
(TString) y un dato numerico que representa la cantidad de memoria usada.


Laboratorio de Programacion 2.
InCo-FI-UDELAR
*******************************************************************************)

FROM STextIO     IMPORT WriteString, WriteLn;
FROM ListaString IMPORT ListaString, IrInicioLista;
FROM Utils       IMPORT TInfo, TString, CrearInfo, NumeroInfo, TCritFiltro, InfoAString;
FROM Binario     IMPORT Binario, CrearHoja, InsertarEnBinario, DestruirBinario, RemoverDeBinario, CantidadBinario, BoolBinario, BuscarABB, Linealizacion, Filtrar, Izquierdo, Derecho, TieneHijoIzquierdo, RaizBinario, TieneHijoDerecho, AlturaBinario;

TYPE
   Procesos = Binario;

PROCEDURE CrearProcesos (texto: TString; num: CARDINAL): Procesos;
(* Devuelve una coleccion de procesos conteniendo unicamente un Proceso con dato
   de texto 'texto' y dato numerico 'num'. *)
BEGIN
   
   RETURN CrearHoja(CrearInfo(num, texto));

END CrearProcesos;

PROCEDURE AgregarProceso (texto: TString; num: CARDINAL; VAR p: Procesos);
(* Si en 'p' ya hay un elemento con dato de texto 'texto', no hace nada.
   De lo contrario, agrega en 'p' un Proceso con dato de texto 'texto' y dato
   numerico 'num'. *)
BEGIN
   
   InsertarEnBinario(CrearInfo(num, texto), p);

END AgregarProceso;

PROCEDURE EliminarProceso (texto: TString; VAR p: Procesos);
(* Si en 'p' no hay un elemento con dato de texto 'texto', no hace nada.
   Si en 'p' hay un solo elemento y su dato de texto es 'texto', destruye 'p'.
   De lo contrario, elimina de 'p' el elemento cuyo dato de texto es 'texto' y
   libera la memoria asignada al mismo. *)
BEGIN
   
   RemoverDeBinario(texto, p);

END EliminarProceso;

PROCEDURE CantidadProcesos (p: Procesos): CARDINAL;
(* Devuelve la cantidad de elementos de 'p'. *)
BEGIN
   
   RETURN CantidadBinario(p);

END CantidadProcesos;

PROCEDURE IncluyeProceso (texto: TString; p: Procesos): BOOLEAN;
(* Devuelve TRUE si en 'p' hay un elemento con dato de texto 'texto' o
   FALSE en caso contrario. *)
VAR busqueda : BoolBinario;
BEGIN
   
   busqueda := BuscarABB(texto, p);
   RETURN busqueda.hayBinario;

END IncluyeProceso;

PROCEDURE ValorProceso (texto: TString; p: Procesos): CARDINAL;
(*  Precondicion: IncluyeProceso (texto, p).
    Devuelve el dato numerico del elemento de 'p' con dato de texto 'texto'. *)
VAR 
   busqueda : BoolBinario;
   info : TInfo;
BEGIN
   
   busqueda := BuscarABB(texto, p);
   info := RaizBinario(busqueda.arbol);
   RETURN NumeroInfo(info);

END ValorProceso;

PROCEDURE ImprimirProcesos (p: Procesos);
(* Imprime el arbol de procesos, un nivel por linea, de abajo hacia arriba y
   de izquierda a derecha.
   Cada elemento se imprime con el formato (numero,texto) seguido de un espacio
   en blanco. *)

   PROCEDURE ImprimirNivel(nActual, nImprimir : CARDINAL; a: Binario);
   BEGIN
      IF nActual = nImprimir THEN
         WriteString(InfoAString(RaizBinario(a)));
         WriteString(" ");
      ELSE
         IF TieneHijoIzquierdo(a) THEN
            ImprimirNivel(nActual + 1, nImprimir, Izquierdo(a));
         END;
         IF TieneHijoDerecho(a) THEN
            ImprimirNivel(nActual + 1, nImprimir, Derecho(a));
         END;
      END;
   END ImprimirNivel;

VAR i, altura : CARDINAL;
BEGIN
   
   altura := AlturaBinario(p);
   FOR i := 1 TO altura DO
      ImprimirNivel(i, (altura + 1) - i, p);
      IF NOT (i = altura) THEN
         WriteLn();
      END;
   END;

END ImprimirProcesos;


PROCEDURE ListarProcesos (p: Procesos): ListaString;
(* Devuelve una lista en orden lexicografico creciente de los identificadores de
   proceso de 'p'. *) 
VAR l : ListaString;
BEGIN
   
   l := Linealizacion(p);
   IrInicioLista(l);
   RETURN l;

END ListarProcesos;

PROCEDURE MuyConsumidores (mem: CARDINAL; p: Procesos): ListaString;
VAR filtrado : BoolBinario;
(* Devuelve una lista en orden lexicografico creciente de los identificadores de
   proceso de 'p' que la memoria que consumen es mayor a 'mem'. *) 
VAR l : ListaString;
BEGIN

   filtrado := Filtrar(mem, FltMenor, p);
   IF filtrado.hayBinario THEN
      l := Linealizacion(filtrado.arbol);
      IrInicioLista(l);
      RETURN l;
   END;     

END MuyConsumidores;  
   
PROCEDURE DestruirProcesos (VAR p: Procesos);
(* Libera la memoria asociada a 'p' y a todos sus elementos. *)
BEGIN
   
   DestruirBinario(p);

END DestruirProcesos;


END Procesos.
