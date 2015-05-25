(* 4623178 *)
IMPLEMENTATION MODULE Binario;

FROM Utils IMPORT TCritFiltro, TInfo, TString;
FROM ListaString IMPORT ListaString;

PROCEDURE CrearHoja (i: TInfo): Binario;
BEGIN
   
END CrearHoja;

PROCEDURE CopiaBinario (a: Binario): Binario;
BEGIN
   
END CopiaBinario;

PROCEDURE Balanceado (VAR l: ListaString): Binario;
BEGIN
   
END Balanceado;

PROCEDURE InsertarEnBinario (i: TInfo; VAR a: Binario);
BEGIN
   
END InsertarEnBinario;
   
PROCEDURE Filtrar (clave: CARDINAL; criterio: TCritFiltro; a: Binario):
BEGIN
   
END Filtrar;
 
PROCEDURE RemoverDeBinario (txt: TString; VAR a: Binario);
BEGIN
   
END RemoverDeBinario;

PROCEDURE DestruirBinario (VAR a: Binario);
BEGIN
   
END DestruirBinario;

PROCEDURE TieneHijoIzquierdo (a: Binario): BOOLEAN;
BEGIN
   
END TieneHijoIzquierdo;

PROCEDURE TieneHijoDerecho (a: Binario): BOOLEAN;
BEGIN
   
END TieneHijoDerecho;

PROCEDURE EsHoja (a: Binario): BOOLEAN;
BEGIN
   
END EsHoja;
 
PROCEDURE RaizBinario (a: Binario): TInfo;
BEGIN
   
END RaizBinario;

PROCEDURE Izquierdo (a: Binario): Binario;
BEGIN
   
END Izquierdo;

PROCEDURE Derecho (a: Binario): Binario;
BEGIN
   
END Derecho;

PROCEDURE AlturaBinario (a: Binario): CARDINAL;
BEGIN
   
END AlturaBinario;

PROCEDURE CantidadBinario (a: Binario): CARDINAL;
BEGIN
   
END CantidadBinario;

PROCEDURE Linealizacion (a: Binario): ListaString;
BEGIN
   
END Linealizacion;

PROCEDURE BuscarABB (txt: TString; a: Binario): BoolBinario;
BEGIN
   
END BuscarABB;

PROCEDURE ImprimirBinario (a: Binario);
BEGIN
   
END ImprimirBinario;
 
END Binario.
