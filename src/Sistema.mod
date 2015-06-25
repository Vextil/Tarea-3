(* 4623178 *)
IMPLEMENTATION MODULE Sistema;
(*******************************************************************************
Modulo de implementacion de Sistema.

En Sistema se mantiene los procesos que estan corriendo y los recursos que
solicitan. 

Laboratorio de Programacion 2.
InCo-FIng-UDELAR
*******************************************************************************)

FROM Storage     IMPORT ALLOCATE, DEALLOCATE;
FROM ListaString IMPORT ListaString, EsPosicionValida, DestruirLista, CrearLista, EsVaciaLista, RemoverDeLista, InsertarEnLista, ActualLista;
FROM Solicitudes IMPORT RangoRecursos;
FROM Utils       IMPORT TString;
FROM Solicitudes IMPORT Solicitudes, CrearSolicitudes, IngresarSolicitud, EnAmbosSolicitudes, DestruirSolicitudes;
FROM Procesos    IMPORT Procesos, CrearProcesos, AgregarProceso, ListarProcesos, MuyConsumidores, DestruirProcesos;
FROM Set         IMPORT Set, InsertarEnSet, PerteneceASet;

TYPE 
   Sistema = POINTER TO TipoSistema;
   TipoSistema = RECORD
		solicitudes : Solicitudes;
		procesos : Procesos;
   END;
   
PROCEDURE BootearSistema (mem: CARDINAL): Sistema;
(* Crea un sistema con un unico proceso identificado como 'boot' y  que consume
   'mem' megabytes de memoria. *)
VAR sistema : Sistema;
BEGIN
	
	NEW(sistema);
	sistema^.solicitudes := CrearSolicitudes();
	sistema^.procesos := CrearProcesos("boot", mem);
	RETURN sistema;

END BootearSistema;

PROCEDURE CorrerProceso (id: TString; mem: CARDINAL; VAR s: Sistema);
(*  Agrega a 's' el proceso 'id' que consume 'mem' megabytes de memoria. 
    Si 'id' ya estaba en 's' no hace nada. *)
BEGIN
	
	AgregarProceso(id, mem, s^.procesos);

END CorrerProceso;

PROCEDURE PedirRecurso (id: TString; rec: RangoRecursos; VAR s: Sistema);
(*  Registra que el proceso 'id' pide el recurso 'rec'. 
    Si 'id' no está en 's' no hace nada. Si 'id' ya habia pedido 'rec' repite el
    pedido. *)
BEGIN
	
	IngresarSolicitud(rec, id, s^.solicitudes);

END PedirRecurso;

PROCEDURE SolicitanOMuyConsumidores (mem: CARDINAL; rec: RangoRecursos; s: Sistema): ListaString;
(* Devuelve una lista en orden lexicografico creciente estricto (o sea, sin 
   elementos repetidos) de los identificadores de proceso de 's' que solicitan
   el recurso 'r' o que la memoria que consumen es mayor a 'mem'. *)   
VAR	
	setResultado : Set;
	listaConsumidores, listaProcesos, listaResultado : ListaString;
BEGIN
	
	setResultado := EnAmbosSolicitudes(rec, rec, s^.solicitudes);
	listaConsumidores := MuyConsumidores(mem, s^.procesos);
	WHILE EsPosicionValida(listaConsumidores) DO
		InsertarEnSet(ActualLista(listaConsumidores), setResultado);
		RemoverDeLista(listaConsumidores);
	END;
	DestruirLista(listaConsumidores);
	listaProcesos := ListarProcesos(s^.procesos);
	listaResultado := CrearLista();
	WHILE EsPosicionValida(listaProcesos) DO
		IF PerteneceASet(ActualLista(listaProcesos), setResultado) THEN
			InsertarEnLista(ActualLista(listaProcesos), listaResultado);
		END;
		RemoverDeLista(listaProcesos);
	END;
	DestruirLista(listaProcesos);
	RETURN listaResultado;

END SolicitanOMuyConsumidores;

PROCEDURE ApagarSistema (VAR s: Sistema );
(* Libera toda la memoria asignada a 's' y a sus procesos y recursos. *)
BEGIN
	
	DestruirSolicitudes(s^.solicitudes);
	DestruirProcesos(s^.procesos);
	DISPOSE(s);

END ApagarSistema;

END Sistema.
