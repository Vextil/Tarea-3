IMPLEMENTATION MODULE Sistema;
(*******************************************************************************
Modulo de implementacion de Sistema.

En Sistema se mantiene los procesos que estan corriendo y los recursos que
solicitan. 

Laboratorio de Programacion 2.
InCo-FIng-UDELAR
*******************************************************************************)

FROM ListaString IMPORT ListaString;
FROM Solicitudes IMPORT RangoRecursos;
FROM Utils       IMPORT TString;

TYPE 
   Sistema;
   
PROCEDURE BootearSistema (mem: CARDINAL): Sistema;
(* Crea un sistema con un unico proceso identificado como 'boot' y  que consume
   'mem' megabytes de memoria. *)

PROCEDURE CorrerProceso (id: TString; mem: CARDINAL; VAR s: Sistema);
(*  Agrega a 's' el proceso 'id' que consume 'mem' megabytes de memoria. 
    Si 'id' ya estaba en 's' no hace nada. *)

PROCEDURE PedirRecurso (id: TString; rec: RangoRecursos; VAR s: Sistema);
(*  Registra que el proceso 'id' pide el recurso 'rec'. 
    Si 'id' no está en 's' no hace nada. Si 'id' ya habia pedido 'rec' repite el
    pedido. *)

PROCEDURE SolicitanOMuyConsumidores (mem: CARDINAL; rec: RangoRecursos; 
                                    s: Sistema): ListaString;
(* Devuelve una lista en orden lexicografico creciente estricto (o sea, sin 
   elementos repetidos) de los identificadores de proceso de 's' que solicitan
   el recurso 'r' o que la memoria que consumen es mayor a 'mem'. *)   

PROCEDURE ApagarSistema (VAR s: Sistema );
(* Libera toda la memoria asignada a 's' y a sus procesos y recursos. *)

END Sistema.
