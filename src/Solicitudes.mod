(* 4623178 *)
IMPLEMENTATION MODULE Solicitudes;
(*******************************************************************************
Modulo de implementacion de Solicitudes.

Solicitudes mantiene cada solicitud de algun recurso realizada por algun proceso
Los recursos se identifican con numeros entre 1 y MAX_RECURSOS.
Cada proceso puede solicitar varias veces cada recurso.

Laboratorio de Programacion 2.
InCo-FI-UDELAR
*******************************************************************************)

FROM Strings       IMPORT Append;
FROM WholeStr      IMPORT CardToStr;
FROM Utils         IMPORT TString;
FROM ListaString   IMPORT ListaString, InsertarEnLista, CrearLista, DestruirLista;
FROM Set           IMPORT Set, ConstruirSet, DestruirSet, Union, Interseccion, Diferencia;
FROM ColaPrioridad IMPORT ColaPrioridad, MinimoColaPrioridad, EsVaciaColaPrioridad, CrearColaPrioridad, ExtraerDeMinimoColaPrioridad, PrioridadMinimoColaPrioridad, DestruirColaPrioridad, ElementosColaPrioridad, InsertarEnColaPrioridad;

TYPE Solicitudes = ColaPrioridad;
   
PROCEDURE MenorRecurso (s: Solicitudes): CARDINAL;
(* Devuelve el valor del menor recurso para el cual hay alguna solicitud o 
   0 si no hay solicitudes. 
   El tiempo de ejecucion es O(1). *)
BEGIN
   
   RETURN PrioridadMinimoColaPrioridad(s);

END MenorRecurso;
   
PROCEDURE CancelarSolicitud (VAR s: Solicitudes);
(* Cancela la solicitud mas antigua correspondiente al menor recurso para el que
   haya solicitudes. Si no hay solicitudes no hace nada.
   El tiempo de ejecucion es O(log (MAX_RECURSOS). *)   
BEGIN
   
   ExtraerDeMinimoColaPrioridad(s);

END CancelarSolicitud;

PROCEDURE CrearSolicitudes (): Solicitudes;
(* Devuelve una coleccion de solicitudes vacia (sin elementos). *)
BEGIN

   RETURN CrearColaPrioridad();
   
END CrearSolicitudes;

PROCEDURE IngresarSolicitud (r: RangoRecursos; id: TString; VAR s: Solicitudes);
(* Se ingresa en 's' la solicitud del recurso 'r' por parte del proceso con dato
   de texto 'id'. Si es la primera vez que algun proceso solicita el recurso
   'r', el tiempo de ejecucion es O(log MAX_RECURSOS). *)
BEGIN
   
   InsertarEnColaPrioridad(id, r, s);

END IngresarSolicitud;

PROCEDURE ListarSolicitudes (VAR s: Solicitudes): ListaString;
(* Devuelve una lista con todas las solicitudes de 's'.
   La lista debe estar ordenada de manera creciente segun el numero de recurso y
   en orden cronologico creciente entre las solicitudes del mismo recurso (esto
   es, dadas dos solicitudes para el mismo recurso debe aparecer primero la
   que se haya hecho antes).
   El formato de los elementos de la lista es (recurso,proceso).
   Asumiendo que hay N solicitudes, el tiempo de ejecucion es
   O(N + MAX_RECURSOS . log (MAX_RECURSOS) ).
   Al finalizar, 's' debe quedar vacia (sin solicitudes). *)
VAR 
   lista : ListaString;
   elemento, prioridad : TString;
BEGIN
   
   lista := CrearLista();
   WHILE NOT EsVaciaColaPrioridad(s) DO
      CardToStr(PrioridadMinimoColaPrioridad(s), prioridad);
      elemento := "(";
      Append(prioridad, elemento);
      Append(",", elemento);
      Append(MinimoColaPrioridad(s), elemento);
      Append(")", elemento);
      InsertarEnLista(elemento, lista);
      ExtraerDeMinimoColaPrioridad(s);
   END;
   RETURN lista;

END ListarSolicitudes;

PROCEDURE EnAlgunoSolicitudes (r1, r2: RangoRecursos; s: Solicitudes): Set;
(* Devuelve un conjunto con los procesos que solicitan alguno de los recursos
   'r1' o 'r2'. *)
VAR 
   l1, l2 : ListaString;
   s1, s2, resultado : Set;
BEGIN
   
   l1 := ElementosColaPrioridad(r1, s);
   l2 := ElementosColaPrioridad(r2, s);
   s1 := ConstruirSet(l1);
   s2 := ConstruirSet(l2);
   resultado := Union(s1, s2);
   DestruirSet(s1);
   DestruirSet(s2);
   RETURN resultado;

END EnAlgunoSolicitudes;

PROCEDURE EnAmbosSolicitudes (r1, r2: RangoRecursos; s: Solicitudes): Set;
(* Devuelve un conjunto con los procesos que solicitan los recursos
   'r1' y 'r2'. *)
VAR 
   l1, l2 : ListaString;
   s1, s2, resultado : Set;
BEGIN
   
   l1 := ElementosColaPrioridad(r1, s);
   l2 := ElementosColaPrioridad(r2, s);
   s1 := ConstruirSet(l1);
   s2 := ConstruirSet(l2);
   resultado := Interseccion(s1, s2);
   DestruirSet(s1);
   DestruirSet(s2);
   RETURN resultado;

END EnAmbosSolicitudes;

PROCEDURE SoloEnUnoSolicitudes (r1, r2: RangoRecursos; s: Solicitudes): Set;
(* Devuelve un conjunto con los procesos que solicitan uno de los recursos 'r1'
   o r2' pero no el otro. *)
VAR 
   l1, l2 : ListaString;
   s1, s2, s3, resultado : Set;
BEGIN
   
   l1 := ElementosColaPrioridad(r1, s);
   l2 := ElementosColaPrioridad(r2, s);
   s1 := ConstruirSet(l1);
   s2 := ConstruirSet(l2);
   s3 := Diferencia(s1, s2);
   s1 := Diferencia(s2, s1);
   resultado := Union(s1, s3);
   DestruirSet(s1);
   DestruirSet(s2);
   DestruirSet(s3);
   RETURN resultado;

END SoloEnUnoSolicitudes;

PROCEDURE DestruirSolicitudes (VAR s: Solicitudes);
(* Libera la memoria asignada a 's'. *)
BEGIN
   
   IF NOT EsVaciaColaPrioridad(s) THEN
      DestruirColaPrioridad(s);
   END;

END DestruirSolicitudes;

END Solicitudes.
