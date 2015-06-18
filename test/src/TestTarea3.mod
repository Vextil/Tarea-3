MODULE TestTarea3;
(******************************************************************************
Interprete de comandos para probar los modulos.

Se dispone de un arreglo de listas, uno de binarios y uno de sets, (que se 
identifican con numeros desde el 1) una pila,
una cola de binarios y una cola de prioridad que se crean
al inicio y destruyen al concluir.
Hay una coleccion de procesos. Se crea al inicio y cada vez que se elimine el
ultimo, con un único proceso identificado con 'boot'. Se destruye al concluir.
También hay una coleccion de solicitudes y un Sistema.
Se asume que los comandos estan bien formados. Esto significa que
- el nombre de comando es uno de los listados mas abajo;
- la cantidad y el tipo de parametros corresponden al nombre;
- se cumplen las precondiciones de las operaciones invocadas.

Los parametros de tipo texto que se ingresen no deben contener espacios.

Los comandos posibles son:

   Fin
      Termina el programa.

   # comentario
      Imprime 'comentario'.

   Reinicio
      Destruye todas las estructuras y vuelve a crearlas.

   ********************************* ListaString **************************

   CrearLista id texto1 texto2 texto3 texto4 ... FIN

   CopiaLista id1 id2
      Crea una lista con id 'id2' con los elementos de la lista con id 'id1'.

   PartirLista id1 id2
      Parte la lista con id 'id1', dejando la segunda mitad en la lista con id
      'id2'.

   RemoverDeLista id pos
      Remueve el 'pos'-esimo elemento de la lista con id 'id' si tiene al
      menos esa cantidad de elementos.

   ImprimirLista id
      Imprime la lista con id 'id' e informa si esta ordenada.

   ******************************** Binario *******************************

   CrearBinario texto1 num1 id texto2 num2 texto3 num3 texto4 num4 ... FIN
      Crea un binario con id 'id', con los elementos de tipo TInfo con
      texto y numero pasados como parametros, hasta encontrar el texto FIN.
      Notar que comienza con un TInfo, luego con el binario y luego sigue
      con los demas TInfo. Se pide uno al principio para poder comenzar
      a crear el binario, que tiene que tener al menos un nodo.

   CrearBalanceado idL idB
      Crea un binario balanceado con id 'idB' con los elementos que estan en la
      lista con id 'idL'.

   Izquierdo id1 id2
      Crea un binario con id 'id2' que es copia del subarbol izquierdo del
      binario con id 'id1', si existe.

   Derecho id1 id2
      Crea un binario con id 'id2' que es copia del subarbol derecho del
      binario con id 'id1', si existe.

   Linealizar idB idL
      Crea una lista con id 'idL' que resulta del recorrido en orden del binario
      con id 'idB'.

   AlturaCantidadBinario id
      Imprime la altura y cantidad de nodos del binario con id 'id'.

   ImprimirBinario id
      Imprime el binario con id 'id'.

   RemoverBinario id txt
      Remueve del binario con id 'id' el nodo con dato de texto 'txt'.

   FiltroBinario id1 num criterioFiltro id2
      Dado el binario con id 'id1', llama a la funcion Filtrar de Binario
      con los parametros 'num' y 'criterioFiltro' (que puede ser "<",
      ">" o "="), guardando el resultado en el binario con id 'id2'.

   BuscarABB id texto
      Busca el texto 'texto' en el binario con id 'id' e imprime el
      subarbol que tiene dicho campo de texto como raiz. Si no lo
      encuentra imprime que no lo encontro.

   ********************************* Pila *********************************

   Apilar texto num
      Apila (num, texto)

   Desapilar
      Desapila e imprime lo que habia en el tope de la pila o, si esta vacia,
      lo indica.

   ********************************* ColaBinario **************************

   Encolar idB
      Encola el binario con id 'idB'.

   Desencolar
      Desencola e imprime lo que habia al frente de la cola o, si esta vacia,
      lo indica.

   ********************************* Set **********************************

   ConstruirSet idL idS
      Crea el set con id 'idS' con los elementos de la lista con id 'idL'.

   CopiarSet id1 id2
      Crea un set con id 'id2' que es copia del set con id 'id1'.

   UnionInterDif id1 id2 idU idI idD1 idD2
      Crea sets con operaciones entre los sets con ids 'id1' e 'id2':
         - en el de id 'idU', la union
         - en el de id 'idI', la interseccion
         - en el de id 'idD1', la diferencia entre 'id1' e 'id2'
         - en el de id 'idD2', la diferencia entre 'id2' e 'id1'

   InsertarEnSet idS idL
      Inserta en el set con id 'idS' los elementos de la lista con id 'idL'.

   RemoverDeSet idS idL
      Remueve del set con id 'idS' los elementos de la lista con id 'idL'.

   PerteneceASet idS idL
      Para cada uno de los elementos de la lista con id 'idL' imprime 'SI' o
      'NO' si pertenece o no al set con id 'idS'.

   ********************************* ColaPrioridad ************************

   InsertarCP texto1 prio1 texto2 prio2 texto3 prio3 texto4 prio4 ... FIN
      Inserta en la cola de prioridad cada uno de los textoI con su
      correspondiente prioridad prioI.

   ExtraerCP
      Si la cola de prioridad no es vacia extrae el minimo e imprime el texto
      y la prioridad del que era minimo.

   ElementosCP prio idL
      Crea la lista con id 'idL' con los elementos con prioridad 'prio' o,
      si no hay ninguno, imprime que no hay elementos con esa prioridad.


   ******************************** Procesos ******************************
   AgregarProcesos id1 mem1 id2 mem2 id3 mem3 id4 mem4 ... FIN
      Agrega los procesos con ids y cantidad de memorias pasados a la
      coleccion de procesos.

   EliminarProceso id
      Elimina el proceso con id 'id'.
         
   ImprimirProcesos
      Imprime el arbol de procesos de la coleccion.
   
   ListarProcesos
      Imprimre los procesos por orden alfabetico.
   
   MuyConsumidores mem
      Imprime los procesos que consumen mas memoria que 'mem'.

   ******************************** Solicitudes ***************************
   IngresarSolicitudes proc1 rec1 proc2 rec2 proc3 rec3 ... FIN
      Ingresa las solicitudes de los nombres de procesos y numero de
      recursos pasados.
   
   CancelarSolicitud
      Cancela la solicitud mas antigua correspondiente al menor recurso para el
      que haya solicitudes
      
   EnAlgunoSolicitudes rec1 rec2 idSet
      Devuelve el resultado de aplicar EnAlgunoSolicitudes con 'rec1',
      'rec2' y el conjunto con id 'idSet'
   
   EnAmbosSolicitudes rec1 rec2 idSet
      Devuelve el resultado de aplicar EnAmbosSolicitudes con 'rec1',
      'rec2' y el conjunto con id 'idSet'
   
   SoloEnUnoSolicitudes rec1 rec2 idSet
      Devuelve el resultado de aplicar SoloEnUnoSolicitudes con 'rec1',
      'rec2' y el conjunto con id 'idSet'

   ListarSolicitudes
      Lista las solicitudes de la coleccion.

   ******************************** Sistema *******************************
   CorrerProcesos id1 mem1 id2 mem2 id3 mem3 id4 mem4 ... FIN
      Manda a correr en el sistema los procesos con id y cantidad de
      memoria.

   PedirRecursos proc1 rec1 proc2 rec2 proc3 rec3 ... FIN
      Hace la solicitud de recursos en el sistema por parte de los nombres
      de procesos y numeros de recursos pasados.
   
   SolicitanOMuyConsumidores mem rec
      Devuelve los procesos que consumen el recurso 'rec', o que consumen
      mas memoria que 'mem'.

   ******************************** Memoria ********************************
   Los siguientes son tests para comprobar la correcta gestion de la memoria.  

   MemoriaLista

   MemoriaBinario

   MemoriaPila
   
   MemoriaColaBinario
   
   MemoriaSet

   MemoriaColaPrioridad
   
   MemoriaProcesos
   
   MemoriaSolicitudes
   
   MemoriaSistema
   
   ******************************** Tiempo ********************************
   Los siguientes son tests para comprobar que se cumplan los tiempos 
   requeridos en la ejecucion de las operaciones de los modulos.
   Cada uno deberia demorar aproximadamente un segundo.   
   
   TiempoLista

   TiempoBinario
   
   TiempoPila
   
   TiempoColaBinario

   TiempoSet

   TiempoColaPrioridad
   
   TiempoSolicitudes

      
   
Laboratorio de Programacion 2.
InCo-FI-UDELAR
******************************************************************************)

FROM     Binario       IMPORT AlturaBinario, Balanceado, Binario, BoolBinario,
                              BuscarABB, CantidadBinario, CopiaBinario,
                              CrearHoja, Derecho, DestruirBinario, EsHoja,
                              Filtrar, ImprimirBinario, InsertarEnBinario,
                              Izquierdo, Linealizacion, RaizBinario, 
                              RemoverDeBinario, TieneHijoDerecho,
                              TieneHijoIzquierdo;
FROM     ColaBinario   IMPORT ColaBinario, CrearColaBinario, DesencolarBinario,
                              DestruirColaBinario, EncolarBinario,
                              EsVaciaColaBinario, PrimeroColaBinario;
FROM     ColaPrioridad IMPORT ColaPrioridad, CrearColaPrioridad,
                              DestruirColaPrioridad, ElementosColaPrioridad,
                              EsVaciaColaPrioridad,
                              ExtraerDeMinimoColaPrioridad,
                              InsertarEnColaPrioridad, K, MinimoColaPrioridad,
                              PerteneceAColaPrioridad,
                              PrioridadMinimoColaPrioridad;
FROM     ListaString   IMPORT ActualLista, CantidadLista, CopiaLista,
                              CrearLista, DestruirLista, EsPosicionValida,
                              EstaOrdenadaLista, EsVaciaLista, ImprimirLista,
                              InsertarEnLista, IrInicioLista, IrSiguienteLista,
                              ListaString, PartirLista, RemoverDeLista;
FROM     Pila          IMPORT Apilar, CrearPila, Desapilar, DestruirPila,
                              EsVaciaPila, Pila, Tope;
FROM     Procesos      IMPORT AgregarProceso, CantidadProcesos, 
                              CrearProcesos, DestruirProcesos, EliminarProceso,
                              ImprimirProcesos, IncluyeProceso, ListarProcesos,
                              MuyConsumidores, Procesos, ValorProceso;                              
FROM     Set           IMPORT ConstruirSet, CopiaSet, CrearSet, DestruirSet,
                              Diferencia, EsVacioSet, InsertarEnSet,
                              Interseccion, PerteneceASet, RemoverDeSet, Set,
                              Union;
FROM     SIOResult     IMPORT ReadResult, ReadResults;
FROM     Sistema       IMPORT ApagarSistema, BootearSistema, CorrerProceso, 
                              PedirRecurso, Sistema, SolicitanOMuyConsumidores;
FROM     Solicitudes   IMPORT CancelarSolicitud, CrearSolicitudes, 
                              DestruirSolicitudes, EnAlgunoSolicitudes, 
                              EnAmbosSolicitudes, IngresarSolicitud, 
                              ListarSolicitudes,MAX_RECURSOS, MenorRecurso,
                              RangoRecursos, Solicitudes, SoloEnUnoSolicitudes;
FROM     StdChans      IMPORT StdErrChan;
FROM     STextIO       IMPORT ReadRestLine, ReadToken, SkipLine, WriteLn,
                              WriteString;
FROM     Storage       IMPORT ALLOCATE, DEALLOCATE;
FROM     Strings       IMPORT Equal;
FROM     SWholeIO      IMPORT WriteCard;
IMPORT   TextIO;
FROM     Utils         IMPORT Assert, CrearInfo, DestruirInfo, InfoAString,
                              NumeroInfo, TCritFiltro, TextoInfo, TInfo,
                              TString;
FROM     WholeStr      IMPORT CardToStr, ConvResults, StrToCard;



CONST
   CANT_LISTAS                   = 10;
   CANT_BINARIOS                 = 10;
   CANT_SETS                     = 10;
   PROMPT                        = ">";


   CMD_FIN                       = "Fin";
   CMD_COMENTARIO                = "#";
   CMD_REINICIO                  = "Reinicio";

   CMD_CREAR_LISTA               = "CrearLista";
   CMD_COPIA_LISTA               = "CopiaLista";
   CMD_PARTIR_LISTA              = "PartirLista";
   CMD_REMOVER_LISTA             = "RemoverDeLista";
   CMD_IMPRIMIR_LISTA            = "ImprimirLista";

   CMD_CREAR_BINARIO             = "CrearBinario";
   CMD_BALANCEADO                = "CrearBalanceado";
   CMD_IZQUIERDO                 = "Izquierdo";
   CMD_DERECHO                   = "Derecho";
   CMD_LINEALIZAR                = "Linealizar";
   CMD_ALTURA_CANTIDAD_BINARIO   = "AlturaCantidadBinario";
   CMD_IMPRIMIR_BINARIO          = "ImprimirBinario";
   CMD_REMOVER_BINARIO           = "RemoverBinario";
   CMD_FILTRAR                   = "FiltroBinario";
   CMD_BUSCAR_ABB                = "BuscarABB";

   CMD_APILAR                    = "Apilar";
   CMD_DESAPILAR                 = "Desapilar";

   CMD_ENCOLAR                   = "Encolar";
   CMD_DESENCOLAR                = "Desencolar";

   CMD_CONS_SET                  = "ConstruirSet";
   CMD_COPIAR_SET                = "CopiarSet";
   CMD_UN_INT_DIF                = "UnionInterDif";
   CMD_INSERTAR_SET              = "InsertarEnSet";
   CMD_REMOVER_SET               = "RemoverDeSet";
   CMD_PERTENCE_SET              = "PerteneceASet";

   CMD_INSERTAR_CP               = "InsertarCP";
   CMD_EXTRAER_CP                = "ExtraerCP";
   CMD_ELEMENTOS_CP              = "ElementosCP";

   CMD_AGREGAR_PROCESOS          = "AgregarProcesos";
   CMD_ELIMINAR_PROCESOS         = "EliminarProceso";
   CMD_IMPRIMIR_PROCESOS         = "ImprimirProcesos";
   CMD_LISTAR_PROCESOS           = "ListarProcesos";
   CMD_MUY_CONSUMIDORES          = "MuyConsumidores";
   
   CMD_INGRESAR_SOLICITUDES      = "IngresarSolicitudes";
   CMD_CANCELAR_SOLICITUDES      = "CancelarSolicitud";
   CMD_EN_ALGUNO_SOLICITUDES     = "EnAlgunoSolicitudes";
   CMD_EN_AMBOS_SOLICITUDES      = "EnAmbosSolicitudes";
   CMD_SOLO_EN_UNO_SOLICITUDES   = "SoloEnUnoSolicitudes";
   CMD_LISTAR_SOLICITUDES        = "ListarSolicitudes";

   CMD_CORRER_PROCESOS           = "CorrerProcesos";
   CMD_PEDIR_RECURSOS            = "PedirRecursos";
   CMD_SOLICITAR_O_CONSUMIDORES  = "SolicitanOMuyConsumidores";   
   
   CMD_MEMORIA_LISTA             = "MemoriaLista";
   CMD_MEMORIA_BINARIO           = "MemoriaBinario";
   CMD_MEMORIA_PILA              = "MemoriaPila"; 
   CMD_MEMORIA_COLA_BINARIO      = "MemoriaColaBinario"; 
   CMD_MEMORIA_SET               = "MemoriaSet";
   CMD_MEMORIA_COLA_PRIORIDAD    = "MemoriaColaPrioridad";
   CMD_MEMORIA_PROCESOS          = "MemoriaProcesos";  
   CMD_MEMORIA_SOLICITUDES       = "MemoriaSolicitudes"; 
   CMD_MEMORIA_SISTEMA           = "MemoriaSistema";
   
   CMD_TIEMPO_LISTA              = "TiempoLista";
   CMD_TIEMPO_BINARIO            = "TiempoBinario";
   CMD_TIEMPO_PILA               = "TiempoPila";
   CMD_TIEMPO_COLA_BINARIO       = "TiempoColaBinario";
   CMD_TIEMPO_SET                = "TiempoSet";
   CMD_TIEMPO_COLA_PRIORIDAD     = "TiempoColaPrioridad";
   CMD_TIEMPO_SOLICITUDES        = "TiempoSolicitudes";

   

TYPE
   PBinario = POINTER TO Binario;
   PLista   = POINTER TO ListaString;
   PSet     = POINTER TO Set;
VAR
   binarios:      ARRAY [1..CANT_BINARIOS] OF RECORD
                     CASE existe: BOOLEAN OF
                        TRUE: pbinario: PBinario;
                     END;
                  END;
   boolBinario:   BoolBinario;
   boolean:       BOOLEAN; 
   cantidad:      CARDINAL;
   cardinal:      CARDINAL;
   colaBinarios:  ColaBinario;
   colaPrioridad: ColaPrioridad;
   contCmds:      CARDINAL;
   critFiltro:    TCritFiltro;
   esFinal:       BOOLEAN;
   info,
   info2:         TInfo;
   lista:         ListaString;
   listas:        ARRAY [1..CANT_LISTAS] OF RECORD
                     CASE existe: BOOLEAN OF
                        TRUE: plista: PLista;
                     END;
                  END;
   nomCmd:        TString;
   pararDeLeer:   BOOLEAN;
   pbinario:      PBinario;
   pbinario1:     PBinario;
   pbinario2:     PBinario;
   pila:          Pila;
   plista:        PLista;
   plista1:       PLista;
   plista2:       PLista;
   procesos:      Procesos;
   pset:          PSet;
   pset1:         PSet;
   pset2:         PSet;
   readResult:    ReadResults;
   rec1,
   rec2:          RangoRecursos;
   sets:          ARRAY [1..CANT_SETS] OF RECORD
                     CASE existe: BOOLEAN OF
                        TRUE: pset: PSet;
                     END;
                  END;
   sistema:       Sistema;       
   solicitudes:   Solicitudes;               
   texto:         TString;

PROCEDURE LeerTexto (): TString;
VAR
   result:     TString;
   readResult: ReadResults;
BEGIN
   ReadToken (result);
   readResult := ReadResult ();
   Assert (readResult <> endOfInput,
         "se esperaba un comando o mas parametros");
   Assert (readResult <> endOfLine, "se esperaba un comando o mas parametros");
   Assert (readResult = allRight, "no se pudo leer el comando o el parametro");
   RETURN result
END LeerTexto;

PROCEDURE LeerCardinal (): CARDINAL;
VAR
   resConv: ConvResults;
   result:  CARDINAL;
   texto:   TString;
BEGIN
   texto := LeerTexto ();
   StrToCard (texto, result, resConv);
   Assert (resConv = strAllRight,
         "no se pudo convertir el parametro a un numero");
   RETURN result
END LeerCardinal;

PROCEDURE LeerInfoRepeat (VAR info: TInfo; VAR pararDeLeer: BOOLEAN);
VAR
   num:     CARDINAL;
   texto:   TString;
BEGIN
   texto := LeerTexto ();

   IF Equal (texto, "FIN") THEN
      pararDeLeer := TRUE
   ELSE
      pararDeLeer := FALSE;

      num := LeerCardinal ();

      info := CrearInfo (num, texto)
   END
END LeerInfoRepeat;

PROCEDURE LeerInfo (): TInfo;
VAR
   num:     CARDINAL;
   texto:   TString;
BEGIN
   texto := LeerTexto ();
   num := LeerCardinal ();
   RETURN CrearInfo (num, texto)
END LeerInfo;

PROCEDURE LeerCriterioFiltro (): TCritFiltro;
VAR
   critFiltro: TCritFiltro;
   texto:      TString;
BEGIN
   texto := LeerTexto ();

   IF Equal (texto, "=") THEN
      critFiltro := FltIgual
   ELSIF Equal (texto, "<") THEN
      critFiltro := FltMenor
   ELSIF Equal (texto, ">") THEN
      critFiltro := FltMayor
   ELSE
      critFiltro := FltIgual; (* Para sacar el warning. *)
      Assert (FALSE, "criterio de filtro no reconocido")
   END;

   RETURN critFiltro
END LeerCriterioFiltro;


PROCEDURE LeerIndiceValidoLista (): CARDINAL;
VAR i: CARDINAL;
BEGIN
   i := LeerCardinal ();
   Assert ((0 < i) AND (i <= CANT_LISTAS),
         "indice de la lista fuera de rango");

   RETURN i
END LeerIndiceValidoLista;

PROCEDURE LeerListaExistente (): PLista;
VAR i: CARDINAL;
BEGIN
   i := LeerIndiceValidoLista ();

   Assert (listas [i].existe, "lista no inicializada");

   RETURN listas [i].plista
END LeerListaExistente;

PROCEDURE LeerListaInexistenteYAsignar (plista: PLista);
VAR i: CARDINAL;
BEGIN
   i := LeerIndiceValidoLista ();

   IF listas [i].existe THEN
      DestruirLista (listas [i].plista^);
   END;

   listas [i].existe := TRUE;
   listas [i].plista := plista
END LeerListaInexistenteYAsignar;

PROCEDURE LeerIndiceValidoBinario (): CARDINAL;
VAR i: CARDINAL;
BEGIN
   i := LeerCardinal ();
   Assert ((0 < i) AND (i <= CANT_BINARIOS),
         "indice del binario fuera de rango");

   RETURN i
END LeerIndiceValidoBinario;

PROCEDURE LeerBinarioExistente (): PBinario;
VAR i: CARDINAL;
BEGIN
   i := LeerIndiceValidoBinario ();

   Assert (binarios [i].existe, "binario no inicializado");

   RETURN binarios [i].pbinario
END LeerBinarioExistente;

PROCEDURE LeerBinarioInexistenteYAsignar (pbinario: PBinario);
VAR i: CARDINAL;
BEGIN
   i := LeerIndiceValidoBinario ();

   IF binarios [i].existe THEN
      DestruirBinario (binarios [i].pbinario^)
   END;

   binarios [i].existe := TRUE;
   binarios [i].pbinario := pbinario
END LeerBinarioInexistenteYAsignar;

PROCEDURE LeerIndiceValidoSet (): CARDINAL;
VAR i: CARDINAL;
BEGIN
   i := LeerCardinal ();
   Assert ((0 < i) AND (i <= CANT_SETS),
         "indice del set fuera de rango");

   RETURN i
END LeerIndiceValidoSet;

PROCEDURE LeerSetExistente (): PSet;
VAR i: CARDINAL;
BEGIN
   i := LeerIndiceValidoSet ();

   Assert (sets [i].existe, "set no inicializado");

   RETURN sets [i].pset
END LeerSetExistente;

PROCEDURE LeerSetInexistenteYAsignar (pset: PSet);
VAR i: CARDINAL;
BEGIN
   i := LeerIndiceValidoSet ();

   IF sets [i].existe THEN
      DestruirSet (sets [i].pset^)
   END;

   sets [i].existe := TRUE;
   sets [i].pset := pset
END LeerSetInexistenteYAsignar;

PROCEDURE MemoriaLista ();
CONST
   MAX_ITER = 400000;
VAR
   i:     CARDINAL;
   lista: ListaString;
BEGIN
   FOR i := 1 TO MAX_ITER DO
      lista := CrearLista ();
      InsertarEnLista ("q", lista);
      InsertarEnLista ("q", lista);
      DestruirLista (lista)
   END;
   WriteString ("Fin Destruir. ");
   lista := CrearLista ();
   InsertarEnLista ("q", lista);
   InsertarEnLista ("q", lista);
   FOR i := 1 TO MAX_ITER DO
      IrInicioLista (lista);
      RemoverDeLista (lista);
      InsertarEnLista ("w", lista)
   END;
   DestruirLista (lista);
   WriteString ("Fin Remover. ");
   WriteLn
END MemoriaLista;

PROCEDURE MemoriaBinario ();
CONST
   MAX_ITER = 400000;
   MAX_BAL  = 40;
VAR
   i:       CARDINAL;
   binario: Binario;
   info:    TInfo;
   lista,
   lista2:  ListaString;
   texto:   TString;
BEGIN
   FOR i := 1 TO MAX_ITER DO
      binario := CrearHoja (CrearInfo (0, "q"));
      InsertarEnBinario (CrearInfo (0, "a"), binario);
      InsertarEnBinario (CrearInfo (0, "w"), binario);
      info := CrearInfo (0, "q");
      InsertarEnBinario (info, binario);
      DestruirInfo (info);
      DestruirBinario (binario)
   END;
   WriteString ("Fin Destruir. ");
   binario := CrearHoja (CrearInfo (0, "q"));
   InsertarEnBinario (CrearInfo (0, "a"), binario);
   InsertarEnBinario (CrearInfo (0, "w"), binario);
   FOR i := 1 TO MAX_ITER DO
      InsertarEnBinario (CrearInfo (0, "b"), binario);
      InsertarEnBinario (CrearInfo (0, "c"), binario);
      RemoverDeBinario ("b", binario);
      RemoverDeBinario ("c", binario)
   END;
   DestruirBinario (binario);
   WriteString ("Fin Remover. ");

   lista := CrearLista ();
   FOR i := 10000 TO 20000 DO
      CardToStr (i, texto);
      InsertarEnLista (texto, lista)
   END;
   FOR i := 1 TO MAX_BAL DO
      lista2 := CopiaLista (lista);
      binario := Balanceado (lista2);
      DestruirBinario (binario)
   END;
   DestruirLista (lista);
   WriteString ("Fin Balanceado. ");

   WriteLn
END MemoriaBinario;

PROCEDURE MemoriaPila ();
CONST
   MAX_DES = 400000;
   MAX_EXT = 400000;
VAR
   i:    CARDINAL;
   info: TInfo;
   pila: Pila;
BEGIN
   FOR i := 1 TO MAX_DES DO
      pila := CrearPila ();
      info := CrearInfo (1, 'a');
      Apilar (info, pila);
      DestruirPila (pila);
   END; 
   WriteString ("Fin Destruir. ");
   info := CrearInfo (1, 'a');
   pila := CrearPila ();
   FOR i := 1 TO MAX_EXT DO
      Apilar (info, pila);
      info := Tope(pila);
      Desapilar (pila);
   END;
   DestruirPila(pila); 
   WriteString ("Fin Desapilar. "); 
   WriteLn 
END MemoriaPila;

PROCEDURE MemoriaColaBinario ();
CONST
   MAX_DES = 400000;
   MAX_EXT = 400000;
VAR
   i:    CARDINAL;
   cb:ColaBinario;
   binario:Binario;
BEGIN   
   FOR i := 1 TO MAX_DES DO
      cb := CrearColaBinario ();
      binario := CrearHoja (CrearInfo (0, "a"));
      InsertarEnBinario (CrearInfo (0, "b"), binario);
      EncolarBinario (binario,cb);
      DestruirColaBinario (cb);
   END; 
   WriteString ("Fin Destruir ColaBinario. ");
   cb := CrearColaBinario ();
   binario := CrearHoja (CrearInfo (0, "a"));
   InsertarEnBinario (CrearInfo (0, "b"), binario);
   FOR i := 1 TO MAX_EXT DO
      EncolarBinario (binario,cb);     
      binario:=PrimeroColaBinario(cb);
      DesencolarBinario(cb);
   END;
   DestruirColaBinario (cb);
   WriteString ("Fin DesencolarBinario. "); 
   WriteLn;
END MemoriaColaBinario;


PROCEDURE MemoriaSet ();
CONST
   MAX_DES = 400000;
   MAX_RMV = 40000;
   MAX_OP  = 40000;
VAR
   A,
   B,
   diferencia,
   interseccion,
   res,
   union:        Set;
   i:            CARDINAL;
BEGIN
   FOR i := 1 TO MAX_DES DO
      A := CrearSet ();
      InsertarEnSet ("q", A);
      DestruirSet (A)
   END;
   WriteString ("Fin Destruir. ");

   A := CrearSet ();
   FOR i := 1 TO MAX_RMV DO
      InsertarEnSet ("q", A);
      InsertarEnSet ("w", A);
      RemoverDeSet ("q", A);
      RemoverDeSet ("w", A)
   END;
   DestruirSet (A);
   WriteString ("Fin Remover. ");

   A := CrearSet ();
   InsertarEnSet ("q", A);
   InsertarEnSet ("w", A);
   InsertarEnSet ("e", A);
   InsertarEnSet ("r", A);
   B := CrearSet ();
   InsertarEnSet ("e", B);
   InsertarEnSet ("r", B);
   InsertarEnSet ("t", B);
   InsertarEnSet ("y", B);

   FOR i := 1 TO MAX_OP DO
      union := Union (A, B);
      DestruirSet (union);
      interseccion := Interseccion (A, B);
      diferencia := Diferencia (A, B);
      res := Interseccion (interseccion, diferencia);
      DestruirSet (res);
      res := Diferencia (interseccion, A);
      DestruirSet (res);
      DestruirSet (interseccion);
      DestruirSet (diferencia)
   END;
   DestruirSet (A);
   DestruirSet (B);
   WriteString ("Fin Operaciones. ");

   WriteLn;
END MemoriaSet;

PROCEDURE MemoriaColaPrioridad ();
CONST
   MAX_DES = 40000;
   MAX_EXT = 400000;
VAR
   cp: ColaPrioridad;
   i:  CARDINAL;
BEGIN
   FOR i := 1 TO MAX_DES DO
      cp := CrearColaPrioridad ();
      DestruirColaPrioridad (cp)
   END;
   WriteString ("Fin Destruir. ");
   cp := CrearColaPrioridad ();
   InsertarEnColaPrioridad ("q", 1, cp);
   FOR i := 1 TO MAX_EXT DO
      InsertarEnColaPrioridad ("q", 1, cp);
      ExtraerDeMinimoColaPrioridad (cp)
   END;
   DestruirColaPrioridad (cp);
   WriteString ("Fin Extraer. ");
   WriteLn
END MemoriaColaPrioridad;

PROCEDURE MemoriaProcesos ();
CONST
   MAX_DES = 40000;
   MAX_ELI = 100000;
   MAX_IMP = 400000;
VAR
   proc: Procesos;
   i:    CARDINAL;
BEGIN
   FOR i := 1 TO MAX_DES DO
      proc := CrearProcesos ("q", 1);
      DestruirProcesos (proc)
   END;
   FOR i := 1 TO MAX_DES DO
      proc := CrearProcesos ("q", 1);
      EliminarProceso ("q", proc)
   END;
   WriteString ("Fin Destruir. ");

   proc := CrearProcesos ("q", 1);   
   FOR i := 1 TO MAX_ELI DO
      AgregarProceso ("w", 1, proc);
      EliminarProceso ("w", proc)
   END;    
   DestruirProcesos (proc);
   WriteString ("Fin Eliminar. ");

   proc := CrearProcesos ("q", 1);   
   FOR i := 1 TO MAX_IMP DO
      ImprimirProcesos (proc)
   END;
   DestruirProcesos (proc);
   WriteString ("Fin Imprimir. ");
   
   WriteLn
END MemoriaProcesos;

PROCEDURE MemoriaSolicitudes ();
CONST
   MAX_DES = 40000;
   MAX_LIS = 80000;
   MAX_OP  = 40000;
VAR
   i:        CARDINAL;
   lista:    ListaString;
   set:      Set;
   sol:      Solicitudes;
BEGIN
   FOR i:= 1 TO MAX_DES DO
      sol := CrearSolicitudes ();
      DestruirSolicitudes (sol)
   END;
   WriteString ("Fin Destruir.");

   sol := CrearSolicitudes ();
   FOR i:= 1 TO MAX_LIS DO
      IngresarSolicitud (1, "q", sol);
      lista := ListarSolicitudes (sol);
      DestruirLista (lista)
   END;
   DestruirSolicitudes (sol);
   WriteString ("Fin Listar.");
   
   sol := CrearSolicitudes ();
   IngresarSolicitud (1, "q", sol);
   IngresarSolicitud (2, "w", sol);
   FOR i:= 1 TO MAX_OP DO
      set := EnAlgunoSolicitudes (1, 2, sol);
      DestruirSet (set)
   END;
   DestruirSolicitudes (sol);
   WriteString ("Fin Operaciones.");
   
   WriteLn
END MemoriaSolicitudes;

PROCEDURE MemoriaSistema ();
CONST
   MAX_APG = 400000;
   MAX_SOL = 400000;
VAR
   i:        CARDINAL;
   lista:    ListaString;
   sist:     Sistema;
BEGIN   
   FOR i:= 1 TO MAX_APG DO
      sist := BootearSistema (100);
      ApagarSistema (sist)
   END;
   WriteString ("Fin Apagar.");

   sist := BootearSistema (100);
   CorrerProceso ("q", 10, sist);
   CorrerProceso ("w", 10, sist);
   PedirRecurso ("q", 5, sist);
   PedirRecurso ("w", 5, sist);
   FOR i:= 1 TO MAX_SOL DO
      lista := SolicitanOMuyConsumidores (100, 1, sist);
      DestruirLista (lista)
   END;
   ApagarSistema (sist);
   WriteString ("Fin Solicitan.");
   
   WriteLn
END MemoriaSistema;


PROCEDURE TiempoLista ();
CONST
   MAX_INS  = 30000;
   MAX_CANT = 30000;
   MAX_RMV  = 30000;
VAR
   cant,
   i:     CARDINAL;
   lista: ListaString;
BEGIN
   TextIO.WriteString (StdErrChan (), "Test de tiempo de ListaString ...");
   TextIO.WriteLn (StdErrChan ());
   lista := CrearLista ();
   FOR i := 1 TO MAX_INS DO
      InsertarEnLista ("a", lista)
   END;
   WriteString ("Fin Insertar. ");

   FOR i := 1 TO MAX_CANT DO
      cant := CantidadLista (lista)
   END;
   WriteString ("Fin Cantidad. ");

   IrInicioLista (lista);
   FOR i := 1 TO MAX_INS - 2 DO
       IrSiguienteLista (lista)
   END;
   FOR i := 1 TO MAX_RMV DO
      RemoverDeLista (lista);
      InsertarEnLista ("b", lista)
   END;
   DestruirLista (lista);
   WriteString ("Fin Remover. ");

   WriteLn;
   TextIO.WriteString (StdErrChan (), "... Compruebe que el test haya durado aproximadamente 1 segundo.");
   TextIO.WriteLn (StdErrChan ());
END TiempoLista;

PROCEDURE TiempoBinario ();
CONST
   MAX_COPIA = 100;
   MAX_BAL   = 100;
   MAX_LIN   = 100;
   MAX_NIVEL = 13;
VAR
   binario,
   copia:  Binario;
   i:      CARDINAL;
   lista,
   lista2: ListaString;
   texto:  TString;
BEGIN
   TextIO.WriteString (StdErrChan (), "Test de tiempo de Binario ...");
   TextIO.WriteLn (StdErrChan ());
   binario := CrearHoja (CrearInfo (20000, "20000"));
   lista := CrearLista ();
   FOR i := 20001 TO 21000 DO
      CardToStr (i, texto);
      InsertarEnBinario (CrearInfo (i, texto), binario)
   END;
   FOR i := 19999 TO 19000 BY -1 DO
      CardToStr (i, texto);
      InsertarEnBinario (CrearInfo (i, texto), binario)
   END;

   FOR i := 1 TO MAX_COPIA DO
      copia := CopiaBinario (binario);
      DestruirBinario (copia)
   END;
   WriteString ("Fin Copia. ");

   FOR i := 1 TO MAX_LIN DO
      lista := Linealizacion (binario);
      DestruirLista (lista)
   END;
   WriteString ("Fin Linealizacion. ");

   lista := Linealizacion (binario);
   FOR i := 1 TO MAX_BAL DO
      lista2 := CopiaLista (lista);
      binario := Balanceado (lista2);
      DestruirBinario (binario)
   END;
   DestruirLista (lista);
   WriteString ("Fin Balanceado. ");

   WriteLn;
   TextIO.WriteString (StdErrChan (), "... Compruebe que el test haya durado aproximadamente 1 segundo.");
   TextIO.WriteLn (StdErrChan ());
END TiempoBinario;

PROCEDURE TiempoPila ();
CONST
   MAX_ENP = 100000;
VAR
   i:    CARDINAL;
   info: TInfo;
   p:    Pila;
BEGIN
   TextIO.WriteString (StdErrChan (), "Test de tiempo de Pila ...");
   TextIO.WriteLn (StdErrChan ());
   
   info := CrearInfo (0, "a");
   p := CrearPila();
   FOR i := 1 TO MAX_ENP DO
      Apilar (info, p);
   END;
   FOR i := 1 TO MAX_ENP DO
      Desapilar (p);
   END;
   DestruirInfo (info);
   DestruirPila (p);
   WriteString ("Fin Apilar. ");
   
   WriteLn;
   TextIO.WriteString (StdErrChan (), "... Compruebe que el test haya durado aproximadamente 1 segundo.");
   TextIO.WriteLn (StdErrChan ());
END TiempoPila;

PROCEDURE TiempoColaBinario ();
CONST
   MAX_ENC = 100000;
VAR
   b: Binario;
   c: ColaBinario;
   i: CARDINAL;
BEGIN
   TextIO.WriteString (StdErrChan (), "Test de tiempo de ColaBinario ...");
   TextIO.WriteLn (StdErrChan ());
   
   b := CrearHoja (CrearInfo (0, "a"));
   c := CrearColaBinario ();
   FOR i := 1 TO MAX_ENC DO
      EncolarBinario (b, c)
   END;
   FOR i := 1 TO MAX_ENC DO
      DesencolarBinario (c)
   END;
   DestruirBinario (b);
   DestruirColaBinario (c);
   WriteString ("Fin Encolar. ");
   
   WriteLn;
   TextIO.WriteString (StdErrChan (), "... Compruebe que el test haya durado aproximadamente 1 segundo.");
   TextIO.WriteLn (StdErrChan ());
END TiempoColaBinario;


PROCEDURE TiempoSet ();
CONST
   BASE     = 20000;
   MAX      = 1000;
VAR
   A,
   B,
   diferencia:   Set;
   i:            INTEGER;
   interseccion,
   union:        Set;
   pertenece:    BOOLEAN;
   texto:        TString;
BEGIN
   TextIO.WriteString (StdErrChan (), "Test de tiempo de Set ...");
   TextIO.WriteLn (StdErrChan ());
   A := CrearSet ();
   B := CrearSet ();

   FOR i := 1 TO MAX DO
      CardToStr (BASE + i, texto);
      InsertarEnSet (texto, A)
   END;
   diferencia := Diferencia (A, B);

   FOR i := 1 TO MAX DO
      CardToStr (BASE + i, texto);
      InsertarEnSet (texto, B)
   END;
   FOR i := 0 TO - MAX BY -1 DO
      CardToStr (BASE - i, texto);
      InsertarEnSet (texto, B)
   END;
   union := Union (A, B);
   interseccion := Interseccion (A, B);

   FOR i := - 2 * MAX TO 2 * MAX DO
      CardToStr (BASE + i, texto);
      pertenece := PerteneceASet (texto, union);
      pertenece := PerteneceASet (texto, interseccion);
      pertenece := PerteneceASet (texto, diferencia)
   END;
   DestruirSet (A);
   DestruirSet (B);
   DestruirSet (union);
   DestruirSet (interseccion);
   DestruirSet (diferencia);
   WriteString ("Fin operaciones. ");

   WriteLn;
   TextIO.WriteString (StdErrChan (), "... Compruebe que el test haya durado aproximadamente 1 segundo.");
   TextIO.WriteLn (StdErrChan ());
END TiempoSet;

PROCEDURE TiempoColaPrioridad ();
CONST
   MAX_INS        = 50;
   MAX_ITER       = 500000;
   MAX_ITER_INS   = 1000000;
VAR
   cp:      ColaPrioridad;
   i,
   j:       CARDINAL;
   lista:   ListaString;
   texto:   TString;
   prio:    CARDINAL;
BEGIN
   TextIO.WriteString (StdErrChan (), "Test de tiempo de ColaPrioridad ...");
   TextIO.WriteLn (StdErrChan ());
   cp := CrearColaPrioridad ();
   InsertarEnColaPrioridad ("a", K, cp);
   FOR i := 1 TO MAX_ITER DO
      IF NOT EsVaciaColaPrioridad (cp) THEN
         texto := MinimoColaPrioridad (cp);
         prio := PrioridadMinimoColaPrioridad (cp)
      END
   END;
   WriteString ("Fin EsVacia, Minimo, Prioridad. ");

   FOR i := 1 TO MAX_ITER DO
      ExtraerDeMinimoColaPrioridad (cp);
      InsertarEnColaPrioridad ("a", K, cp)
   END;
   WriteString ("Fin Extraer. ");

   prio := K DIV 2;
   FOR i := 1 TO MAX_INS DO
      InsertarEnColaPrioridad ("a", prio, cp)
   END;
   FOR i := 1 TO MAX_ITER DO
      lista := ElementosColaPrioridad (prio, cp);
   END;
   FOR i := 1 TO MAX_INS DO
      ExtraerDeMinimoColaPrioridad (cp)
   END;
   WriteString ("Fin Elementos. ");

   FOR j := 2 TO K DO
      InsertarEnColaPrioridad ("a", j, cp)
   END;
   FOR i := 1 TO MAX_ITER_INS DO
      InsertarEnColaPrioridad ("a", 1, cp);
      ExtraerDeMinimoColaPrioridad (cp)
   END;
   FOR j := 2 TO K DO
      ExtraerDeMinimoColaPrioridad (cp)
   END;
   DestruirColaPrioridad (cp);
   WriteString ("Fin Insertar. ");

   WriteLn;
   TextIO.WriteString (StdErrChan (), "... Compruebe que el test haya durado aproximadamente 1 segundo.");
   TextIO.WriteLn (StdErrChan ());
END TiempoColaPrioridad;

PROCEDURE TiempoSolicitudes ();
CONST
   MAX_SOL = 100;
VAR
   i, 
   j,
   k:     CARDINAL;
   lista: ListaString;
   sol:   Solicitudes; 
BEGIN
   TextIO.WriteString (StdErrChan (), "Test de tiempo de Solicitudes ...");
   TextIO.WriteLn (StdErrChan ());
   sol := CrearSolicitudes ();
   FOR k := 1 TO MAX_SOL DO
      FOR j := MAX_RECURSOS TO (MAX_RECURSOS DIV 2) + 1 BY - 1 DO
         FOR i := 1 TO MAX_SOL DO
            IngresarSolicitud (j, "qwe", sol)
         END
      END;
      FOR j := 1 TO MAX_RECURSOS DIV 2 DO
         FOR i := 1 TO 10 DO
            IngresarSolicitud (j, "qwe", sol)
         END
      END;
      lista := ListarSolicitudes (sol);
      DestruirLista (lista)
   END;
   DestruirSolicitudes (sol);
   WriteString ("Fin de la prueba. ");
   
   WriteLn;
   TextIO.WriteString (StdErrChan (), "... Compruebe que el test haya durado aproximadamente 1 segundo.");
   TextIO.WriteLn (StdErrChan ());
END TiempoSolicitudes;





PROCEDURE IniciarEstructuras ();
VAR i: CARDINAL;
BEGIN
   FOR i := 1 TO CANT_LISTAS DO
      listas [i].existe := FALSE
   END;

   FOR i := 1 TO CANT_BINARIOS DO
      binarios [i].existe := FALSE
   END;

   FOR i := 1 TO CANT_SETS DO
      sets [i].existe := FALSE
   END;

   pila := CrearPila ();
   colaBinarios := CrearColaBinario ();
   colaPrioridad := CrearColaPrioridad ();
   procesos := CrearProcesos ("boot", 100);
   solicitudes := CrearSolicitudes ();
   sistema := BootearSistema (100)
END IniciarEstructuras;

PROCEDURE DestruirEstructuras ();
VAR i: CARDINAL;
BEGIN
   FOR i := 1 TO CANT_LISTAS DO
      IF listas [i].existe THEN
         DestruirLista (listas [i].plista^);
         DISPOSE (listas [i].plista)
      END
   END;

   FOR i := 1 TO CANT_BINARIOS DO
      IF binarios [i].existe THEN
         DestruirBinario (binarios [i].pbinario^);
         DISPOSE (binarios [i].pbinario)
      END
   END;

   FOR i := 1 TO CANT_SETS DO
      IF sets [i].existe THEN
         DestruirSet (sets [i].pset^);
         DISPOSE (sets [i].pset)
      END
   END;

   DestruirPila (pila);
   DestruirColaBinario (colaBinarios);
   DestruirColaPrioridad (colaPrioridad);
   DestruirProcesos (procesos);
   DestruirSolicitudes (solicitudes);
   ApagarSistema (sistema)
END DestruirEstructuras;





BEGIN
   contCmds := 0;
   esFinal := FALSE;
   IniciarEstructuras ();
   REPEAT
      INC (contCmds);
      WriteCard (contCmds, 1); WriteString (PROMPT);

      nomCmd := LeerTexto ();

      IF Equal (CMD_FIN, nomCmd) THEN
         esFinal := TRUE;
         WriteString ("Fin");
         WriteLn
      ELSIF Equal (CMD_COMENTARIO, nomCmd) THEN
         ReadRestLine (texto);
         readResult := ReadResult ();
         Assert (readResult = allRight,
                 "no se pudo leer el comando o el parametro");
         WriteString (CMD_COMENTARIO);
         WriteString (texto);
         WriteLn
      ELSIF Equal (CMD_REINICIO, nomCmd) THEN
         WriteString ("Reiniciando");
         WriteLn;
         DestruirEstructuras ();
         IniciarEstructuras ()
(********************************** ListaString ***************************)
      ELSIF Equal (CMD_CREAR_LISTA, nomCmd) THEN
         NEW (plista);
         plista^ := CrearLista ();
         LeerListaInexistenteYAsignar (plista);
         texto := LeerTexto ();
         WHILE NOT Equal ("FIN", texto) DO
            InsertarEnLista (texto, plista^);
            texto := LeerTexto ()
         END;
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_COPIA_LISTA, nomCmd) THEN
         plista1 := LeerListaExistente ();
         NEW (plista2);
         plista2^ := CopiaLista (plista1^);
         LeerListaInexistenteYAsignar (plista2);
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_PARTIR_LISTA, nomCmd) THEN
         plista1 := LeerListaExistente ();
         IF CantidadLista (plista1^) > 1 THEN
            NEW (plista2);
            plista2^ := PartirLista (plista1^);
            LeerListaInexistenteYAsignar (plista2);
            WriteString ("OK")
         ELSE
            WriteString ("La lista tiene menos de 2 elementos.")
         END;
         WriteLn
     ELSIF Equal (CMD_REMOVER_LISTA, nomCmd) THEN
         plista := LeerListaExistente ();
         cantidad := LeerCardinal () - 1;
         IF NOT EsVaciaLista (plista^) THEN
            IrInicioLista (plista^);
            WHILE EsPosicionValida (plista^) AND (cantidad > 0) DO
               IrSiguienteLista (plista^);
               DEC (cantidad)
            END;
            IF EsPosicionValida (plista^) THEN
               WriteString (ActualLista (plista^));
               RemoverDeLista (plista^)
            ELSE
               WriteString ("La lista tiene solo ");
               WriteCard (CantidadLista (plista^), 1);
               WriteString (" elementos.")
            END;
         ELSE
            WriteString ("La lista es vacia.")
         END;
         WriteLn
      ELSIF Equal (CMD_IMPRIMIR_LISTA, nomCmd) THEN
         plista := LeerListaExistente ();
         IF EstaOrdenadaLista (plista^) THEN
            WriteString ("Ordenada. ")
         ELSE
            WriteString ("NO ordenada. ")
         END;
         IF EsVaciaLista (plista^) THEN
            WriteString ("Lista vacia. ")
         ELSE
            ImprimirLista (plista^)
         END;
         WriteLn
(********************************* Binario ********************************)
      ELSIF Equal (CMD_CREAR_BINARIO, nomCmd) THEN
         LeerInfoRepeat (info, pararDeLeer);
         Assert (NOT pararDeLeer, "el binario debe tener al menos un nodo");
         NEW (pbinario);
         pbinario^ := CrearHoja (info);
         Assert (EsHoja (pbinario^),
            "el binario solo con un elemento deberia dar TRUE en EsHoja");
         LeerBinarioInexistenteYAsignar (pbinario);
         REPEAT
            LeerInfoRepeat (info, pararDeLeer);
            IF NOT pararDeLeer THEN
               InsertarEnBinario (info, pbinario^);
               Assert (NOT EsHoja (pbinario^),
                  "el arbol con mas de 1 elemento deberia dar FALSE en EsHoja")
            END
         UNTIL pararDeLeer;
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_IZQUIERDO, nomCmd) THEN
         pbinario1 := LeerBinarioExistente ();
         IF TieneHijoIzquierdo (pbinario1^) THEN
            NEW (pbinario2);
            pbinario2^ := CopiaBinario (Izquierdo (pbinario1^));
            LeerBinarioInexistenteYAsignar (pbinario2);
            WriteString ("OK")
         ELSE
            WriteString ("No tiene hijo izquierdo.")
         END;
         WriteLn
      ELSIF Equal (CMD_DERECHO, nomCmd) THEN
         pbinario1 := LeerBinarioExistente ();
         IF TieneHijoDerecho (pbinario1^) THEN
            NEW (pbinario2);
            pbinario2^ := CopiaBinario (Derecho (pbinario1^));
            LeerBinarioInexistenteYAsignar (pbinario2);
            WriteString ("OK");
         ELSE
            WriteString ("No tiene hijo derecho.")
         END;
         WriteLn
      ELSIF Equal (CMD_BALANCEADO, nomCmd) THEN
         plista := LeerListaExistente ();
         IF (NOT EsVaciaLista (plista^)) AND EstaOrdenadaLista (plista^) THEN
            NEW (pbinario);
            lista := CopiaLista (plista^);
            pbinario^ := Balanceado (lista);
            LeerBinarioInexistenteYAsignar (pbinario);
            WriteString ("OK")
         ELSE
            WriteString ("La lista no puede ser vacia y debe estar ordenada.")
         END;
         WriteLn
      ELSIF Equal (CMD_LINEALIZAR, nomCmd) THEN
         pbinario1 := LeerBinarioExistente ();
         NEW (plista);
         plista^ := Linealizacion (pbinario1^);
         LeerListaInexistenteYAsignar (plista);
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_ALTURA_CANTIDAD_BINARIO, nomCmd) THEN
         pbinario := LeerBinarioExistente ();
         WriteString ("Altura: ");
         WriteCard (AlturaBinario (pbinario^), 1);
         WriteString ("; Cantidad: " );
         WriteCard (CantidadBinario (pbinario^), 1);
         WriteLn
      ELSIF Equal (CMD_IMPRIMIR_BINARIO, nomCmd) THEN
         pbinario := LeerBinarioExistente ();
         WriteLn;
         ImprimirBinario (pbinario^)
      ELSIF Equal (CMD_REMOVER_BINARIO, nomCmd) THEN
         pbinario := LeerBinarioExistente ();
         texto := LeerTexto ();
         IF (CantidadBinario (pbinario^) <> 1)
             OR (NOT Equal (texto, TextoInfo (RaizBinario (pbinario^)))) THEN
            RemoverDeBinario (texto, pbinario^);
            WriteString ("OK")
         ELSE
            WriteString ("No se puede eliminar remover el unico elemento.")
         END;
         WriteLn
      ELSIF Equal (CMD_FILTRAR, nomCmd) THEN
         pbinario1 := LeerBinarioExistente ();
         cardinal := LeerCardinal ();
         critFiltro := LeerCriterioFiltro ();
         boolBinario := Filtrar (cardinal, critFiltro, pbinario1^);
         IF boolBinario.hayBinario THEN
            NEW (pbinario2);
            pbinario2^ := boolBinario.arbol;
            LeerBinarioInexistenteYAsignar (pbinario2);
            WriteString ("OK");
            WriteLn
         ELSE
            WriteString ("El filtro devolvio un arbol sin elementos.");
            WriteLn
         END
      ELSIF Equal (CMD_BUSCAR_ABB, nomCmd) THEN
         pbinario := LeerBinarioExistente ();
         texto := LeerTexto ();
         boolBinario := BuscarABB (texto, pbinario^);
         IF boolBinario.hayBinario THEN
            WriteLn;
            ImprimirBinario (boolBinario.arbol)
         ELSE
            WriteString ("No se encontro el texto.");
            WriteLn
         END
(********************************** Pila **********************************)
      ELSIF Equal (CMD_APILAR, nomCmd) THEN
         info := LeerInfo ();
         Apilar (info, pila);
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_DESAPILAR, nomCmd) THEN
         IF EsVaciaPila (pila) THEN
            WriteString ("La pila estaba vacia. ")
         ELSE
            info := Tope (pila);
            Desapilar (pila);
            (* info2 en probablmente mismo lugar de memoria si fue eliminado *)
            info2 := CrearInfo (0, "nuevo");
            WriteString (InfoAString (info));
            DestruirInfo (info);
            DestruirInfo (info2)
         END;
         WriteLn
(********************************** ColaBinario ***************************)
      ELSIF Equal (CMD_ENCOLAR, nomCmd) THEN
         pbinario := LeerBinarioExistente ();
         EncolarBinario (CopiaBinario (pbinario^), colaBinarios);
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_DESENCOLAR, nomCmd) THEN
         IF EsVaciaColaBinario (colaBinarios) THEN
            WriteString ("La cola estaba vacia.")
         ELSE
            WriteLn;
            NEW (pbinario1);
            pbinario1^ := PrimeroColaBinario (colaBinarios);
            DesencolarBinario (colaBinarios);
            (* pbinario2 similar a info2 en CMD_DESAPILAR *)
            NEW (pbinario2);
            pbinario2^ := CrearHoja (CrearInfo (0, "nuevo"));
            ImprimirBinario (pbinario1^);
            DestruirBinario (pbinario1^);
            DestruirBinario (pbinario2^)
         END;
         WriteLn
(********************************** Set ***********************************)
      ELSIF Equal (CMD_CONS_SET, nomCmd) THEN
         plista := LeerListaExistente ();
         NEW (pset);
         pset^ := ConstruirSet (plista^);
         LeerSetInexistenteYAsignar (pset);
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_COPIAR_SET, nomCmd) THEN
         pset1 := LeerSetExistente ();
         NEW (pset2);
         pset2^ := CopiaSet (pset1^);
         LeerSetInexistenteYAsignar (pset2);
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_UN_INT_DIF, nomCmd) THEN
         pset1 := LeerSetExistente ();
         pset2 := LeerSetExistente ();
         NEW (pset);
         pset^ := Union (pset1^, pset2^);
         LeerSetInexistenteYAsignar (pset);
         NEW (pset);
         pset^ := Interseccion (pset1^, pset2^);
         LeerSetInexistenteYAsignar (pset);
         NEW (pset);
         pset^ := Diferencia (pset1^, pset2^);
         LeerSetInexistenteYAsignar (pset);
         NEW (pset);
         pset^ := Diferencia (pset2^, pset1^);
         LeerSetInexistenteYAsignar (pset);
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_INSERTAR_SET, nomCmd) THEN
         pset := LeerSetExistente ();
         IF EsVacioSet (pset^) THEN
            WriteString ("El conjunto estaba vacio. ")
         END;
         plista := LeerListaExistente ();
         IF NOT EsVaciaLista (plista^) THEN
            IrInicioLista (plista^);
            WHILE EsPosicionValida (plista^) DO
               InsertarEnSet (ActualLista (plista^), pset^);
               IrSiguienteLista (plista^)
            END
         END;
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_REMOVER_SET, nomCmd) THEN
         pset := LeerSetExistente ();
         plista := LeerListaExistente ();
         IF NOT EsVaciaLista (plista^) THEN
            IrInicioLista (plista^);
            WHILE EsPosicionValida (plista^) DO
               RemoverDeSet (ActualLista (plista^), pset^);
               IrSiguienteLista (plista^)
            END
         END;
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_PERTENCE_SET, nomCmd) THEN
         pset := LeerSetExistente ();
         plista := LeerListaExistente ();
         IF EsVaciaLista (plista^) THEN
            WriteString ("La lista esta vacia.")
         ELSE
            IrInicioLista (plista^);
            WHILE EsPosicionValida (plista^) DO
               IF PerteneceASet (ActualLista (plista^), pset^) THEN
                  WriteString ("SI ")
               ELSE
                  WriteString ("NO ")
               END;
               IrSiguienteLista (plista^)
            END
         END;
         WriteLn
(********************************** ColaPrioridad *************************)
      ELSIF Equal (CMD_INSERTAR_CP, nomCmd) THEN
         REPEAT
            LeerInfoRepeat (info, pararDeLeer);
            IF NOT pararDeLeer THEN
               InsertarEnColaPrioridad (TextoInfo (info), NumeroInfo (info),
                                        colaPrioridad)
            END
         UNTIL pararDeLeer;
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_EXTRAER_CP, nomCmd) THEN
         IF EsVaciaColaPrioridad (colaPrioridad) THEN
            WriteString ("La cola de prioridad estaba vacia. ")
         ELSE
            WriteString (MinimoColaPrioridad (colaPrioridad));
            WriteString (" - ");
            WriteCard (PrioridadMinimoColaPrioridad (colaPrioridad), 1);
            ExtraerDeMinimoColaPrioridad (colaPrioridad)
         END;
         WriteLn
      ELSIF Equal (CMD_ELEMENTOS_CP, nomCmd) THEN
         cardinal := LeerCardinal ();
         IF PerteneceAColaPrioridad (cardinal, colaPrioridad) THEN
            NEW (plista);
            plista^ := CopiaLista (ElementosColaPrioridad (cardinal,
                                   colaPrioridad));
            LeerListaInexistenteYAsignar (plista);
            WriteString ("OK")
         ELSE
            WriteString ("No hay elementos con esa prioridad.")
         END;
         WriteLn;
(********************************** Procesos *********************************)
      ELSIF Equal (CMD_AGREGAR_PROCESOS, nomCmd) THEN
         REPEAT
            LeerInfoRepeat (info, pararDeLeer);
            IF NOT pararDeLeer THEN
               AgregarProceso (TextoInfo (info), NumeroInfo (info), procesos); 
               DestruirInfo (info)
            END;
         UNTIL pararDeLeer;
         WriteString ("Hay ");
         WriteCard (CantidadProcesos (procesos), 1);
         WriteString (" procesos.");
         WriteLn
         
      ELSIF Equal (CMD_ELIMINAR_PROCESOS, nomCmd) THEN
         texto := LeerTexto ();
         IF NOT IncluyeProceso (texto, procesos) THEN
            boolean := FALSE;
            WriteString ("No existe el proceso.")
         ELSE
            boolean := TRUE;
            WriteString ("La memoria que se libera es ");
            WriteCard (ValorProceso (texto, procesos), 1);
            WriteString (".");
         END;
         cantidad := CantidadProcesos (procesos);
         EliminarProceso (texto, procesos);
         IF (cantidad = 1) AND boolean THEN
            procesos := CrearProcesos ("boot", 100)
         END;   
         WriteLn
      ELSIF Equal (CMD_MUY_CONSUMIDORES, nomCmd) THEN
         cardinal := LeerCardinal ();
         lista := MuyConsumidores (cardinal, procesos);
         ImprimirLista (lista);
         WriteLn;
         DestruirLista (lista)
      ELSIF Equal (CMD_LISTAR_PROCESOS, nomCmd) THEN
         lista := ListarProcesos (procesos);
         ImprimirLista (lista);
         WriteLn;
         DestruirLista (lista)
      ELSIF Equal (CMD_IMPRIMIR_PROCESOS, nomCmd) THEN
         WriteLn;
         ImprimirProcesos (procesos)

(********************************** Solicitudes ******************************)
      ELSIF Equal (CMD_INGRESAR_SOLICITUDES, nomCmd) THEN
         REPEAT
            LeerInfoRepeat (info, pararDeLeer);
            IF NOT pararDeLeer THEN
               IngresarSolicitud (NumeroInfo (info), TextoInfo (info),  
                                  solicitudes); 
               DestruirInfo (info)
            END;
         UNTIL pararDeLeer;
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_CANCELAR_SOLICITUDES, nomCmd) THEN
         WriteCard (MenorRecurso (solicitudes), 0);
         CancelarSolicitud (solicitudes);
         WriteLn          
      ELSIF Equal (CMD_EN_ALGUNO_SOLICITUDES, nomCmd) THEN
         rec1 := LeerCardinal ();
         rec2 := LeerCardinal ();
         NEW (pset);
         pset^ := EnAlgunoSolicitudes (rec1, rec2, solicitudes);
         LeerSetInexistenteYAsignar (pset);
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_EN_AMBOS_SOLICITUDES, nomCmd) THEN
         rec1 := LeerCardinal ();
         rec2 := LeerCardinal ();
         NEW (pset);
         pset^ := EnAmbosSolicitudes (rec1, rec2, solicitudes);
         LeerSetInexistenteYAsignar (pset);
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_SOLO_EN_UNO_SOLICITUDES, nomCmd) THEN
         rec1 := LeerCardinal ();
         rec2 := LeerCardinal ();
         NEW (pset);
         pset^ := SoloEnUnoSolicitudes (rec1, rec2, solicitudes);
         LeerSetInexistenteYAsignar (pset);
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_LISTAR_SOLICITUDES, nomCmd) THEN
         lista := ListarSolicitudes (solicitudes);
         IF EsVaciaLista (lista) THEN
            WriteString ("No hay solicitudes")
         ELSE
            ImprimirLista (lista)
         END;
         WriteLn;
         DestruirLista (lista)         
(********************************** Sistema **********************************)
      ELSIF Equal (CMD_CORRER_PROCESOS, nomCmd) THEN
         REPEAT
            LeerInfoRepeat (info, pararDeLeer);
            IF NOT pararDeLeer THEN
               CorrerProceso (TextoInfo (info), NumeroInfo (info), sistema); 
               DestruirInfo (info)
            END;
         UNTIL pararDeLeer;
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_PEDIR_RECURSOS, nomCmd) THEN
         REPEAT
            LeerInfoRepeat (info, pararDeLeer);
            IF NOT pararDeLeer THEN
               PedirRecurso (TextoInfo (info), NumeroInfo (info),   
                                  sistema); 
               DestruirInfo (info)
            END;
         UNTIL pararDeLeer;
         WriteString ("OK");
         WriteLn
      ELSIF Equal (CMD_SOLICITAR_O_CONSUMIDORES, nomCmd) THEN
         cardinal := LeerCardinal ();
         rec1 := LeerCardinal ();
         lista := SolicitanOMuyConsumidores (cardinal, rec1, sistema);
         IF EsVaciaLista (lista) THEN
            WriteString ("Lista vacia. ");
         ELSE
		    ImprimirLista (lista);
         END;		 
         
		   
         WriteLn;
         DestruirLista (lista)
(********************************** Memoria **********************************)
      ELSIF Equal (CMD_MEMORIA_LISTA, nomCmd) THEN
         MemoriaLista ()
      ELSIF Equal (CMD_MEMORIA_BINARIO, nomCmd) THEN
         MemoriaBinario ()
      ELSIF Equal (CMD_MEMORIA_PILA, nomCmd) THEN
         MemoriaPila ();       
      ELSIF Equal (CMD_MEMORIA_COLA_BINARIO, nomCmd) THEN
         MemoriaColaBinario ();      
      ELSIF Equal (CMD_MEMORIA_SET, nomCmd) THEN
         MemoriaSet ()
      ELSIF Equal (CMD_MEMORIA_COLA_PRIORIDAD, nomCmd) THEN
         MemoriaColaPrioridad ()
      ELSIF Equal (CMD_MEMORIA_PROCESOS, nomCmd) THEN
         MemoriaProcesos ()
      ELSIF Equal (CMD_MEMORIA_SOLICITUDES, nomCmd) THEN
         MemoriaSolicitudes ()
      ELSIF Equal (CMD_MEMORIA_SISTEMA, nomCmd) THEN
         MemoriaSistema ()
       
(********************************** Tiempo ***********************************)
      ELSIF Equal (CMD_TIEMPO_LISTA, nomCmd) THEN
         TiempoLista ()
      ELSIF Equal (CMD_TIEMPO_BINARIO, nomCmd) THEN
         TiempoBinario ()
      ELSIF Equal (CMD_TIEMPO_PILA, nomCmd) THEN
         TiempoPila ()
      ELSIF Equal (CMD_TIEMPO_COLA_BINARIO, nomCmd) THEN
         TiempoColaBinario ()
      ELSIF Equal (CMD_TIEMPO_SET, nomCmd) THEN
         TiempoSet ()
      ELSIF Equal (CMD_TIEMPO_COLA_PRIORIDAD, nomCmd) THEN
         TiempoColaPrioridad ()
      ELSIF Equal (CMD_TIEMPO_SOLICITUDES, nomCmd) THEN
         TiempoSolicitudes ()
         
(*****************************************************************************)
      ELSE
         Assert (FALSE, "comando no reconocido")
      END;
      SkipLine
   UNTIL esFinal;

   DestruirEstructuras ()
END TestTarea3.
