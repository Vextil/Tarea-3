(* 4623178 *)
IMPLEMENTATION MODULE Utils;
(*******************************************************************************
Modulo de implementacion de Utils.

En Utils se definen constantes, tipos y procedimientos de uso general.

Laboratorio de Programacion 2.
InCo-FI-UDELAR
*******************************************************************************)

FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM Strings IMPORT Append;
FROM WholeStr IMPORT CardToStr;
FROM STextIO IMPORT WriteString;

TYPE TInfo = POINTER TO Info;
     Info = RECORD
               numero : CARDINAL;
               texto : TString;
            END;

(* Constructora, destructura y selectoras de TInfo. *)
PROCEDURE CrearInfo (numero: CARDINAL; texto: TString): TInfo;
(* Devuelve un TInfo tal que su dato numerico es 'numero' y su dato de texto
   es 'texto'. *)
VAR newInfo : TInfo;
BEGIN

   NEW(newInfo);
   newInfo^.numero := numero;
   newInfo^.texto := texto;
   RETURN newInfo;

END CrearInfo;

PROCEDURE CopiaInfo (i: TInfo): TInfo;
(* Devuelve una copia de 'i'. El elemento resultado no comparte memoria con 
   'i'. *) 
BEGIN
   
   RETURN CrearInfo(i^.numero, i^.texto);

END CopiaInfo;
  
PROCEDURE DestruirInfo (VAR i: TInfo);
(* Libera la memoria reservada por TInfo. *)
BEGIN

   IF i # NIL THEN
      DISPOSE(i);
   END;

END DestruirInfo;

PROCEDURE NumeroInfo (i: TInfo): CARDINAL;
(* Devuelve el dato numerico asociado a 'i'. *)
VAR numero: CARDINAL;
BEGIN

   numero := i^.numero;
   RETURN numero;

END NumeroInfo;

PROCEDURE TextoInfo (i: TInfo): TString;
(* Devuelve el dato de texto asociado a 'i'. *)
VAR texto: TString;
BEGIN

   texto := i^.texto;
   RETURN texto;

END TextoInfo;

PROCEDURE InfoAString (i: TInfo): TString;
(* Devuelve un TString formado por la concatenacion del dato numerico de 'i',
   el string "," y el dato de texto de 'i' encerrados entre parentesis.
   Ejemplos:
   (4,a)
   (56,jojo)
*)
VAR resultado, numeroStr : TString;
BEGIN
   
   CardToStr(NumeroInfo(i), numeroStr);
   resultado := "(";
   Append(numeroStr, resultado);
   Append(",", resultado);
   Append(TextoInfo(i), resultado);
   Append(")", resultado);
   RETURN resultado;

END InfoAString;

(* Operacion para facilitar la depuracion *)
PROCEDURE Assert (condicion: BOOLEAN; mensaje: ARRAY OF CHAR);
(* Si (NOT condicion) se imprime 'mensaje' y se detiene la ejecucion (HALT). *)
BEGIN
   
   IF NOT condicion THEN
      WriteString(mensaje);
      HALT;
   END;

END Assert;

END Utils.
