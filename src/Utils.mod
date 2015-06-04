(* 4623178 *)
IMPLEMENTATION MODULE Utils;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM Strings IMPORT Append;
FROM WholeStr IMPORT CardToStr;
FROM STextIO IMPORT WriteString;

TYPE TInfo = POINTER TO Info;
     Info = RECORD
               numero : CARDINAL;
               texto : TString;
            END;

PROCEDURE CrearInfo (numero: CARDINAL; texto: TString): TInfo;
VAR newInfo : TInfo;
BEGIN

   NEW(newInfo);
   newInfo^.numero := numero;
   newInfo^.texto := texto;
   RETURN newInfo;

END CrearInfo;

PROCEDURE CopiaInfo (i: TInfo): TInfo;
BEGIN
   
   RETURN CrearInfo(i^.numero, i^.texto);

END CopiaInfo;
  
PROCEDURE DestruirInfo (VAR i: TInfo);
BEGIN

   DISPOSE(i);

END DestruirInfo;

PROCEDURE NumeroInfo (i: TInfo): CARDINAL;
VAR numero: CARDINAL;
BEGIN

   numero := i^.numero;
   RETURN numero;

END NumeroInfo;

PROCEDURE TextoInfo (i: TInfo): TString;
VAR texto: TString;
BEGIN

   texto := i^.texto;
   RETURN texto;

END TextoInfo;

PROCEDURE InfoAString (i: TInfo): TString;
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

PROCEDURE Assert (condicion: BOOLEAN; mensaje: ARRAY OF CHAR);
BEGIN
   
   IF NOT condicion THEN
      WriteString(mensaje);
      HALT;
   END;

END Assert;

END Utils.
