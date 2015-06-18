@echo off

SET ENTREGAR=(Binario, ColaBinario, ColaPrioridad, ListaString, Pila, Procesos, Set, Sistema, Solicitudes, Utils)

call clean

FOR %%I IN %ENTREGAR% DO xc =make %%I.mod

FOR %%I IN %ENTREGAR% DO ( 
	IF NOT EXIST obj\%%I.obj GOTO NOCOMPILA
)


IF EXIST TestTarea3.exe del TestTarea3.exe
:: compila
xc =make TestTarea3.mod 


SET CASOSGENERAL=(00 01 02 03 04 05 06 07 08 09 11 12 13 14 15 16 17 18 19)

FOR %%I IN %CASOSGENERAL% DO TestTarea3.exe < test\Caso%%I.in > test\Caso%%I.sal
FOR %%I IN %CASOSGENERAL% DO diff test\Caso%%I.out test\Caso%%I.sal > test\Caso%%I.diff


:: si el tamaño del archivo es 0, entonces hay un error
dir test\Caso*.sal
:: si el tamaño del archivo NO es 0, entonces hay un error
dir test\Caso*.diff

:: Archivo a entregar
IF EXIST Entrega3.tar.gz del Entrega3.tar.gz 
tar cvf Entrega3.tar -C src/ Binario.mod ColaBinario.mod ColaPrioridad.mod ListaString.mod Pila.mod Procesos.mod Set.mod Sistema.mod Solicitudes.mod Utils.mod
gzip Entrega3.tar

GOTO FIN
::-----------------------------------------

:NOCOMPILA
echo Error de compilacion
GOTO FIN
::-----------------------------------------

:FIN
