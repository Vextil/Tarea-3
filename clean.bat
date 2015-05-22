@echo off


IF EXIST test\*.sal del test\*.sal
IF EXIST test\*.diff del test\*.diff
IF EXIST test\*.log del test\*.log
IF EXIST errinfo.$$$ del errinfo.$$$


IF EXIST obj\*.obj del obj\*.obj
IF EXIST sym\*.sym del sym\*.sym
IF EXIST *.exe del *.exe
IF EXIST tmp.lnk del tmp.lnk
