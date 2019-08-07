@echo off
if "%1"=="" goto USAGE
for %%f in ( %1 ) do filtyp3 %%f %2
goto DONE
:USAGE
echo USAGE: FLAGTYPE fname [ ftype ]
echo   where: fname can contain wildcards, and
echo          ftype is one of: UNIX, DOS, or MAC
:DONE
