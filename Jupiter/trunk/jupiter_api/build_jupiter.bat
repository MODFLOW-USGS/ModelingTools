@echo off

REM  Batch file for compiling API modules and example applications
REM  Invoke this batch file from the directory that is the parent 
REM  of the "jupiter_api" directory.

REM Set up environment variables for running ifort, the Intel Fortran compiler.

echo Calling  ifortvars.bat...
call ifortvars.bat
REM call "D:\Program Files\Intel\Compiler\Fortran\10.1.011\IA32\Bin\ifortvars.bat"

REM Compile API modules
echo Compiling API modules...
echo Compiling typ.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\typ.f90
echo Compiling gdt.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\gdt.f90
echo Compiling utl.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\utl.f90
echo Compiling bas.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\bas.f90
echo Compiling mio.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\mio.f90
echo Compiling eqn.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\eqn.f90
echo Compiling dep.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\dep.f90
echo Compiling pri.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\pri.f90
echo Compiling pll.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\pll.f90
echo Compiling sen.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\sen.f90
echo Compiling sta.f90...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\api_modules\sta.f90

REM Build example applications
echo.
echo Building example applications...
echo.

REM   Group_example
echo Building group_example...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c /libs:static /Fe.\jupiter_api\bin\ .\jupiter_api\src\group_example\group_example.f90
link /NODEFAULTLIB:LIBC .\jupiter_api\obj\typ.obj .\jupiter_api\obj\gdt.obj .\jupiter_api\obj\utl.obj .\jupiter_api\obj\group_example.obj /OUT:.\jupiter_api\bin\group_example.exe

REM   Sensitivity_example
echo Building sensitivity_example...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\sensitivity_example\sensitivity_example_mod.f90
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c /libs:static /Fe.\jupiter_api\bin\ .\jupiter_api\src\sensitivity_example\sensitivity_example.f90
link /NODEFAULTLIB:LIBC .\jupiter_api\obj\typ.obj .\jupiter_api\obj\gdt.obj .\jupiter_api\obj\utl.obj .\jupiter_api\obj\bas.obj .\jupiter_api\obj\mio.obj .\jupiter_api\obj\eqn.obj .\jupiter_api\obj\dep.obj .\jupiter_api\obj\pri.obj .\jupiter_api\obj\pll.obj .\jupiter_api\obj\sen.obj .\jupiter_api\obj\sta.obj .\jupiter_api\obj\sensitivity_example_mod.obj .\jupiter_api\obj\sensitivity_example.obj /OUT:.\jupiter_api\bin\sensitivity_example.exe

REM   Jrunner
echo Building jrunner...
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c .\jupiter_api\src\jrunner\jrunner_mod.f90
ifort /O2 /Fo.\jupiter_api\obj\ /nologo /module:.\jupiter_api\mod /c /libs:static /Fe.\jupiter_api\bin\ .\jupiter_api\src\jrunner\jrunner.f90
link /NODEFAULTLIB:LIBC .\jupiter_api\obj\typ.obj .\jupiter_api\obj\gdt.obj .\jupiter_api\obj\utl.obj .\jupiter_api\obj\bas.obj .\jupiter_api\obj\mio.obj .\jupiter_api\obj\eqn.obj .\jupiter_api\obj\dep.obj .\jupiter_api\obj\pri.obj .\jupiter_api\obj\pll.obj .\jupiter_api\obj\sen.obj .\jupiter_api\obj\sta.obj .\jupiter_api\obj\jrunner_mod.obj .\jupiter_api\obj\jrunner.obj /OUT:.\jupiter_api\bin\jrunner.exe

if "%1"=="nopause" goto end
if "%1"=="NOPAUSE" goto end
pause
:end
