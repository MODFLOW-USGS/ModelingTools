@echo off
echo Start of MAKE_JUPITER_DIST.BAT -- Argument(s) are: %1 %2
if "%1"=="" goto usage
set dirnam=%1

REM
REM ########################################################################
REM Set up temporary directory structure
REM ########################################################################
REM
echo Creating temporary directory structure in Release directory...
D:
cd \Projects\Jupiter\Release
mkdir %dirnam%
cd %dirnam%
  mkdir %dirnam%
  cd %dirnam%
    mkdir jupiter_api
    cd jupiter_api
      mkdir bin
      mkdir doc
      mkdir mod
      mkdir obj
      mkdir src
      cd src
        mkdir api_modules
        mkdir group_example
        mkdir jrunner
        mkdir sensitivity_example
        cd ..
      mkdir test
      cd test
        mkdir group_example
        mkdir sensitivity_example
        cd sensitivity_example
         mkdir runner1
         mkdir runner2
         cd ..
      cd ..
    cd ..
  cd ..
REM
REM ########################################################################
REM Delete old .f90, .obj, .mod, .txt, and pdf files in ForRelease directory
REM ########################################################################
REM
echo Deleting .f90, .obj, and .mod files in ForRelease directory...
cd D:\Projects\jupiter\forrelease\jupiter_api
cd doc
  del readme.txt
  del release.txt
  del subprograms_addendum.doc
  del subprograms_addendum.pdf
  cd ..
cd mod
  del *.mod
  cd ..
cd obj
  del *.obj
  cd ..
cd src
  cd api_modules
    del *.f90
    cd ..
  cd group_example
    del *.f90
    cd ..
  cd sensitivity_example
    del *.f90
    cd ..
  cd jrunner
    del *.f90
    cd ..
  cd ..
REM
REM ########################################################################
REM  Copy documentation files into ForRelease
REM ########################################################################
REM
echo Copying documentation files to ForRelease...
cd D:\Projects\Jupiter\ForRelease\jupiter_api\doc
  copy D:\Projects\Jupiter_repository\jupiter_api\doc\readme.txt .
  copy D:\Projects\Jupiter_repository\jupiter_api\doc\release.txt .
  copy D:\Projects\Jupiter_repository\jupiter_api\doc\subprograms_addendum.doc .
  copy D:\Projects\Jupiter_repository\jupiter_api\doc\subprograms_addendum.pdf .
  cd ..
REM
REM ########################################################################
REM  Copy source code files into ForRelease
REM ########################################################################
REM
echo Copying source-code files to ForRelease...
cd D:\Projects\Jupiter\ForRelease\jupiter_api\src
cd api_modules
  copy D:\Projects\Jupiter_repository\jupiter_api\src\api_modules\*.f90 .
  cd ..
cd group_example
  copy D:\Projects\Jupiter_repository\jupiter_api\src\group_example\*.f90 .
  cd ..
cd sensitivity_example
  copy D:\Projects\Jupiter_repository\jupiter_api\src\sensitivity_example\*.f90 .
  cd ..
cd jrunner
  copy D:\Projects\Jupiter_repository\jupiter_api\src\jrunner\*.f90 .
  cd ..
REM
REM ########################################################################
REM  Compile executables in ForRelease
REM ########################################################################
REM  
REM  Build JUPITER example applications
cd D:\projects\jupiter\forrelease
if exist build_jupiter.bat del build_jupiter.bat
copy D:\Projects\Jupiter_repository\jupiter_api\build_jupiter.bat .
echo.
echo Make_jupiter_dist is invoking build_jupiter.bat
echo   to compile JUPITER example applications ...
echo.
call build_jupiter.bat nopause
REM
REM ########################################################################
REM  Copy files and directories from ForRelease to Release
REM ########################################################################
REM
echo.
echo Make_jupiter_dist will now copy release directory tree ...
echo.
REM
REM Copy executable files 
cd \projects\jupiter\release
copy D:\projects\jupiter\forrelease\jupiter_api\bin\*.exe .\%dirnam%\%dirnam%\jupiter_api\bin
REM
REM Copy source-code files
copy D:\projects\jupiter\forrelease\jupiter_api\src\api_modules\*.f90 .\%dirnam%\%dirnam%\jupiter_api\src\api_modules
copy D:\projects\jupiter\forrelease\jupiter_api\src\group_example\*.f90 .\%dirnam%\%dirnam%\jupiter_api\src\group_example
copy D:\projects\jupiter\forrelease\jupiter_api\src\jrunner\*.f90 .\%dirnam%\%dirnam%\jupiter_api\src\jrunner
copy D:\projects\jupiter\forrelease\jupiter_api\src\sensitivity_example\*.f90 .\%dirnam%\%dirnam%\jupiter_api\src\sensitivity_example
REM
REM Copy contents of test directories
copy D:\projects\jupiter_repository\jupiter_api\test\group_example\*.* .\%dirnam%\%dirnam%\jupiter_api\test\group_example
copy D:\projects\jupiter_repository\jupiter_api\test\sensitivity_example\*.* .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example
copy D:\projects\jupiter_repository\jupiter_api\test\sensitivity_example\runner1\*.* .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\runner1
copy D:\projects\jupiter_repository\jupiter_api\test\sensitivity_example\runner2\*.* .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\runner2
REM
REM Copy documentation
REM Copy text files from jupiter_repository\jupiter_api\doc
copy D:\projects\jupiter_repository\jupiter_api\doc\*.txt .\%dirnam%\%dirnam%\jupiter_api\doc
REM Copy PDF file from jupiter\forrelease\jupiter_api\doc
copy D:\projects\jupiter\forrelease\jupiter_api\doc\*.pdf .\%dirnam%\%dirnam%\jupiter_api\doc
REM
REM Delete unneeded directories
rmdir .\%dirnam%\%dirnam%\jupiter_api\mod
rmdir .\%dirnam%\%dirnam%\jupiter_api\obj
REM
REM Compress files, preserving directory structure
wzzip -rp %dirnam%.zip %dirnam%\%dirnam%\*.*

REM
REM ########################################################################
REM  Delete temporary directory structure
REM ########################################################################
REM
  
echo.
echo Make_jupiter_dist will now delete temporary directory structure ...
echo.

REM bin directory
del .\%dirnam%\%dirnam%\jupiter_api\bin\*.exe
rmdir .\%dirnam%\%dirnam%\jupiter_api\bin
REM
REM doc directory
del .\%dirnam%\%dirnam%\jupiter_api\doc\*.txt
del .\%dirnam%\%dirnam%\jupiter_api\doc\*.pdf
rmdir .\%dirnam%\%dirnam%\jupiter_api\doc
REM
REM api_modules source directory
del .\%dirnam%\%dirnam%\jupiter_api\src\api_modules\*.f90
rmdir .\%dirnam%\%dirnam%\jupiter_api\src\api_modules
REM
REM group_example source directory
del .\%dirnam%\%dirnam%\jupiter_api\src\group_example\*.f90
rmdir .\%dirnam%\%dirnam%\jupiter_api\src\group_example
REM
REM jrunner source directory
del .\%dirnam%\%dirnam%\jupiter_api\src\jrunner\*.f90
rmdir .\%dirnam%\%dirnam%\jupiter_api\src\jrunner
REM
REM sensitivity_example source directory
del .\%dirnam%\%dirnam%\jupiter_api\src\sensitivity_example\*.f90
rmdir .\%dirnam%\%dirnam%\jupiter_api\src\sensitivity_example
REM
REM test directory files and subdirectories
del .\%dirnam%\%dirnam%\jupiter_api\test\group_example\gp.*
del .\%dirnam%\%dirnam%\jupiter_api\test\group_example\runtest.bat
rmdir .\%dirnam%\%dirnam%\jupiter_api\test\group_example
del .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\runner1\tc1*.*
del .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\runner1\start_runner.bat
rmdir .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\runner1
del .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\runner2\tc1*.*
del .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\runner2\start_runner.bat
rmdir .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\runner2
del .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\*.bat
del .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\*.in
del .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\sen*.*
del .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example\tc1*.*
rmdir .\%dirnam%\%dirnam%\jupiter_api\test\sensitivity_example
REM
REM mod, obj, src, and test directories
rmdir .\%dirnam%\%dirnam%\jupiter_api\src
rmdir .\%dirnam%\%dirnam%\jupiter_api\test
REM
REM parent directories
rmdir .\%dirnam%\%dirnam%\jupiter_api
rmdir .\%dirnam%\%dirnam%
rmdir .\%dirnam%
echo.
echo Make_jupiter_dist done making distribution: %dirnam%
echo.
goto end
REM
:usage
echo.
echo Usage: MAKE_JUPITER_DIST version-name
echo.
echo Example:
echo.
echo MAKE_JUPITER_DIST jupiter_api_1_4_0
echo.
REM
:end
cd D:\Projects\Jupiter_repository\jupiter_api
  
