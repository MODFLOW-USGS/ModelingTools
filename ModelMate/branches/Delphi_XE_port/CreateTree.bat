@echo off
echo Start of CreateTree...
if "%1"=="" goto usage
set dirnam=%1

if exist %dirnam% goto usage

mkdir %dirnam%

cd %dirnam%

mkdir Bin

copy ..\ModelMate.exe bin
copy ..\accjupiter.dll bin
copy ..\FastMM_FullDebugMode.dll bin
copy C:\WRDAPP\MF2005.1_8\bin\mf2005.exe bin
copy C:\WRDAPP\ucode_2005_1.019\bin\ucode_2005.exe bin

mkdir Example

copy ..\test_project\tc1.bas Example
copy ..\test_project\tc1.dis Example
copy ..\test_project\tc1.ghb Example
copy ..\test_project\tc1.lpf Example
copy ..\test_project\tc1.mlt Example
copy ..\test_project\tc1.nam Example
copy ..\test_project\tc1.obh Example
copy ..\test_project\tc1.oc Example
copy ..\test_project\tc1.pcg Example
copy ..\test_project\tc1.rch Example
copy ..\test_project\tc1.riv Example
copy ..\test_project\tc1.wel Example
copy ..\test_project\tc1.zon Example
copy ..\test_project\tc1_start.pval Example
copy ..\test_project\tc1_start.pval Example\tc1.pval
copy ..\test_project\tc1pred.bas Example
copy ..\test_project\tc1pred.dis Example
copy ..\test_project\tc1pred.nam Example
copy ..\test_project\tc1pred.obh Example
copy ..\test_project\tc1pred.pval Example
copy ..\test_project\tc1pred.wel Example

mkdir Completed_Example

copy ..\Completed_Example\tc1.bas Completed_Example
copy ..\Completed_Example\tc1.dis Completed_Example
copy ..\Completed_Example\tc1.ghb Completed_Example
copy ..\Completed_Example\tc1.lpf Completed_Example
copy ..\Completed_Example\tc1.mlt Completed_Example
copy ..\Completed_Example\tc1.nam Completed_Example
copy ..\Completed_Example\tc1.obh Completed_Example
copy ..\Completed_Example\tc1.oc Completed_Example
copy ..\Completed_Example\tc1.pcg Completed_Example
copy ..\Completed_Example\tc1.rch Completed_Example
copy ..\Completed_Example\tc1.riv Completed_Example
copy ..\Completed_Example\tc1.wel Completed_Example
copy ..\Completed_Example\tc1.zon Completed_Example
copy ..\Completed_Example\tc1_start.pval Completed_Example
copy ..\Completed_Example\tc1.pval Completed_Example
copy ..\Completed_Example\tc1_test.mtc Completed_Example
copy ..\Completed_Example\tc1pred.bas Completed_Example
copy ..\Completed_Example\tc1pred.dis Completed_Example
copy ..\Completed_Example\tc1pred.nam Completed_Example
copy ..\Completed_Example\tc1pred.obh Completed_Example
copy ..\Completed_Example\tc1pred.pval Completed_Example
copy ..\Completed_Example\tc1pred.wel Completed_Example

copy ..\Completed_Example\RunUcode_tutorial.bat Completed_Example
copy ..\Completed_Example\tc1._os Completed_Example
copy ..\Completed_Example\tc1._os.jif Completed_Example
copy ..\Completed_Example\tc1.bhd Completed_Example
copy ..\Completed_Example\tc1.cbc Completed_Example
copy ..\Completed_Example\tc1.lst Completed_Example
copy ..\Completed_Example\tc1_main.in Completed_Example
copy ..\Completed_Example\tc1_main_out.#uout Completed_Example
copy ..\Completed_Example\tc1_main_out._b1 Completed_Example
copy ..\Completed_Example\tc1_main_out._dm Completed_Example
copy ..\Completed_Example\tc1_main_out._gm Completed_Example
copy ..\Completed_Example\tc1_main_out._init Completed_Example
copy ..\Completed_Example\tc1_main_out._init._dm Completed_Example
copy ..\Completed_Example\tc1_main_out._init._mv Completed_Example
copy ..\Completed_Example\tc1_main_out._init._su Completed_Example
copy ..\Completed_Example\tc1_main_out._init._supri Completed_Example
copy ..\Completed_Example\tc1_main_out._mc Completed_Example
copy ..\Completed_Example\tc1_main_out._mv Completed_Example
copy ..\Completed_Example\tc1_main_out._nm Completed_Example
copy ..\Completed_Example\tc1_main_out._os Completed_Example
copy ..\Completed_Example\tc1_main_out._pa Completed_Example
copy ..\Completed_Example\tc1_main_out._paopt Completed_Example
copy ..\Completed_Example\tc1_main_out._papri Completed_Example
copy ..\Completed_Example\tc1_main_out._pasub Completed_Example
copy ..\Completed_Example\tc1_main_out._pc Completed_Example
copy ..\Completed_Example\tc1_main_out._pcc Completed_Example
copy ..\Completed_Example\tc1_main_out._pr Completed_Example
copy ..\Completed_Example\tc1_main_out._r Completed_Example
copy ..\Completed_Example\tc1_main_out._s1 Completed_Example
copy ..\Completed_Example\tc1_main_out._sc Completed_Example
copy ..\Completed_Example\tc1_main_out._sc_svd Completed_Example
copy ..\Completed_Example\tc1_main_out._scgrp Completed_Example
copy ..\Completed_Example\tc1_main_out._sd Completed_Example
copy ..\Completed_Example\tc1_main_out._so Completed_Example
copy ..\Completed_Example\tc1_main_out._ss Completed_Example
copy ..\Completed_Example\tc1_main_out._su Completed_Example
copy ..\Completed_Example\tc1_main_out._summary Completed_Example
copy ..\Completed_Example\tc1_main_out._supri Completed_Example
copy ..\Completed_Example\tc1_main_out._w Completed_Example
copy ..\Completed_Example\tc1_main_out._ws Completed_Example
copy ..\Completed_Example\tc1_main_out._wt Completed_Example
copy ..\Completed_Example\tc1_main_out._wtpri Completed_Example
copy ..\Completed_Example\tc1_main_out._ww Completed_Example
copy ..\Completed_Example\tc1_main_out.omit Completed_Example
copy ..\Completed_Example\tc1_pred.in Completed_Example
copy ..\Completed_Example\tc1_pred_out.#upred Completed_Example
copy ..\Completed_Example\tc1_pred_out._dm Completed_Example
copy ..\Completed_Example\tc1_pred_out._dmp Completed_Example
copy ..\Completed_Example\tc1_pred_out._gmp Completed_Example
copy ..\Completed_Example\tc1_pred_out._mv Completed_Example
copy ..\Completed_Example\tc1_pred_out._p Completed_Example
copy ..\Completed_Example\tc1_pred_out._paopt Completed_Example
copy ..\Completed_Example\tc1_pred_out._pc Completed_Example
copy ..\Completed_Example\tc1_pred_out._pv Completed_Example
copy ..\Completed_Example\tc1_pred_out.omit Completed_Example
copy ..\Completed_Example\tc1_pval.jtf Completed_Example
copy ..\Completed_Example\tc1pred._os Completed_Example
copy ..\Completed_Example\tc1pred._os.jif Completed_Example
copy ..\Completed_Example\tc1pred.bhd Completed_Example
copy ..\Completed_Example\tc1pred.cbc Completed_Example
copy ..\Completed_Example\tc1pred.lst Completed_Example
copy ..\Completed_Example\tc1pred_pval.jtf Completed_Example

mkdir Doc

copy ..\ModelMate_tutorial.doc Doc

cd ..
goto end

:usage
echo.
echo Usage: CreateTree Directory-name
echo where Directory-name is of form: ModelMate_0_1
echo Directory-name must not exist
echo.

REM
:end
