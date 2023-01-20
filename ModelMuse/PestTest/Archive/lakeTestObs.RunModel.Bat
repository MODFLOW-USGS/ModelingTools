@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\lakeTestObs.Lakebed_Leakance_1.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_1.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_2.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_2.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_3.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_3.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_4.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_4.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_5.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_5.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_7.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_7.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_8.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_8.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_9.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_9.arrays"
if exist "lakeTestObs.lak" del "lakeTestObs.lak"
if exist "lakeTestObs.Mf2005Values" del "lakeTestObs.Mf2005Values"
if exist "lakeTestObs.cbc" del "lakeTestObs.cbc"
if exist "lakeTestObs.fdn" del "lakeTestObs.fdn"
if exist "lakeTestObs.fhd" del "lakeTestObs.fhd"
if exist "lakeTestObs.hob_out" del "lakeTestObs.hob_out"
if exist "lakeTestObs.lakg1" del "lakeTestObs.lakg1"
if exist "lakeTestObs.lakg2" del "lakeTestObs.lakg2"
if exist "lakeTestObs.lst" del "lakeTestObs.lst"
"plproc.exe" lakeTestObs.Lakebed_Leakance.script
"EnhancedTemplateProcessor.exe" lakeTestObs.lak.tpl lakeTestObs.pval
mf2005.exe lakeTestObs.nam /wait
"Mf2005ObsExtractor.exe" lakeTestObs.Mf2005ExtractValues
@echo off
rem Get end time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "end=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)

rem Get elapsed time:
set /A elapsed=end-start

rem Show elapsed time:
set /A hh=elapsed/(60*60*100), rest=elapsed%%(60*60*100), mm=rest/(60*100), rest%%=60*100, ss=rest/100, cc=rest%%100
if %mm% lss 10 set mm=0%mm%
if %ss% lss 10 set ss=0%ss%
if %cc% lss 10 set cc=0%cc%
echo elapsed time: %hh% hours, %mm% minutes %ss%.%cc% seconds

