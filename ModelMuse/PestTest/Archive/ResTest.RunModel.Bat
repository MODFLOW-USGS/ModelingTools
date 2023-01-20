@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\ResTest.Reservoir_Bed_Thickness_1.arrays" del "arrays\ResTest.Reservoir_Bed_Thickness_1.arrays"
if exist "ResTest.Mf2005Values" del "ResTest.Mf2005Values"
if exist "ResTest.resv" del "ResTest.resv"
if exist "ResTest.cbc" del "ResTest.cbc"
if exist "ResTest.fdn" del "ResTest.fdn"
if exist "ResTest.fhd" del "ResTest.fhd"
if exist "ResTest.hob_out" del "ResTest.hob_out"
if exist "ResTest.lst" del "ResTest.lst"
"plproc.exe" ResTest.Reservoir_Bed_Thickness.script
"EnhancedTemplateProcessor.exe" ResTest.resv.tpl ResTest.pval
mf2005.exe ResTest.nam /wait
"Mf2005ObsExtractor.exe" ResTest.Mf2005ExtractValues
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

