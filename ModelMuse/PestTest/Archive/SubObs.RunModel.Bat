@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\SubObs.Layer1_D_Delay_Interbed_Equivalent_Thickness_1.arrays" del "arrays\SubObs.Layer1_D_Delay_Interbed_Equivalent_Thickness_1.arrays"
if exist "SubObs.Mf2005Values" del "SubObs.Mf2005Values"
if exist "SubObs.cbc" del "SubObs.cbc"
if exist "SubObs.fdn" del "SubObs.fdn"
if exist "SubObs.fhd" del "SubObs.fhd"
if exist "SubObs.lst" del "SubObs.lst"
if exist "SubObs.SubComIsOut" del "SubObs.SubComIsOut"
if exist "SubObs.SubComMlOut" del "SubObs.SubComMlOut"
if exist "SubObs.SubSubOut" del "SubObs.SubSubOut"
"plproc.exe" SubObs.Layer1_D_Delay_Interbed_Equivalent_Thickness.script
mf2005.exe SubObs.nam /wait
"Mf2005ObsExtractor.exe" SubObs.Mf2005ExtractValues
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

