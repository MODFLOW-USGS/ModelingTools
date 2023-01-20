@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\SwtObsTest.Specific_Gravity_Saturated_1.arrays" del "arrays\SwtObsTest.Specific_Gravity_Saturated_1.arrays"
if exist "SwtObsTest.Mf2005Values" del "SwtObsTest.Mf2005Values"
if exist "SwtObsTest.cbc" del "SwtObsTest.cbc"
if exist "SwtObsTest.fdn" del "SwtObsTest.fdn"
if exist "SwtObsTest.fhd" del "SwtObsTest.fhd"
if exist "SwtObsTest.lst" del "SwtObsTest.lst"
if exist "SwtObsTest.Swt_Out" del "SwtObsTest.Swt_Out"
"plproc.exe" SwtObsTest.Specific_Gravity_Saturated.script
mf2005.exe SwtObsTest.nam /wait
"Mf2005ObsExtractor.exe" SwtObsTest.Mf2005ExtractValues
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

