@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "DrtTest.drt" del "DrtTest.drt"
if exist "DrtTest.Mf2005Values" del "DrtTest.Mf2005Values"
if exist "DrtTest.cbc" del "DrtTest.cbc"
if exist "DrtTest.chob_out" del "DrtTest.chob_out"
if exist "DrtTest.fdn" del "DrtTest.fdn"
if exist "DrtTest.fhd" del "DrtTest.fhd"
if exist "DrtTest.lst" del "DrtTest.lst"
"EnhancedTemplateProcessor.exe" DrtTest.drt.tpl DrtTest.pval
mf2005.exe DrtTest.nam /wait
"Mf2005ObsExtractor.exe" DrtTest.Mf2005ExtractValues
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

