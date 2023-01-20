@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "EvtTest.evt" del "EvtTest.evt"
if exist "EvtTest.Mf2005Values" del "EvtTest.Mf2005Values"
if exist "EvtTest.cbc" del "EvtTest.cbc"
if exist "EvtTest.fdn" del "EvtTest.fdn"
if exist "EvtTest.fhd" del "EvtTest.fhd"
if exist "EvtTest.lst" del "EvtTest.lst"
if exist "EvtTest.rvob_out" del "EvtTest.rvob_out"
"EnhancedTemplateProcessor.exe" EvtTest.evt.tpl EvtTest.pval
mf2005.exe EvtTest.nam /wait
"Mf2005ObsExtractor.exe" EvtTest.Mf2005ExtractValues
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

