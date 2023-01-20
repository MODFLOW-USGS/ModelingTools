@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "RechAddTest.Mf2005Values" del "RechAddTest.Mf2005Values"
if exist "RechAddTest.rch" del "RechAddTest.rch"
if exist "RechAddTest.cbc" del "RechAddTest.cbc"
if exist "RechAddTest.fdn" del "RechAddTest.fdn"
if exist "RechAddTest.fhd" del "RechAddTest.fhd"
if exist "RechAddTest.hob_out" del "RechAddTest.hob_out"
if exist "RechAddTest.lst" del "RechAddTest.lst"
"EnhancedTemplateProcessor.exe" RechAddTest.rch.tpl RechAddTest.pval
mf2005.exe RechAddTest.nam /wait
"Mf2005ObsExtractor.exe" RechAddTest.Mf2005ExtractValues
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

