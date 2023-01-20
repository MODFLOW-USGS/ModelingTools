@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "GhbTest.ghb" del "GhbTest.ghb"
if exist "GhbTest.Mf2005Values" del "GhbTest.Mf2005Values"
if exist "GhbTest.cbc" del "GhbTest.cbc"
if exist "GhbTest.fdn" del "GhbTest.fdn"
if exist "GhbTest.fhd" del "GhbTest.fhd"
if exist "GhbTest.hob_out" del "GhbTest.hob_out"
if exist "GhbTest.lst" del "GhbTest.lst"
"EnhancedTemplateProcessor.exe" GhbTest.ghb.tpl GhbTest.pval
mf2005.exe GhbTest.nam /wait
"Mf2005ObsExtractor.exe" GhbTest.Mf2005ExtractValues
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

