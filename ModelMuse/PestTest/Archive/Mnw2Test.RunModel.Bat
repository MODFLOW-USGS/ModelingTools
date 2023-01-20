@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "Mnw2Test.Mf2005Values" del "Mnw2Test.Mf2005Values"
if exist "Mnw2Test.mnw2" del "Mnw2Test.mnw2"
if exist "Mnw2Test.cbc" del "Mnw2Test.cbc"
if exist "Mnw2Test.fdn" del "Mnw2Test.fdn"
if exist "Mnw2Test.fhd" del "Mnw2Test.fhd"
if exist "Mnw2Test.hob_out" del "Mnw2Test.hob_out"
if exist "Mnw2Test.lst" del "Mnw2Test.lst"
"EnhancedTemplateProcessor.exe" Mnw2Test.mnw2.tpl Mnw2Test.pval
mf2005.exe Mnw2Test.nam /wait
"Mf2005ObsExtractor.exe" Mnw2Test.Mf2005ExtractValues
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

