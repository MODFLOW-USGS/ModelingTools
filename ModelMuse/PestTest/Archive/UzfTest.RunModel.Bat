@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "UzfTest.Mf2005Values" del "UzfTest.Mf2005Values"
if exist "UzfTest.uzf" del "UzfTest.uzf"
if exist "UzfTest.cbc" del "UzfTest.cbc"
if exist "UzfTest.chob_out" del "UzfTest.chob_out"
if exist "UzfTest.fdn" del "UzfTest.fdn"
if exist "UzfTest.fhd" del "UzfTest.fhd"
if exist "UzfTest.lst" del "UzfTest.lst"
if exist "UzfTest.sfrg1" del "UzfTest.sfrg1"
if exist "UzfTest.sfrg2" del "UzfTest.sfrg2"
"EnhancedTemplateProcessor.exe" UzfTest.uzf.tpl UzfTest.pval
mf2005.exe UzfTest.nam /wait
"Mf2005ObsExtractor.exe" UzfTest.Mf2005ExtractValues
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

