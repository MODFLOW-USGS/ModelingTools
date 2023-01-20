@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "sfrtest2.Mf2005Values" del "sfrtest2.Mf2005Values"
if exist "sfrtest2.sfr" del "sfrtest2.sfr"
if exist "sfrtest2.cbc" del "sfrtest2.cbc"
if exist "sfrtest2.fdn" del "sfrtest2.fdn"
if exist "sfrtest2.fhd" del "sfrtest2.fhd"
if exist "sfrtest2.hob_out" del "sfrtest2.hob_out"
if exist "sfrtest2.lst" del "sfrtest2.lst"
"EnhancedTemplateProcessor.exe" sfrtest2.sfr.tpl sfrtest2.pval
mf2005.exe sfrtest2.nam /wait
"Mf2005ObsExtractor.exe" sfrtest2.Mf2005ExtractValues
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

