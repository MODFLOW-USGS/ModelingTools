@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\EtsTest.Kx_1.arrays" del "arrays\EtsTest.Kx_1.arrays"
if exist "arrays\EtsTest.Kx_2.arrays" del "arrays\EtsTest.Kx_2.arrays"
if exist "arrays\EtsTest.Kx_3.arrays" del "arrays\EtsTest.Kx_3.arrays"
if exist "EtsTest.ets" del "EtsTest.ets"
if exist "EtsTest.Mf2005Values" del "EtsTest.Mf2005Values"
if exist "EtsTest.cbc" del "EtsTest.cbc"
if exist "EtsTest.fdn" del "EtsTest.fdn"
if exist "EtsTest.fhd" del "EtsTest.fhd"
if exist "EtsTest.hob_out" del "EtsTest.hob_out"
if exist "EtsTest.lst" del "EtsTest.lst"
"plproc.exe" EtsTest.Kx.script
"EnhancedTemplateProcessor.exe" EtsTest.ets.tpl EtsTest.pval
mf2005.exe EtsTest.nam /wait
"Mf2005ObsExtractor.exe" EtsTest.Mf2005ExtractValues
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

