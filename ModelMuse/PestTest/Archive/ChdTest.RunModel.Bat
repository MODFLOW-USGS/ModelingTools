@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\ChdTest.Kx_1.arrays" del "arrays\ChdTest.Kx_1.arrays"
if exist "arrays\ChdTest.Kx_2.arrays" del "arrays\ChdTest.Kx_2.arrays"
if exist "arrays\ChdTest.Kx_3.arrays" del "arrays\ChdTest.Kx_3.arrays"
if exist "ChdTest.chd" del "ChdTest.chd"
if exist "ChdTest.Mf2005Values" del "ChdTest.Mf2005Values"
if exist "ChdTest.cbc" del "ChdTest.cbc"
if exist "ChdTest.fdn" del "ChdTest.fdn"
if exist "ChdTest.fhd" del "ChdTest.fhd"
if exist "ChdTest.hob_out" del "ChdTest.hob_out"
if exist "ChdTest.lst" del "ChdTest.lst"
"plproc.exe" ChdTest.Kx.script
"EnhancedTemplateProcessor.exe" ChdTest.chd.tpl ChdTest.pval
mf2005.exe ChdTest.nam /wait
"Mf2005ObsExtractor.exe" ChdTest.Mf2005ExtractValues
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

