@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\Drntest2.Kx_1.arrays" del "arrays\Drntest2.Kx_1.arrays"
if exist "arrays\Drntest2.Kx_2.arrays" del "arrays\Drntest2.Kx_2.arrays"
if exist "arrays\Drntest2.Kx_3.arrays" del "arrays\Drntest2.Kx_3.arrays"
if exist "arrays\Drntest2.test_1.arrays" del "arrays\Drntest2.test_1.arrays"
if exist "Drntest2.drn" del "Drntest2.drn"
if exist "Drntest2.Mf2005Values" del "Drntest2.Mf2005Values"
if exist "Drntest2.bdn" del "Drntest2.bdn"
if exist "Drntest2.hob_out" del "Drntest2.hob_out"
if exist "Drntest2.lst" del "Drntest2.lst"
"plproc.exe" Drntest2.Kx.script
"plproc.exe" Drntest2.test.script
"EnhancedTemplateProcessor.exe" Drntest2.drn.tpl Drntest2.pval
mf2005.exe Drntest2.nam /wait
"Mf2005ObsExtractor.exe" Drntest2.Mf2005ExtractValues
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

