@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\WellTest.Horizontal_Anisotropy_1.arrays" del "arrays\WellTest.Horizontal_Anisotropy_1.arrays"
if exist "arrays\WellTest.Horizontal_Anisotropy_2.arrays" del "arrays\WellTest.Horizontal_Anisotropy_2.arrays"
if exist "arrays\WellTest.Horizontal_Anisotropy_3.arrays" del "arrays\WellTest.Horizontal_Anisotropy_3.arrays"
if exist "WellTest.Mf2005Values" del "WellTest.Mf2005Values"
if exist "WellTest.wel" del "WellTest.wel"
if exist "WellTest.cbc" del "WellTest.cbc"
if exist "WellTest.chob_out" del "WellTest.chob_out"
if exist "WellTest.fdn" del "WellTest.fdn"
if exist "WellTest.fhd" del "WellTest.fhd"
if exist "WellTest.lst" del "WellTest.lst"
"plproc.exe" WellTest.Horizontal_Anisotropy.script
"EnhancedTemplateProcessor.exe" WellTest.wel.tpl WellTest.pval
mf2005.exe WellTest.nam /wait
"Mf2005ObsExtractor.exe" WellTest.Mf2005ExtractValues
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

