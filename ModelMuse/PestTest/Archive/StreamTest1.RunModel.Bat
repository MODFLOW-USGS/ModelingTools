@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\StreamTest1.Kx_1.arrays" del "arrays\StreamTest1.Kx_1.arrays"
if exist "arrays\StreamTest1.Kx_2.arrays" del "arrays\StreamTest1.Kx_2.arrays"
if exist "arrays\StreamTest1.Kx_3.arrays" del "arrays\StreamTest1.Kx_3.arrays"
if exist "StreamTest1.Mf2005Values" del "StreamTest1.Mf2005Values"
if exist "StreamTest1.str" del "StreamTest1.str"
if exist "StreamTest1.cbc" del "StreamTest1.cbc"
if exist "StreamTest1.fdn" del "StreamTest1.fdn"
if exist "StreamTest1.fhd" del "StreamTest1.fhd"
if exist "StreamTest1.lst" del "StreamTest1.lst"
if exist "StreamTest1.stob_out" del "StreamTest1.stob_out"
if exist "StreamTest1.str_flow" del "StreamTest1.str_flow"
"plproc.exe" StreamTest1.Kx.script
"EnhancedTemplateProcessor.exe" StreamTest1.str.tpl StreamTest1.pval
mf2005.exe StreamTest1.nam /wait
"Mf2005ObsExtractor.exe" StreamTest1.Mf2005ExtractValues
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

