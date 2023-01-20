@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\DryCells.Kx_1.arrays" del "arrays\DryCells.Kx_1.arrays"
if exist "arrays\DryCells.Kx_2.arrays" del "arrays\DryCells.Kx_2.arrays"
if exist "arrays\DryCells.Kx_3.arrays" del "arrays\DryCells.Kx_3.arrays"
if exist "DryCells.Mf2005Values" del "DryCells.Mf2005Values"
if exist "DryCells.bhd" del "DryCells.bhd"
if exist "DryCells.cbc" del "DryCells.cbc"
if exist "DryCells.drob_out" del "DryCells.drob_out"
if exist "DryCells.fdn" del "DryCells.fdn"
if exist "DryCells.hob_out" del "DryCells.hob_out"
if exist "DryCells.lst" del "DryCells.lst"
if exist "DryCells.sfrg1" del "DryCells.sfrg1"
"plproc.exe" DryCells.Kx.script
mf2005.exe DryCells.nam /wait
"Mf2005ObsExtractor.exe" DryCells.Mf2005ExtractValues
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

