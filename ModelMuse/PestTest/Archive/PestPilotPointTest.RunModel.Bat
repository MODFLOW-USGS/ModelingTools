@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\PestPilotPointTest.Kx_1.arrays" del "arrays\PestPilotPointTest.Kx_1.arrays"
if exist "arrays\PestPilotPointTest.Kx_2.arrays" del "arrays\PestPilotPointTest.Kx_2.arrays"
if exist "arrays\PestPilotPointTest.Kx_3.arrays" del "arrays\PestPilotPointTest.Kx_3.arrays"
if exist "arrays\PestPilotPointTest.Ky_1.arrays" del "arrays\PestPilotPointTest.Ky_1.arrays"
if exist "arrays\PestPilotPointTest.Ky_2.arrays" del "arrays\PestPilotPointTest.Ky_2.arrays"
if exist "arrays\PestPilotPointTest.Ky_3.arrays" del "arrays\PestPilotPointTest.Ky_3.arrays"
if exist "PestPilotPointTest.drn" del "PestPilotPointTest.drn"
if exist "PestPilotPointTest.Mf6Values" del "PestPilotPointTest.Mf6Values"
if exist "mfsim.lst" del "mfsim.lst"
if exist "PestPilotPointTest.bhd" del "PestPilotPointTest.bhd"
if exist "PestPilotPointTest.cbc" del "PestPilotPointTest.cbc"
if exist "PestPilotPointTest.drob_out_drn.csv" del "PestPilotPointTest.drob_out_drn.csv"
if exist "PestPilotPointTest.InnerSolution.CSV" del "PestPilotPointTest.InnerSolution.CSV"
if exist "PestPilotPointTest.lst" del "PestPilotPointTest.lst"
if exist "PestPilotPointTest.ob_gw_out_head.csv" del "PestPilotPointTest.ob_gw_out_head.csv"
if exist "PestPilotPointTest.OuterSolution.CSV" del "PestPilotPointTest.OuterSolution.CSV"
"plproc.exe" PestPilotPointTest.Kx.script
"plproc.exe" PestPilotPointTest.Ky.script
"EnhancedTemplateProcessor.exe" PestPilotPointTest.drn.tpl PestPilotPointTest.pval
mf6.exe
"Mf6ObsExtractor.exe" PestPilotPointTest.Mf6ExtractValues
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

