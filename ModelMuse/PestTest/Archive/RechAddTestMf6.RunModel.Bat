@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "RechAddTestMf6.chd" del "RechAddTestMf6.chd"
if exist "RechAddTestMf6.Mf6Values" del "RechAddTestMf6.Mf6Values"
if exist "RechAddTestMf6.rch" del "RechAddTestMf6.rch"
if exist "mfsim.lst" del "mfsim.lst"
if exist "RechAddTestMf6.bhd" del "RechAddTestMf6.bhd"
if exist "RechAddTestMf6.cbc" del "RechAddTestMf6.cbc"
if exist "RechAddTestMf6.InnerSolution.CSV" del "RechAddTestMf6.InnerSolution.CSV"
if exist "RechAddTestMf6.lst" del "RechAddTestMf6.lst"
if exist "RechAddTestMf6.ob_gw_out_head.csv" del "RechAddTestMf6.ob_gw_out_head.csv"
if exist "RechAddTestMf6.OuterSolution.CSV" del "RechAddTestMf6.OuterSolution.CSV"
"EnhancedTemplateProcessor.exe" RechAddTestMf6.chd.tpl RechAddTestMf6.pval
"EnhancedTemplateProcessor.exe" RechAddTestMf6.rch.tpl RechAddTestMf6.pval
mf6.exe
"Mf6ObsExtractor.exe" RechAddTestMf6.Mf6ExtractValues
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

