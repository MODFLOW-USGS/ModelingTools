@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "HfbMf6Test2.chd" del "HfbMf6Test2.chd"
if exist "HfbMf6Test2.hfb" del "HfbMf6Test2.hfb"
if exist "HfbMf6Test2.Mf6Values" del "HfbMf6Test2.Mf6Values"
if exist "HfbMf6Test2.bhd" del "HfbMf6Test2.bhd"
if exist "HfbMf6Test2.cbc" del "HfbMf6Test2.cbc"
if exist "HfbMf6Test2.InnerSolution.CSV" del "HfbMf6Test2.InnerSolution.CSV"
if exist "HfbMf6Test2.lst" del "HfbMf6Test2.lst"
if exist "HfbMf6Test2.ob_gw_out_head.csv" del "HfbMf6Test2.ob_gw_out_head.csv"
if exist "HfbMf6Test2.OuterSolution.CSV" del "HfbMf6Test2.OuterSolution.CSV"
if exist "mfsim.lst" del "mfsim.lst"
"EnhancedTemplateProcessor.exe" HfbMf6Test2.chd.tpl HfbMf6Test2.pval
"EnhancedTemplateProcessor.exe" HfbMf6Test2.hfb.tpl HfbMf6Test2.pval
mf6.exe
"Mf6ObsExtractor.exe" HfbMf6Test2.Mf6ExtractValues
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

