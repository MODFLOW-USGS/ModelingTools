@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "Prediction.chd" del "Prediction.chd"
if exist "Prediction.drn" del "Prediction.drn"
if exist "Prediction.evt" del "Prediction.evt"
if exist "Prediction.ghb" del "Prediction.ghb"
if exist "Prediction.Mf6Values" del "Prediction.Mf6Values"
if exist "Prediction.rch" del "Prediction.rch"
if exist "Prediction.riv" del "Prediction.riv"
if exist "Prediction.wel" del "Prediction.wel"
if exist "mfsim.lst" del "mfsim.lst"
if exist "Prediction.bhd" del "Prediction.bhd"
if exist "Prediction.cbc" del "Prediction.cbc"
if exist "Prediction.InnerSolution.CSV" del "Prediction.InnerSolution.CSV"
if exist "Prediction.lst" del "Prediction.lst"
if exist "Prediction.ob_gw_out_head.csv" del "Prediction.ob_gw_out_head.csv"
if exist "Prediction.OuterSolution.CSV" del "Prediction.OuterSolution.CSV"
"EnhancedTemplateProcessor.exe" Prediction.chd.tpl Prediction.pval
"EnhancedTemplateProcessor.exe" Prediction.ghb.tpl Prediction.pval
"EnhancedTemplateProcessor.exe" Prediction.wel.tpl Prediction.pval
"EnhancedTemplateProcessor.exe" Prediction.riv.tpl Prediction.pval
"EnhancedTemplateProcessor.exe" Prediction.drn.tpl Prediction.pval
"EnhancedTemplateProcessor.exe" Prediction.rch.tpl Prediction.pval
"EnhancedTemplateProcessor.exe" Prediction.evt.tpl Prediction.pval
mf6.exe
"Mf6ObsExtractor.exe" Prediction.Mf6ExtractValues
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

