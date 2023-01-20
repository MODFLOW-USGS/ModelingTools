@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "Pareto.chd" del "Pareto.chd"
if exist "Pareto.drn" del "Pareto.drn"
if exist "Pareto.evt" del "Pareto.evt"
if exist "Pareto.ghb" del "Pareto.ghb"
if exist "Pareto.Mf6Values" del "Pareto.Mf6Values"
if exist "Pareto.rch" del "Pareto.rch"
if exist "Pareto.riv" del "Pareto.riv"
if exist "Pareto.wel" del "Pareto.wel"
if exist "mfsim.lst" del "mfsim.lst"
if exist "Pareto.bhd" del "Pareto.bhd"
if exist "Pareto.cbc" del "Pareto.cbc"
if exist "Pareto.InnerSolution.CSV" del "Pareto.InnerSolution.CSV"
if exist "Pareto.lst" del "Pareto.lst"
if exist "Pareto.ob_gw_out_head.csv" del "Pareto.ob_gw_out_head.csv"
if exist "Pareto.OuterSolution.CSV" del "Pareto.OuterSolution.CSV"
"EnhancedTemplateProcessor.exe" Pareto.chd.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.ghb.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.wel.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.riv.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.drn.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.rch.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.evt.tpl Pareto.pval
mf6.exe
"Mf6ObsExtractor.exe" Pareto.Mf6ExtractValues
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

