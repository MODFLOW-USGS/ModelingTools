@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\importedSWR.MyVertOffset_1.arrays" del "arrays\importedSWR.MyVertOffset_1.arrays"
if exist "importedSWR.Mf2005Values" del "importedSWR.Mf2005Values"
if exist "importedSWR.swr" del "importedSWR.swr"
if exist "importedSWR.cbc" del "importedSWR.cbc"
if exist "importedSWR.fdn" del "importedSWR.fdn"
if exist "importedSWR.fhd" del "importedSWR.fhd"
if exist "importedSWR.hob_out" del "importedSWR.hob_out"
if exist "importedSWR.lst" del "importedSWR.lst"
if exist "importedSWR.Swr_Convergence" del "importedSWR.Swr_Convergence"
if exist "importedSWR.Swr_Obs_A" del "importedSWR.Swr_Obs_A"
if exist "importedSWR.Swr_ReachExchange_B" del "importedSWR.Swr_ReachExchange_B"
if exist "importedSWR.Swr_ReachGroupFlows_B" del "importedSWR.Swr_ReachGroupFlows_B"
if exist "importedSWR.Swr_ReachStage_A" del "importedSWR.Swr_ReachStage_A"
if exist "importedSWR.Swr_RIV" del "importedSWR.Swr_RIV"
"plproc.exe" importedSWR.MyVertOffset.script
"EnhancedTemplateProcessor.exe" importedSWR.swr.tpl importedSWR.pval
MODFLOW-NWT.exe importedSWR.nam /wait
"Mf2005ObsExtractor.exe" importedSWR.Mf2005ExtractValues
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

