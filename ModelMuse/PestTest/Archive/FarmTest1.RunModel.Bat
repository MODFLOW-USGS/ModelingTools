@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "FarmTest1.fmp" del "FarmTest1.fmp"
if exist "FarmTest1.Mf2005Values" del "FarmTest1.Mf2005Values"
if exist "FarmTest1.cbc" del "FarmTest1.cbc"
if exist "FarmTest1.fdn" del "FarmTest1.fdn"
if exist "FarmTest1.FDS_BIN" del "FarmTest1.FDS_BIN"
if exist "FarmTest1.fhd" del "FarmTest1.fhd"
if exist "FarmTest1.gbob_out" del "FarmTest1.gbob_out"
if exist "FarmTest1.lst" del "FarmTest1.lst"
if exist "FarmTest1.Swr_Obs_A" del "FarmTest1.Swr_Obs_A"
if exist "FarmTest1.Swr_ReachExchange_B" del "FarmTest1.Swr_ReachExchange_B"
if exist "FarmTest1.Swr_ReachGroupFlows_B" del "FarmTest1.Swr_ReachGroupFlows_B"
if exist "FarmTest1.Swr_ReachStage_B" del "FarmTest1.Swr_ReachStage_B"
if exist "FarmTest1.Swr_StructureFlow_A" del "FarmTest1.Swr_StructureFlow_A"
if exist "FarmTest1.Swr_TimeStepLength_A" del "FarmTest1.Swr_TimeStepLength_A"
if exist "FarmTest1.wbgt" del "FarmTest1.wbgt"
if exist "FarmTest11.uzfg" del "FarmTest11.uzfg"
if exist "FarmTest12.uzfg" del "FarmTest12.uzfg"
if exist "FarmTest13.uzfg" del "FarmTest13.uzfg"
if exist "FarmTest14.uzfg" del "FarmTest14.uzfg"
if exist "FarmTest15.uzfg" del "FarmTest15.uzfg"
if exist "OWHM_EX1.t.ByNode" del "OWHM_EX1.t.ByNode"
if exist "OWHM_EX1.t.Qsum" del "OWHM_EX1.t.Qsum"
if exist "OWHM_EX1.t.wl1" del "OWHM_EX1.t.wl1"
"EnhancedTemplateProcessor.exe" FarmTest1.fmp.tpl FarmTest1.pval
MF_OWHM_Win32.exe FarmTest1.nam /wait
"Mf2005ObsExtractor.exe" FarmTest1.Mf2005ExtractValues
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

