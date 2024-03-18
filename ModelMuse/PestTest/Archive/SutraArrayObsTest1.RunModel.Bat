@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "SutraArrayObsTest1.UVEC" del "SutraArrayObsTest1.UVEC"
if exist "SutraArrayObsTest1.FluxBcs" del "SutraArrayObsTest1.FluxBcs"
if exist "SutraArrayObsTest1.inp" del "SutraArrayObsTest1.inp"
if exist "SutraArrayObsTest1.SPecPBcs" del "SutraArrayObsTest1.SPecPBcs"
if exist "SutraArrayObsTest1.bcof" del "SutraArrayObsTest1.bcof"
if exist "SutraArrayObsTest1.bcop" del "SutraArrayObsTest1.bcop"
if exist "SutraArrayObsTest1.bcos" del "SutraArrayObsTest1.bcos"
if exist "SutraArrayObsTest1.bcou" del "SutraArrayObsTest1.bcou"
if exist "SutraArrayObsTest1.ele" del "SutraArrayObsTest1.ele"
if exist "SutraArrayObsTest1.lst" del "SutraArrayObsTest1.lst"
if exist "SutraArrayObsTest1.nod" del "SutraArrayObsTest1.nod"
if exist "SutraArrayObsTest1.rst" del "SutraArrayObsTest1.rst"
if exist "SutraArrayObsTest1.smy" del "SutraArrayObsTest1.smy"
if exist "SutraArrayObsTest1_FluidFlux1.OBC" del "SutraArrayObsTest1_FluidFlux1.OBC"
if exist "SutraArrayObsTest1_Observatio.OBC" del "SutraArrayObsTest1_Observatio.OBC"
if exist "SutraArrayObsTest1_ObsSched1.OBS" del "SutraArrayObsTest1_ObsSched1.OBS"
if exist "SutraArrayObsTest1_ObsSched2.OBS" del "SutraArrayObsTest1_ObsSched2.OBS"
if exist "SutraArrayObsTest1_SCHED_1.OBC" del "SutraArrayObsTest1_SCHED_1.OBC"
if exist "SutraArrayObsTest1_TIME_STEPS.OBC" del "SutraArrayObsTest1_TIME_STEPS.OBC"
if exist "SutraArrayObsTest1_TIME_STEPS.OBS" del "SutraArrayObsTest1_TIME_STEPS.OBS"
if exist "SutraArrayObsTest1.SUTRA.FIL" copy /Y "SutraArrayObsTest1.SUTRA.FIL" "SUTRA.FIL"
"plproc.exe" SutraArrayObsTest1.InitialConcentration.script
"EnhancedTemplateProcessor.exe" SutraArrayObsTest1.FluxBcs.tpl SutraArrayObsTest1.pval
"EnhancedTemplateProcessor.exe" SutraArrayObsTest1.inp.tpl SutraArrayObsTest1.pval
"EnhancedTemplateProcessor.exe" SutraArrayObsTest1.SPecPBcs.tpl SutraArrayObsTest1.pval
"plproc.exe" 'SutraArrayObsTest1.InitialConcentration.script'

GetValuesFromArrayFiles.exe C:\ModelingTools\ModelMuse\PestTest\SutraArrayObsTest1.InitialConcentration_1.ExtractIns
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraArrayObsTest1.soe_ev
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

