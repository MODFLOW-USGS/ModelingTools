@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "SutraTest1.FluxBcs" del "SutraTest1.FluxBcs"
if exist "SutraTest1.inp" del "SutraTest1.inp"
if exist "SutraTest1.SPecPBcs" del "SutraTest1.SPecPBcs"
if exist "SutraTest1.bcof" del "SutraTest1.bcof"
if exist "SutraTest1.bcop" del "SutraTest1.bcop"
if exist "SutraTest1.bcos" del "SutraTest1.bcos"
if exist "SutraTest1.bcou" del "SutraTest1.bcou"
if exist "SutraTest1.ele" del "SutraTest1.ele"
if exist "SutraTest1.lst" del "SutraTest1.lst"
if exist "SutraTest1.nod" del "SutraTest1.nod"
if exist "SutraTest1.rst" del "SutraTest1.rst"
if exist "SutraTest1.smy" del "SutraTest1.smy"
if exist "SutraTest1_FluidFlux1.OBC" del "SutraTest1_FluidFlux1.OBC"
if exist "SutraTest1_Observatio.OBC" del "SutraTest1_Observatio.OBC"
if exist "SutraTest1_ObsSched1.OBS" del "SutraTest1_ObsSched1.OBS"
if exist "SutraTest1_ObsSched2.OBS" del "SutraTest1_ObsSched2.OBS"
if exist "SutraTest1_SCHED_1.OBC" del "SutraTest1_SCHED_1.OBC"
if exist "SutraTest1_TIME_STEPS.OBC" del "SutraTest1_TIME_STEPS.OBC"
if exist "SutraTest1_TIME_STEPS.OBS" del "SutraTest1_TIME_STEPS.OBS"
"EnhancedTemplateProcessor.exe" SutraTest1.FluxBcs.tpl SutraTest1.pval
"EnhancedTemplateProcessor.exe" SutraTest1.SPecPBcs.tpl SutraTest1.pval
"EnhancedTemplateProcessor.exe" SutraTest1.inp.tpl SutraTest1.pval
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraTest1.soe_ev
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

