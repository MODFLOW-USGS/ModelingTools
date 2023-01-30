@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "SutraTest4.FluxBcs" del "SutraTest4.FluxBcs"
if exist "SutraTest4.inp" del "SutraTest4.inp"
if exist "SutraTest4.SPecPBcs" del "SutraTest4.SPecPBcs"
if exist "SutraTest4.bcof" del "SutraTest4.bcof"
if exist "SutraTest4.bcop" del "SutraTest4.bcop"
if exist "SutraTest4.bcos" del "SutraTest4.bcos"
if exist "SutraTest4.bcou" del "SutraTest4.bcou"
if exist "SutraTest4.ele" del "SutraTest4.ele"
if exist "SutraTest4.lst" del "SutraTest4.lst"
if exist "SutraTest4.nod" del "SutraTest4.nod"
if exist "SutraTest4.rst" del "SutraTest4.rst"
if exist "SutraTest4.smy" del "SutraTest4.smy"
if exist "SutraTest4_Object19.OBC" del "SutraTest4_Object19.OBC"
if exist "SutraTest4_Observatio.OBC" del "SutraTest4_Observatio.OBC"
if exist "SutraTest4_ObsSched1.OBS" del "SutraTest4_ObsSched1.OBS"
if exist "SutraTest4_ObsSched2.OBS" del "SutraTest4_ObsSched2.OBS"
if exist "SutraTest4_TIME_STEPS.OBC" del "SutraTest4_TIME_STEPS.OBC"
if exist "SutraTest4_TIME_STEPS.OBS" del "SutraTest4_TIME_STEPS.OBS"
"EnhancedTemplateProcessor.exe" SutraTest4.FluxBcs.tpl SutraTest4.pval
"EnhancedTemplateProcessor.exe" SutraTest4.inp.tpl SutraTest4.pval
"EnhancedTemplateProcessor.exe" SutraTest4.SPecPBcs.tpl SutraTest4.pval
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraTest4.soe_ev
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

