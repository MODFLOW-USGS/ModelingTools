@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "SutraTest2.FluxBcs" del "SutraTest2.FluxBcs"
if exist "SutraTest2.inp" del "SutraTest2.inp"
if exist "SutraTest2.SPecPBcs" del "SutraTest2.SPecPBcs"
if exist "SutraTest2.SPecUBcs" del "SutraTest2.SPecUBcs"
if exist "SutraTest2.UFluxBcs" del "SutraTest2.UFluxBcs"
if exist "SutraTest2.bcof" del "SutraTest2.bcof"
if exist "SutraTest2.bcop" del "SutraTest2.bcop"
if exist "SutraTest2.bcos" del "SutraTest2.bcos"
if exist "SutraTest2.bcou" del "SutraTest2.bcou"
if exist "SutraTest2.ele" del "SutraTest2.ele"
if exist "SutraTest2.lst" del "SutraTest2.lst"
if exist "SutraTest2.nod" del "SutraTest2.nod"
if exist "SutraTest2.rst" del "SutraTest2.rst"
if exist "SutraTest2.smy" del "SutraTest2.smy"
if exist "SutraTest2_Object13.OBC" del "SutraTest2_Object13.OBC"
if exist "SutraTest2_Observatio.OBC" del "SutraTest2_Observatio.OBC"
if exist "SutraTest2_ObsSchec.OBS" del "SutraTest2_ObsSchec.OBS"
"EnhancedTemplateProcessor.exe" SutraTest2.FluxBcs.tpl SutraTest2.pval
"EnhancedTemplateProcessor.exe" SutraTest2.UFluxBcs.tpl SutraTest2.pval
"EnhancedTemplateProcessor.exe" SutraTest2.SPecPBcs.tpl SutraTest2.pval
"EnhancedTemplateProcessor.exe" SutraTest2.SPecUBcs.tpl SutraTest2.pval
"EnhancedTemplateProcessor.exe" SutraTest2.inp.tpl SutraTest2.pval
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraTest2.soe_ev
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

