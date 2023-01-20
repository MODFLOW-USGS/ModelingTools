@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "SutraTest2a.FluxBcs" del "SutraTest2a.FluxBcs"
if exist "SutraTest2a.inp" del "SutraTest2a.inp"
if exist "SutraTest2a.SPecPBcs" del "SutraTest2a.SPecPBcs"
if exist "SutraTest2a.bcof" del "SutraTest2a.bcof"
if exist "SutraTest2a.bcop" del "SutraTest2a.bcop"
if exist "SutraTest2a.bcos" del "SutraTest2a.bcos"
if exist "SutraTest2a.bcou" del "SutraTest2a.bcou"
if exist "SutraTest2a.ele" del "SutraTest2a.ele"
if exist "SutraTest2a.lst" del "SutraTest2a.lst"
if exist "SutraTest2a.nod" del "SutraTest2a.nod"
if exist "SutraTest2a.rst" del "SutraTest2a.rst"
if exist "SutraTest2a.smy" del "SutraTest2a.smy"
if exist "SutraTest2a_Object13.OBC" del "SutraTest2a_Object13.OBC"
if exist "SutraTest2a_Observatio.OBC" del "SutraTest2a_Observatio.OBC"
if exist "SutraTest2a_ObsSchec.OBS" del "SutraTest2a_ObsSchec.OBS"
"EnhancedTemplateProcessor.exe" SutraTest2a.FluxBcs.tpl SutraTest2a.pval
"EnhancedTemplateProcessor.exe" SutraTest2a.SPecPBcs.tpl SutraTest2a.pval
"EnhancedTemplateProcessor.exe" SutraTest2a.inp.tpl SutraTest2a.pval
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraTest2a.soe_ev
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

