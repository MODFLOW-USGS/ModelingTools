@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\RmaSutraCompleted.Maximum_Hydraulic_Conductivity_1.arrays" del "arrays\RmaSutraCompleted.Maximum_Hydraulic_Conductivity_1.arrays"
if exist "arrays\RmaSutraCompleted.Minimum_Hydraulic_Conductivity_1.arrays" del "arrays\RmaSutraCompleted.Minimum_Hydraulic_Conductivity_1.arrays"
if exist "RmaSutraCompleted.15B" del "RmaSutraCompleted.15B"
if exist "RmaSutraCompleted.FluxBcs" del "RmaSutraCompleted.FluxBcs"
if exist "RmaSutraCompleted.inp" del "RmaSutraCompleted.inp"
if exist "RmaSutraCompleted.bcof" del "RmaSutraCompleted.bcof"
if exist "RmaSutraCompleted.bcop" del "RmaSutraCompleted.bcop"
if exist "RmaSutraCompleted.ele" del "RmaSutraCompleted.ele"
if exist "RmaSutraCompleted.lst" del "RmaSutraCompleted.lst"
if exist "RmaSutraCompleted.nod" del "RmaSutraCompleted.nod"
if exist "RmaSutraCompleted.rst" del "RmaSutraCompleted.rst"
if exist "RmaSutraCompleted.smy" del "RmaSutraCompleted.smy"
if exist "RmaSutraCompleted_Head_Obs1.OBC" del "RmaSutraCompleted_Head_Obs1.OBC"
if exist "RmaSutraCompleted_Head_Obs2.OBC" del "RmaSutraCompleted_Head_Obs2.OBC"
if exist "RmaSutraCompleted_Head_Obs3.OBC" del "RmaSutraCompleted_Head_Obs3.OBC"
if exist "RmaSutraCompleted_Head_Obs4.OBC" del "RmaSutraCompleted_Head_Obs4.OBC"
if exist "RmaSutraCompleted_Head_Obs5.OBC" del "RmaSutraCompleted_Head_Obs5.OBC"
if exist "RmaSutraCompleted_Head_Obs6.OBC" del "RmaSutraCompleted_Head_Obs6.OBC"
if exist "RmaSutraCompleted_Head_Obs7.OBC" del "RmaSutraCompleted_Head_Obs7.OBC"
if exist "RmaSutraCompleted_Head_Obs8.OBC" del "RmaSutraCompleted_Head_Obs8.OBC"
if exist "RmaSutraCompleted_TIME_STEPS.OBS" del "RmaSutraCompleted_TIME_STEPS.OBS"
"plproc.exe" RmaSutraCompleted.Maximum_Hydraulic_Conductivity.script
"EnhancedTemplateProcessor.exe" arrays\RmaSutraCompleted.Minimum_Hydraulic_Conductivity_1.arrays.tpl
"EnhancedTemplateProcessor.exe" RmaSutraCompleted.FluxBcs.tpl RmaSutraCompleted.pval
"EnhancedTemplateProcessor.exe" RmaSutraCompleted.inp.tpl RmaSutraCompleted.pval
"plproc.exe" 'RmaSutraCompleted.15B.script'
"sutra_2_2.exe"
"SutraObsExtractor.exe" RmaSutraCompleted.soe_ev
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

