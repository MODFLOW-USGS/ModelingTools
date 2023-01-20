@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\RmaSutraNodeTest.Maximum_Hydraulic_Conductivity_1.arrays" del "arrays\RmaSutraNodeTest.Maximum_Hydraulic_Conductivity_1.arrays"
if exist "arrays\RmaSutraNodeTest.Minimum_Hydraulic_Conductivity_1.arrays" del "arrays\RmaSutraNodeTest.Minimum_Hydraulic_Conductivity_1.arrays"
if exist "RmaSutraNodeTest.15B" del "RmaSutraNodeTest.15B"
if exist "RmaSutraNodeTest.FluxBcs" del "RmaSutraNodeTest.FluxBcs"
if exist "RmaSutraNodeTest.inp" del "RmaSutraNodeTest.inp"
if exist "RmaSutraNodeTest.bcof" del "RmaSutraNodeTest.bcof"
if exist "RmaSutraNodeTest.bcop" del "RmaSutraNodeTest.bcop"
if exist "RmaSutraNodeTest.ele" del "RmaSutraNodeTest.ele"
if exist "RmaSutraNodeTest.lst" del "RmaSutraNodeTest.lst"
if exist "RmaSutraNodeTest.nod" del "RmaSutraNodeTest.nod"
if exist "RmaSutraNodeTest.rst" del "RmaSutraNodeTest.rst"
if exist "RmaSutraNodeTest.smy" del "RmaSutraNodeTest.smy"
if exist "RmaSutraNodeTest_Head_Obs1.OBC" del "RmaSutraNodeTest_Head_Obs1.OBC"
if exist "RmaSutraNodeTest_Head_Obs2.OBC" del "RmaSutraNodeTest_Head_Obs2.OBC"
if exist "RmaSutraNodeTest_Head_Obs3.OBC" del "RmaSutraNodeTest_Head_Obs3.OBC"
if exist "RmaSutraNodeTest_Head_Obs4.OBC" del "RmaSutraNodeTest_Head_Obs4.OBC"
if exist "RmaSutraNodeTest_Head_Obs5.OBC" del "RmaSutraNodeTest_Head_Obs5.OBC"
if exist "RmaSutraNodeTest_Head_Obs6.OBC" del "RmaSutraNodeTest_Head_Obs6.OBC"
if exist "RmaSutraNodeTest_Head_Obs7.OBC" del "RmaSutraNodeTest_Head_Obs7.OBC"
if exist "RmaSutraNodeTest_Head_Obs8.OBC" del "RmaSutraNodeTest_Head_Obs8.OBC"
"plproc.exe" RmaSutraNodeTest.Maximum_Hydraulic_Conductivity.script
"EnhancedTemplateProcessor.exe" RmaSutraNodeTest.FluxBcs.tpl RmaSutraNodeTest.pval
"EnhancedTemplateProcessor.exe" arrays\RmaSutraNodeTest.Minimum_Hydraulic_Conductivity_1.arrays.tpl
"plproc.exe" 'RmaSutraNodeTest.15B.script'
"EnhancedTemplateProcessor.exe" RmaSutraNodeTest.inp.tpl RmaSutraNodeTest.pval
"sutra_3_0.exe"
"SutraObsExtractor.exe" RmaSutraNodeTest.soe_ev
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

