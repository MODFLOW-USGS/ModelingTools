@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\SutraTest1a.Maximum_Permeability_1.arrays" del "arrays\SutraTest1a.Maximum_Permeability_1.arrays"
if exist "arrays\SutraTest1a.Maximum_Permeability_2.arrays" del "arrays\SutraTest1a.Maximum_Permeability_2.arrays"
if exist "arrays\SutraTest1a.Maximum_Permeability_3.arrays" del "arrays\SutraTest1a.Maximum_Permeability_3.arrays"
if exist "arrays\SutraTest1a.Middle_Permeability_1.arrays" del "arrays\SutraTest1a.Middle_Permeability_1.arrays"
if exist "arrays\SutraTest1a.Middle_Permeability_2.arrays" del "arrays\SutraTest1a.Middle_Permeability_2.arrays"
if exist "arrays\SutraTest1a.Middle_Permeability_3.arrays" del "arrays\SutraTest1a.Middle_Permeability_3.arrays"
if exist "arrays\SutraTest1a.Minimum_Permeability_1.arrays" del "arrays\SutraTest1a.Minimum_Permeability_1.arrays"
if exist "arrays\SutraTest1a.Minimum_Permeability_2.arrays" del "arrays\SutraTest1a.Minimum_Permeability_2.arrays"
if exist "arrays\SutraTest1a.Minimum_Permeability_3.arrays" del "arrays\SutraTest1a.Minimum_Permeability_3.arrays"
if exist "arrays\SutraTest1a.MyMult_1.arrays" del "arrays\SutraTest1a.MyMult_1.arrays"
if exist "arrays\SutraTest1a.MyMult_2.arrays" del "arrays\SutraTest1a.MyMult_2.arrays"
if exist "arrays\SutraTest1a.MyMult_3.arrays" del "arrays\SutraTest1a.MyMult_3.arrays"
if exist "arrays\SutraTest1a.MyMult_4.arrays" del "arrays\SutraTest1a.MyMult_4.arrays"
if exist "SutraTest1a.14B_1" del "SutraTest1a.14B_1"
if exist "SutraTest1a.14B_2" del "SutraTest1a.14B_2"
if exist "SutraTest1a.14B_3" del "SutraTest1a.14B_3"
if exist "SutraTest1a.14B_4" del "SutraTest1a.14B_4"
if exist "SutraTest1a.15B_1" del "SutraTest1a.15B_1"
if exist "SutraTest1a.15B_2" del "SutraTest1a.15B_2"
if exist "SutraTest1a.15B_3" del "SutraTest1a.15B_3"
if exist "SutraTest1a.FluxBcs" del "SutraTest1a.FluxBcs"
if exist "SutraTest1a.inp" del "SutraTest1a.inp"
if exist "SutraTest1a.SPecPBcs" del "SutraTest1a.SPecPBcs"
if exist "SutraTest1a.bcof" del "SutraTest1a.bcof"
if exist "SutraTest1a.bcop" del "SutraTest1a.bcop"
if exist "SutraTest1a.bcos" del "SutraTest1a.bcos"
if exist "SutraTest1a.bcou" del "SutraTest1a.bcou"
if exist "SutraTest1a.ele" del "SutraTest1a.ele"
if exist "SutraTest1a.lst" del "SutraTest1a.lst"
if exist "SutraTest1a.nod" del "SutraTest1a.nod"
if exist "SutraTest1a.rst" del "SutraTest1a.rst"
if exist "SutraTest1a.smy" del "SutraTest1a.smy"
if exist "SutraTest1a_FluidFlux1.OBC" del "SutraTest1a_FluidFlux1.OBC"
if exist "SutraTest1a_Observatio.OBC" del "SutraTest1a_Observatio.OBC"
if exist "SutraTest1a_ObsSched1.OBS" del "SutraTest1a_ObsSched1.OBS"
if exist "SutraTest1a_ObsSched2.OBS" del "SutraTest1a_ObsSched2.OBS"
if exist "SutraTest1a_SCHED_1.OBC" del "SutraTest1a_SCHED_1.OBC"
if exist "SutraTest1a_TIME_STEPS.OBC" del "SutraTest1a_TIME_STEPS.OBC"
if exist "SutraTest1a_TIME_STEPS.OBS" del "SutraTest1a_TIME_STEPS.OBS"
if exist "SutraTest1a.SUTRA.FIL" copy /Y "SutraTest1a.SUTRA.FIL" "SUTRA.FIL"
"plproc.exe" SutraTest1a.Nodal_Porosity.script
"plproc.exe" SutraTest1a.Maximum_Permeability.script
"plproc.exe" SutraTest1a.MyMult.script
"EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Middle_Permeability_1.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Middle_Permeability_2.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Middle_Permeability_3.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Minimum_Permeability_1.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Minimum_Permeability_2.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Minimum_Permeability_3.arrays.tpl
"EnhancedTemplateProcessor.exe" SutraTest1a.FluxBcs.tpl SutraTest1a.pval
"EnhancedTemplateProcessor.exe" SutraTest1a.inp.tpl SutraTest1a.pval
"EnhancedTemplateProcessor.exe" SutraTest1a.SPecPBcs.tpl SutraTest1a.pval
"plproc.exe" 'SutraTest1a.14B.script'
"plproc.exe" 'SutraTest1a.15B.script'
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraTest1a.soe_ev
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

