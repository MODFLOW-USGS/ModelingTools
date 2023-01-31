@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\SutraTest5.Maximum_Permeability_1.arrays" del "arrays\SutraTest5.Maximum_Permeability_1.arrays"
if exist "arrays\SutraTest5.Maximum_Permeability_2.arrays" del "arrays\SutraTest5.Maximum_Permeability_2.arrays"
if exist "arrays\SutraTest5.Maximum_Permeability_3.arrays" del "arrays\SutraTest5.Maximum_Permeability_3.arrays"
if exist "arrays\SutraTest5.Middle_Permeability_1.arrays" del "arrays\SutraTest5.Middle_Permeability_1.arrays"
if exist "arrays\SutraTest5.Middle_Permeability_2.arrays" del "arrays\SutraTest5.Middle_Permeability_2.arrays"
if exist "arrays\SutraTest5.Middle_Permeability_3.arrays" del "arrays\SutraTest5.Middle_Permeability_3.arrays"
if exist "arrays\SutraTest5.Minimum_Permeability_1.arrays" del "arrays\SutraTest5.Minimum_Permeability_1.arrays"
if exist "arrays\SutraTest5.Minimum_Permeability_2.arrays" del "arrays\SutraTest5.Minimum_Permeability_2.arrays"
if exist "arrays\SutraTest5.Minimum_Permeability_3.arrays" del "arrays\SutraTest5.Minimum_Permeability_3.arrays"
if exist "SutraTest5.15B_1" del "SutraTest5.15B_1"
if exist "SutraTest5.15B_2" del "SutraTest5.15B_2"
if exist "SutraTest5.15B_3" del "SutraTest5.15B_3"
if exist "SutraTest5.bcof" del "SutraTest5.bcof"
if exist "SutraTest5.bcop" del "SutraTest5.bcop"
if exist "SutraTest5.bcos" del "SutraTest5.bcos"
if exist "SutraTest5.bcou" del "SutraTest5.bcou"
if exist "SutraTest5.ele" del "SutraTest5.ele"
if exist "SutraTest5.lst" del "SutraTest5.lst"
if exist "SutraTest5.nod" del "SutraTest5.nod"
if exist "SutraTest5.rst" del "SutraTest5.rst"
if exist "SutraTest5.smy" del "SutraTest5.smy"
if exist "SutraTest5_Observatio.OBC" del "SutraTest5_Observatio.OBC"
if exist "SutraTest5_ObsSchec.OBS" del "SutraTest5_ObsSchec.OBS"
if exist "SutraTest5.SUTRA.FIL" copy /Y "SutraTest5.SUTRA.FIL" "SUTRA.FIL"
"plproc.exe" SutraTest5.Maximum_Permeability.script
"plproc.exe" SutraTest5.Minimum_Permeability.script
"plproc.exe" SutraTest5.Middle_Permeability.script
"EnhancedTemplateProcessor.exe" arrays\SutraTest5.Middle_Permeability_1.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\SutraTest5.Middle_Permeability_2.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\SutraTest5.Middle_Permeability_3.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\SutraTest5.Minimum_Permeability_1.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\SutraTest5.Minimum_Permeability_2.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\SutraTest5.Minimum_Permeability_3.arrays.tpl
"plproc.exe" 'SutraTest5.15B.script'
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraTest5.soe_ev
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

