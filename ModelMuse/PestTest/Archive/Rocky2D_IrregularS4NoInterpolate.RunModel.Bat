@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\Rocky2D_IrregularS4NoInterpolate.Maximum_Permeability_1.arrays" del "arrays\Rocky2D_IrregularS4NoInterpolate.Maximum_Permeability_1.arrays"
if exist "arrays\Rocky2D_IrregularS4NoInterpolate.Maximum_Permeability_2.arrays" del "arrays\Rocky2D_IrregularS4NoInterpolate.Maximum_Permeability_2.arrays"
if exist "arrays\Rocky2D_IrregularS4NoInterpolate.Middle_Permeability_1.arrays" del "arrays\Rocky2D_IrregularS4NoInterpolate.Middle_Permeability_1.arrays"
if exist "arrays\Rocky2D_IrregularS4NoInterpolate.Middle_Permeability_2.arrays" del "arrays\Rocky2D_IrregularS4NoInterpolate.Middle_Permeability_2.arrays"
if exist "arrays\Rocky2D_IrregularS4NoInterpolate.Minimum_Permeability_1.arrays" del "arrays\Rocky2D_IrregularS4NoInterpolate.Minimum_Permeability_1.arrays"
if exist "arrays\Rocky2D_IrregularS4NoInterpolate.Minimum_Permeability_2.arrays" del "arrays\Rocky2D_IrregularS4NoInterpolate.Minimum_Permeability_2.arrays"
if exist "arrays\Rocky2D_IrregularS4NoInterpolate.Scaled_Effective_Air_Thermal_Conductivity_1.arrays" del "arrays\Rocky2D_IrregularS4NoInterpolate.Scaled_Effective_Air_Thermal_Conductivity_1.arrays"
if exist "arrays\Rocky2D_IrregularS4NoInterpolate.Scaled_Effective_Air_Thermal_Conductivity_2.arrays" del "arrays\Rocky2D_IrregularS4NoInterpolate.Scaled_Effective_Air_Thermal_Conductivity_2.arrays"
if exist "Rocky2D_IrregularS4NoInterpolate.14B_1" del "Rocky2D_IrregularS4NoInterpolate.14B_1"
if exist "Rocky2D_IrregularS4NoInterpolate.14B_2" del "Rocky2D_IrregularS4NoInterpolate.14B_2"
if exist "Rocky2D_IrregularS4NoInterpolate.14B_3" del "Rocky2D_IrregularS4NoInterpolate.14B_3"
if exist "Rocky2D_IrregularS4NoInterpolate.15B_1" del "Rocky2D_IrregularS4NoInterpolate.15B_1"
if exist "Rocky2D_IrregularS4NoInterpolate.15B_2" del "Rocky2D_IrregularS4NoInterpolate.15B_2"
if exist "Rocky2D_IrregularS4NoInterpolate.bcof" del "Rocky2D_IrregularS4NoInterpolate.bcof"
if exist "Rocky2D_IrregularS4NoInterpolate.bcop" del "Rocky2D_IrregularS4NoInterpolate.bcop"
if exist "Rocky2D_IrregularS4NoInterpolate.ele" del "Rocky2D_IrregularS4NoInterpolate.ele"
if exist "Rocky2D_IrregularS4NoInterpolate.lst" del "Rocky2D_IrregularS4NoInterpolate.lst"
if exist "Rocky2D_IrregularS4NoInterpolate.nod" del "Rocky2D_IrregularS4NoInterpolate.nod"
if exist "Rocky2D_IrregularS4NoInterpolate.rst" del "Rocky2D_IrregularS4NoInterpolate.rst"
if exist "Rocky2D_IrregularS4NoInterpolate.smy" del "Rocky2D_IrregularS4NoInterpolate.smy"
if exist "Rocky2D_IrregularS4NoInterpolate.SUTRA.FIL" copy /Y "Rocky2D_IrregularS4NoInterpolate.SUTRA.FIL" "SUTRA.FIL"
"plproc.exe" Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity.script
"plproc.exe" Rocky2D_IrregularS4NoInterpolate.SolidMatrixCompressibility.script
"plproc.exe" Rocky2D_IrregularS4NoInterpolate.Maximum_Permeability.script
"plproc.exe" Rocky2D_IrregularS4NoInterpolate.SolidGrain_SpecificHeat.script
"plproc.exe" Rocky2D_IrregularS4NoInterpolate.ScaledEffectiveAirThermalConductivity.script
"plproc.exe" Rocky2D_IrregularS4NoInterpolate.Scaled_Effective_Air_Thermal_Conductivity.script
"EnhancedTemplateProcessor.exe" arrays\Rocky2D_IrregularS4NoInterpolate.Middle_Permeability_1.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\Rocky2D_IrregularS4NoInterpolate.Middle_Permeability_2.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\Rocky2D_IrregularS4NoInterpolate.Minimum_Permeability_1.arrays.tpl
"EnhancedTemplateProcessor.exe" arrays\Rocky2D_IrregularS4NoInterpolate.Minimum_Permeability_2.arrays.tpl
"plproc.exe" 'Rocky2D_IrregularS4NoInterpolate.14B.script'
"plproc.exe" 'Rocky2D_IrregularS4NoInterpolate.15B.script'
"sutra_4_0_0_PROVISIONAL_2023-02-28_8b569f6.exe"
"SutraObsExtractor.exe" Rocky2D_IrregularS4NoInterpolate.soe_ev
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

