@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "FmpDialogs.added_crop_demand_flux" del "FmpDialogs.added_crop_demand_flux"
if exist "FmpDialogs.added_crop_demand_rate" del "FmpDialogs.added_crop_demand_rate"
if exist "FmpDialogs.capillary_fringe" del "FmpDialogs.capillary_fringe"
if exist "FmpDialogs.coefficient" del "FmpDialogs.coefficient"
if exist "FmpDialogs.Mf2005Values" del "FmpDialogs.Mf2005Values"
if exist "FmpDialogs.non_routed_delivery" del "FmpDialogs.non_routed_delivery"
if exist "FmpDialogs.surface_vertical_hydraulic_conductivity" del "FmpDialogs.surface_vertical_hydraulic_conductivity"
if exist "FmpDialogs.cbc" del "FmpDialogs.cbc"
if exist "FmpDialogs.Crop_Input" del "FmpDialogs.Crop_Input"
if exist "FmpDialogs.fdn" del "FmpDialogs.fdn"
if exist "FmpDialogs.fhd" del "FmpDialogs.fhd"
if exist "FmpDialogs.lst" del "FmpDialogs.lst"
if exist "FmpDialogs.Salinity_Flush_Input" del "FmpDialogs.Salinity_Flush_Input"
if exist "FmpDialogs.wbgt" del "FmpDialogs.wbgt"
if exist "FmpDialogs.Well_Data_Input" del "FmpDialogs.Well_Data_Input"
"EnhancedTemplateProcessor.exe" FmpDialogs.added_crop_demand_flux.tpl FmpDialogs.pval
"EnhancedTemplateProcessor.exe" FmpDialogs.added_crop_demand_rate.tpl FmpDialogs.pval
"EnhancedTemplateProcessor.exe" FmpDialogs.capillary_fringe.tpl FmpDialogs.pval
"EnhancedTemplateProcessor.exe" FmpDialogs.coefficient.tpl FmpDialogs.pval
"EnhancedTemplateProcessor.exe" FmpDialogs.non_routed_delivery.tpl FmpDialogs.pval
"EnhancedTemplateProcessor.exe" FmpDialogs.surface_vertical_hydraulic_conductivity.tpl FmpDialogs.pval
mf-owhm.exe FmpDialogs.nam /wait
"Mf2005ObsExtractor.exe" FmpDialogs.Mf2005ExtractValues
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

