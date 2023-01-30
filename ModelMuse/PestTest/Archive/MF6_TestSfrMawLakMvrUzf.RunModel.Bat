@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\MF6_TestSfrMawLakMvrUzf.SfrWidth_1.arrays" del "arrays\MF6_TestSfrMawLakMvrUzf.SfrWidth_1.arrays"
if exist "arrays\MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth_1.arrays" del "arrays\MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth_1.arrays"
if exist "arrays\MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth_2.arrays" del "arrays\MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth_2.arrays"
if exist "arrays\MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth_3.arrays" del "arrays\MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth_3.arrays"
if exist "MF6_TestSfrMawLakMvrUzf.3.laktab" del "MF6_TestSfrMawLakMvrUzf.3.laktab"
if exist "MF6_TestSfrMawLakMvrUzf.3.laktab" del "MF6_TestSfrMawLakMvrUzf.3.laktab"
if exist "MF6_TestSfrMawLakMvrUzf.lak6" del "MF6_TestSfrMawLakMvrUzf.lak6"
if exist "MF6_TestSfrMawLakMvrUzf.maw6" del "MF6_TestSfrMawLakMvrUzf.maw6"
if exist "MF6_TestSfrMawLakMvrUzf.Mf6Values" del "MF6_TestSfrMawLakMvrUzf.Mf6Values"
if exist "MF6_TestSfrMawLakMvrUzf.sfr" del "MF6_TestSfrMawLakMvrUzf.sfr"
if exist "MF6_TestSfrMawLakMvrUzf.uzf" del "MF6_TestSfrMawLakMvrUzf.uzf"
if exist "MF6_TestSfrMawLakMvrUzf.bhd" del "MF6_TestSfrMawLakMvrUzf.bhd"
if exist "MF6_TestSfrMawLakMvrUzf.cbc" del "MF6_TestSfrMawLakMvrUzf.cbc"
if exist "MF6_TestSfrMawLakMvrUzf.chob_out_chd.csv" del "MF6_TestSfrMawLakMvrUzf.chob_out_chd.csv"
if exist "MF6_TestSfrMawLakMvrUzf.drob_out_drn.csv" del "MF6_TestSfrMawLakMvrUzf.drob_out_drn.csv"
if exist "MF6_TestSfrMawLakMvrUzf.from-mvr_ob.csv" del "MF6_TestSfrMawLakMvrUzf.from-mvr_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.gbob_out_ghb.csv" del "MF6_TestSfrMawLakMvrUzf.gbob_out_ghb.csv"
if exist "MF6_TestSfrMawLakMvrUzf.infiltration_ob.csv" del "MF6_TestSfrMawLakMvrUzf.infiltration_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.InnerSolution.CSV" del "MF6_TestSfrMawLakMvrUzf.InnerSolution.CSV"
if exist "MF6_TestSfrMawLakMvrUzf.lak_conductance_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_conductance_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_constant_flow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_constant_flow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_evaporation_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_evaporation_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_ext-inflow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_ext-inflow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_ext-outflow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_ext-outflow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_flow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_flow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_from_MVR_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_from_MVR_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_inflow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_inflow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_outlet_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_outlet_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_outlet-inflow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_outlet-inflow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_rainfall_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_rainfall_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_runoff_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_runoff_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_stage_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_stage_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_storage_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_storage_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_surface-area_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_surface-area_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_to_mvr_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_to_mvr_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_volume_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_volume_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_wetted-area_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_wetted-area_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.lak_withdrawal_ob.csv" del "MF6_TestSfrMawLakMvrUzf.lak_withdrawal_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.LakConvergence.CSV" del "MF6_TestSfrMawLakMvrUzf.LakConvergence.CSV"
if exist "MF6_TestSfrMawLakMvrUzf.lk_bud" del "MF6_TestSfrMawLakMvrUzf.lk_bud"
if exist "MF6_TestSfrMawLakMvrUzf.lk_stg" del "MF6_TestSfrMawLakMvrUzf.lk_stg"
if exist "MF6_TestSfrMawLakMvrUzf.lst" del "MF6_TestSfrMawLakMvrUzf.lst"
if exist "MF6_TestSfrMawLakMvrUzf.maw_bud" del "MF6_TestSfrMawLakMvrUzf.maw_bud"
if exist "MF6_TestSfrMawLakMvrUzf.maw_cell_conductance_ob.csv" del "MF6_TestSfrMawLakMvrUzf.maw_cell_conductance_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.maw_cell_flow_rate_ob.csv" del "MF6_TestSfrMawLakMvrUzf.maw_cell_flow_rate_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.maw_flow_rate_ob.csv" del "MF6_TestSfrMawLakMvrUzf.maw_flow_rate_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.maw_flowing_well_conductance_ob.csv" del "MF6_TestSfrMawLakMvrUzf.maw_flowing_well_conductance_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.maw_from_mvr_ob.csv" del "MF6_TestSfrMawLakMvrUzf.maw_from_mvr_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.maw_head" del "MF6_TestSfrMawLakMvrUzf.maw_head"
if exist "MF6_TestSfrMawLakMvrUzf.maw_head_ob.csv" del "MF6_TestSfrMawLakMvrUzf.maw_head_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.maw_pump_rate_ob.csv" del "MF6_TestSfrMawLakMvrUzf.maw_pump_rate_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.maw_pump_rate_to_mvr_ob.csv" del "MF6_TestSfrMawLakMvrUzf.maw_pump_rate_to_mvr_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.mvr_budget" del "MF6_TestSfrMawLakMvrUzf.mvr_budget"
if exist "MF6_TestSfrMawLakMvrUzf.mvr_budget.csv" del "MF6_TestSfrMawLakMvrUzf.mvr_budget.csv"
if exist "MF6_TestSfrMawLakMvrUzf.net-infiltration_ob.csv" del "MF6_TestSfrMawLakMvrUzf.net-infiltration_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.ob_evt_out_evt.csv" del "MF6_TestSfrMawLakMvrUzf.ob_evt_out_evt.csv"
if exist "MF6_TestSfrMawLakMvrUzf.ob_gw_out_drawdown.csv" del "MF6_TestSfrMawLakMvrUzf.ob_gw_out_drawdown.csv"
if exist "MF6_TestSfrMawLakMvrUzf.ob_gw_out_flow-ja-face.csv" del "MF6_TestSfrMawLakMvrUzf.ob_gw_out_flow-ja-face.csv"
if exist "MF6_TestSfrMawLakMvrUzf.ob_gw_out_head.csv" del "MF6_TestSfrMawLakMvrUzf.ob_gw_out_head.csv"
if exist "MF6_TestSfrMawLakMvrUzf.ob_rch_out_rch.csv" del "MF6_TestSfrMawLakMvrUzf.ob_rch_out_rch.csv"
if exist "MF6_TestSfrMawLakMvrUzf.ob_wel_out_wel.csv" del "MF6_TestSfrMawLakMvrUzf.ob_wel_out_wel.csv"
if exist "MF6_TestSfrMawLakMvrUzf.OuterSolution.CSV" del "MF6_TestSfrMawLakMvrUzf.OuterSolution.CSV"
if exist "MF6_TestSfrMawLakMvrUzf.rej-inf_ob.csv" del "MF6_TestSfrMawLakMvrUzf.rej-inf_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.rej-inf-to-mvr_ob.csv" del "MF6_TestSfrMawLakMvrUzf.rej-inf-to-mvr_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.rvob_out_riv.csv" del "MF6_TestSfrMawLakMvrUzf.rvob_out_riv.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_budget" del "MF6_TestSfrMawLakMvrUzf.sfr_budget"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_downstream-flow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_downstream-flow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_evaporation_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_evaporation_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_ext-inflow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_ext-inflow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_ext-outflow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_ext-outflow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_from_mvr_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_from_mvr_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_gw_exchange_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_gw_exchange_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_inflow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_inflow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_outflow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_outflow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_rainfall_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_rainfall_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_runoff_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_runoff_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_stage_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_stage_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_to_mvr_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_to_mvr_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.sfr_upstream-flow_ob.csv" del "MF6_TestSfrMawLakMvrUzf.sfr_upstream-flow_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.SfrConvergence.CSV" del "MF6_TestSfrMawLakMvrUzf.SfrConvergence.CSV"
if exist "MF6_TestSfrMawLakMvrUzf.stage" del "MF6_TestSfrMawLakMvrUzf.stage"
if exist "MF6_TestSfrMawLakMvrUzf.storage_ob.csv" del "MF6_TestSfrMawLakMvrUzf.storage_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.uzet_ob.csv" del "MF6_TestSfrMawLakMvrUzf.uzet_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.uzf_budget" del "MF6_TestSfrMawLakMvrUzf.uzf_budget"
if exist "MF6_TestSfrMawLakMvrUzf.UzfConvergence.CSV" del "MF6_TestSfrMawLakMvrUzf.UzfConvergence.CSV"
if exist "MF6_TestSfrMawLakMvrUzf.uzf-gwd_ob.csv" del "MF6_TestSfrMawLakMvrUzf.uzf-gwd_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.uzf-gwd-to-mvr_ob.csv" del "MF6_TestSfrMawLakMvrUzf.uzf-gwd-to-mvr_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.uzf-gwet_ob.csv" del "MF6_TestSfrMawLakMvrUzf.uzf-gwet_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.uzf-gwrch_ob.csv" del "MF6_TestSfrMawLakMvrUzf.uzf-gwrch_ob.csv"
if exist "MF6_TestSfrMawLakMvrUzf.water-content_ob.csv" del "MF6_TestSfrMawLakMvrUzf.water-content_ob.csv"
if exist "mfsim.lst" del "mfsim.lst"
"plproc.exe" MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth.script
"plproc.exe" MF6_TestSfrMawLakMvrUzf.SfrWidth.script
"EnhancedTemplateProcessor.exe" MF6_TestSfrMawLakMvrUzf.3.laktab.tpl MF6_TestSfrMawLakMvrUzf.pval
"EnhancedTemplateProcessor.exe" MF6_TestSfrMawLakMvrUzf.lak6.tpl MF6_TestSfrMawLakMvrUzf.pval
"EnhancedTemplateProcessor.exe" MF6_TestSfrMawLakMvrUzf.maw6.tpl MF6_TestSfrMawLakMvrUzf.pval
"EnhancedTemplateProcessor.exe" MF6_TestSfrMawLakMvrUzf.sfr.tpl MF6_TestSfrMawLakMvrUzf.pval
"EnhancedTemplateProcessor.exe" MF6_TestSfrMawLakMvrUzf.uzf.tpl MF6_TestSfrMawLakMvrUzf.pval
mf6.exe
"Mf6ObsExtractor.exe" MF6_TestSfrMawLakMvrUzf.Mf6ExtractValues
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

