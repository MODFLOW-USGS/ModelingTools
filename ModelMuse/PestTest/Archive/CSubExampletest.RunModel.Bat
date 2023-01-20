@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\CSubExampletest.Confining_Bed_InitialPorosity_1.arrays" del "arrays\CSubExampletest.Confining_Bed_InitialPorosity_1.arrays"
if exist "arrays\CSubExampletest.Confining_Bed_InitialPorosity_2.arrays" del "arrays\CSubExampletest.Confining_Bed_InitialPorosity_2.arrays"
if exist "arrays\CSubExampletest.Confining_Bed_InitialPorosity_3.arrays" del "arrays\CSubExampletest.Confining_Bed_InitialPorosity_3.arrays"
if exist "arrays\CSubExampletest.Confining_Bed_InitialPorosity_4.arrays" del "arrays\CSubExampletest.Confining_Bed_InitialPorosity_4.arrays"
if exist "CSubExampletest.csub" del "CSubExampletest.csub"
if exist "CSubExampletest.Mf6Values" del "CSubExampletest.Mf6Values"
if exist "CSubExampletest.bhd" del "CSubExampletest.bhd"
if exist "CSubExampletest.cbc" del "CSubExampletest.cbc"
if exist "CSubExampletest.coarse-compaction_ob.csv" del "CSubExampletest.coarse-compaction_ob.csv"
if exist "CSubExampletest.coarse-csub_ob.csv" del "CSubExampletest.coarse-csub_ob.csv"
if exist "CSubExampletest.compaction-cell_ob.csv" del "CSubExampletest.compaction-cell_ob.csv"
if exist "CSubExampletest.csub_z_dis" del "CSubExampletest.csub_z_dis"
if exist "CSubExampletest.csub-cell_ob.csv" del "CSubExampletest.csub-cell_ob.csv"
if exist "CSubExampletest.CsubConvergence.CSV" del "CSubExampletest.CsubConvergence.CSV"
if exist "CSubExampletest.delay-preconstress_ob.csv" del "CSubExampletest.delay-preconstress_ob.csv"
if exist "CSubExampletest.elastic-compaction_ob.csv" del "CSubExampletest.elastic-compaction_ob.csv"
if exist "CSubExampletest.elastic-csub_ob.csv" del "CSubExampletest.elastic-csub_ob.csv"
if exist "CSubExampletest.estress-cell_ob.csv" del "CSubExampletest.estress-cell_ob.csv"
if exist "CSubExampletest.gstress-cell_ob.csv" del "CSubExampletest.gstress-cell_ob.csv"
if exist "CSubExampletest.inelastic-csub_ob.csv" del "CSubExampletest.inelastic-csub_ob.csv"
if exist "CSubExampletest.InnerSolution.CSV" del "CSubExampletest.InnerSolution.CSV"
if exist "CSubExampletest.interbed-compaction_ob.csv" del "CSubExampletest.interbed-compaction_ob.csv"
if exist "CSubExampletest.lst" del "CSubExampletest.lst"
if exist "CSubExampletest.ob_gw_out_head.csv" del "CSubExampletest.ob_gw_out_head.csv"
if exist "CSubExampletest.OuterSolution.CSV" del "CSubExampletest.OuterSolution.CSV"
if exist "CSubExampletest.sk-cell_ob.csv" del "CSubExampletest.sk-cell_ob.csv"
if exist "CSubExampletest.theta_ob.csv" del "CSubExampletest.theta_ob.csv"
if exist "CSubExampletest.wcomp-csub-cell_ob.csv" del "CSubExampletest.wcomp-csub-cell_ob.csv"
if exist "mfsim.lst" del "mfsim.lst"
"plproc.exe" CSubExampletest.Confining_Bed_InitialPorosity.script
"EnhancedTemplateProcessor.exe" CSubExampletest.csub.tpl CSubExampletest.pval
mf6.exe
"Mf6ObsExtractor.exe" CSubExampletest.Mf6ExtractValues
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

