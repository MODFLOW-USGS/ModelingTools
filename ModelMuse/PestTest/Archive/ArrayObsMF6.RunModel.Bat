@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "ArrayObsMF6.chd" del "ArrayObsMF6.chd"
if exist "ArrayObsMF6.Kx_1.arrayvalues" del "ArrayObsMF6.Kx_1.arrayvalues"
if exist "ArrayObsMF6.Kx_2.arrayvalues" del "ArrayObsMF6.Kx_2.arrayvalues"
if exist "ArrayObsMF6.Kx_3.arrayvalues" del "ArrayObsMF6.Kx_3.arrayvalues"
if exist "ArrayObsMF6.Mf6Values" del "ArrayObsMF6.Mf6Values"
if exist "ArrayObsMF6.Kx_1.arrayvalues" del "ArrayObsMF6.Kx_1.arrayvalues"
if exist "ArrayObsMF6.Kx_2.arrayvalues" del "ArrayObsMF6.Kx_2.arrayvalues"
if exist "ArrayObsMF6.Kx_3.arrayvalues" del "ArrayObsMF6.Kx_3.arrayvalues"
if exist "arrays\ArrayObsMF6.Kx_1.arrays" del "arrays\ArrayObsMF6.Kx_1.arrays"
if exist "arrays\ArrayObsMF6.Kx_2.arrays" del "arrays\ArrayObsMF6.Kx_2.arrays"
if exist "arrays\ArrayObsMF6.Kx_3.arrays" del "arrays\ArrayObsMF6.Kx_3.arrays"
if exist "ArrayObsMF6.bhd" del "ArrayObsMF6.bhd"
if exist "ArrayObsMF6.cbc" del "ArrayObsMF6.cbc"
if exist "ArrayObsMF6.InnerSolution.CSV" del "ArrayObsMF6.InnerSolution.CSV"
if exist "ArrayObsMF6.lst" del "ArrayObsMF6.lst"
if exist "ArrayObsMF6.ob_gw_out_head.csv" del "ArrayObsMF6.ob_gw_out_head.csv"
if exist "ArrayObsMF6.OuterSolution.CSV" del "ArrayObsMF6.OuterSolution.CSV"
if exist "mfsim.lst" del "mfsim.lst"
"plproc.exe" ArrayObsMF6.Kx.script
"EnhancedTemplateProcessor.exe" ArrayObsMF6.chd.tpl ArrayObsMF6.pval

GetValuesFromArrayFiles.exe C:\ModelingTools\ModelMuse\PestTest\ArrayObsMF6.Kx_1.ExtractIns
GetValuesFromArrayFiles.exe C:\ModelingTools\ModelMuse\PestTest\ArrayObsMF6.Kx_2.ExtractIns
GetValuesFromArrayFiles.exe C:\ModelingTools\ModelMuse\PestTest\ArrayObsMF6.Kx_3.ExtractIns
mf6.exe
"Mf6ObsExtractor.exe" ArrayObsMF6.Mf6ExtractValues
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

