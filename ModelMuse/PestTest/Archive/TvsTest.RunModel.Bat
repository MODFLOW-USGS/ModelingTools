@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "TvsTest.Mf6Values" del "TvsTest.Mf6Values"
if exist "TvsTest.tvs" del "TvsTest.tvs"
if exist "mfsim.lst" del "mfsim.lst"
if exist "TvsTest.bhd" del "TvsTest.bhd"
if exist "TvsTest.cbc" del "TvsTest.cbc"
if exist "TvsTest.chob_out_chd.csv" del "TvsTest.chob_out_chd.csv"
if exist "TvsTest.InnerSolution.CSV" del "TvsTest.InnerSolution.CSV"
if exist "TvsTest.lst" del "TvsTest.lst"
if exist "TvsTest.OuterSolution.CSV" del "TvsTest.OuterSolution.CSV"
"EnhancedTemplateProcessor.exe" TvsTest.tvs.tpl TvsTest.pval
mf6.exe
"Mf6ObsExtractor.exe" TvsTest.Mf6ExtractValues
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

