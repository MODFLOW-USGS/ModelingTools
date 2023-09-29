@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "TvkTest.Mf6Values" del "TvkTest.Mf6Values"
if exist "TvkTest.tvk" del "TvkTest.tvk"
if exist "mfsim.lst" del "mfsim.lst"
if exist "TvkTest.bhd" del "TvkTest.bhd"
if exist "TvkTest.cbc" del "TvkTest.cbc"
if exist "TvkTest.chob_out_chd.csv" del "TvkTest.chob_out_chd.csv"
if exist "TvkTest.InnerSolution.CSV" del "TvkTest.InnerSolution.CSV"
if exist "TvkTest.lst" del "TvkTest.lst"
if exist "TvkTest.OuterSolution.CSV" del "TvkTest.OuterSolution.CSV"
"EnhancedTemplateProcessor.exe" TvkTest.tvk.tpl TvkTest.pval
mf6.exe
"Mf6ObsExtractor.exe" TvkTest.Mf6ExtractValues
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

