@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "StrFhbTobTest.fhb" del "StrFhbTobTest.fhb"
if exist "StrFhbTobTest.Mf2005Values" del "StrFhbTobTest.Mf2005Values"
if exist "StrFhbTobTest.cbc" del "StrFhbTobTest.cbc"
if exist "StrFhbTobTest.fdn" del "StrFhbTobTest.fdn"
if exist "StrFhbTobTest.fhd" del "StrFhbTobTest.fhd"
if exist "StrFhbTobTest.ftl" del "StrFhbTobTest.ftl"
if exist "StrFhbTobTest.lst" del "StrFhbTobTest.lst"
if exist "StrFhbTobTest.stob_out" del "StrFhbTobTest.stob_out"
if exist "StrFhbTobTest.str_flow" del "StrFhbTobTest.str_flow"
"EnhancedTemplateProcessor.exe" StrFhbTobTest.fhb.tpl StrFhbTobTest.pval
mf2005.exe StrFhbTobTest.nam /wait
"Mf2005ObsExtractor.exe" StrFhbTobTest.Mf2005ExtractValues
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

