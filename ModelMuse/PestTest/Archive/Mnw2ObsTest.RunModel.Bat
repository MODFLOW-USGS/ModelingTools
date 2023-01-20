@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\Mnw2ObsTest.Kx_1.arrays" del "arrays\Mnw2ObsTest.Kx_1.arrays"
if exist "arrays\Mnw2ObsTest.Kx_2.arrays" del "arrays\Mnw2ObsTest.Kx_2.arrays"
if exist "arrays\Mnw2ObsTest.Kx_3.arrays" del "arrays\Mnw2ObsTest.Kx_3.arrays"
if exist "Mnw2ObsTest.Mf2005Values" del "Mnw2ObsTest.Mf2005Values"
if exist "Mnw2ObsTest.cbc" del "Mnw2ObsTest.cbc"
if exist "Mnw2ObsTest.fdn" del "Mnw2ObsTest.fdn"
if exist "Mnw2ObsTest.fhd" del "Mnw2ObsTest.fhd"
if exist "Mnw2ObsTest.lst" del "Mnw2ObsTest.lst"
if exist "Mnw2ObsTest_Well3.mnwi_out" del "Mnw2ObsTest_Well3.mnwi_out"
"plproc.exe" Mnw2ObsTest.Kx.script
mf2005.exe Mnw2ObsTest.nam /wait
"Mf2005ObsExtractor.exe" Mnw2ObsTest.Mf2005ExtractValues
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

