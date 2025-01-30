@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "FreezingWallPest.inp" del "FreezingWallPest.inp"
if exist "FreezingWallPest.bcof" del "FreezingWallPest.bcof"
if exist "FreezingWallPest.bcop" del "FreezingWallPest.bcop"
if exist "FreezingWallPest.bcou" del "FreezingWallPest.bcou"
if exist "FreezingWallPest.ele" del "FreezingWallPest.ele"
if exist "FreezingWallPest.lst" del "FreezingWallPest.lst"
if exist "FreezingWallPest.nod" del "FreezingWallPest.nod"
if exist "FreezingWallPest.rst" del "FreezingWallPest.rst"
if exist "FreezingWallPest.smy" del "FreezingWallPest.smy"
if exist "FreezingWallPest_N_2257.OBS" del "FreezingWallPest_N_2257.OBS"
if exist "FreezingWallPest_N_3233.OBS" del "FreezingWallPest_N_3233.OBS"
if exist "FreezingWallPest_N_4209.OBS" del "FreezingWallPest_N_4209.OBS"
if exist "FreezingWallPest_N_5185.OBS" del "FreezingWallPest_N_5185.OBS"
if exist "FreezingWallPest_N_6161.OBS" del "FreezingWallPest_N_6161.OBS"
if exist "FreezingWallPest.SUTRA.FIL" copy /Y "FreezingWallPest.SUTRA.FIL" "SUTRA.FIL"
"EnhancedTemplateProcessor.exe" FreezingWallPest.inp.tpl FreezingWallPest.pval
"sutra_4_0.exe"
"SutraObsExtractor.exe" FreezingWallPest.soe_ev
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

