Copy /Y /A "BuoyancySFR.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.3_win64\bin\mf6.exe -n mfsim.nam -mv 6
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe BuoyancySFR.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe BuoyancySFR.Salinity.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe BuoyancySFR.Temperature.lst
pause
