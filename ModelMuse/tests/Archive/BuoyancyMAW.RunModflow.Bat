Copy /Y /A "BuoyancyMAW.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.4_win64\bin\mf6.exe -n mfsim.nam -mv 6
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe BuoyancyMAW.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe BuoyancyMAW.Salinity.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe BuoyancyMAW.Temperature.lst
pause
