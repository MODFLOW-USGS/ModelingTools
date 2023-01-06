Copy /Y /A "MwtTimeSeries.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.1\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "MwtTimeSeries.mfsim.lst"
Copy /Y /A "MwtTimeSeries.Chem.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.1\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "MwtTimeSeries.Chem.mfsim.lst"
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe MwtTimeSeries.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe MwtTimeSeries.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe MwtTimeSeries.Chem.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe MwtTimeSeries.Chem.lst
pause
