Copy /y "SftTimeSeries.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.8.1_win64\bin\mf6.exe -n mfsim.nam -mv 6
Copy /y "mfsim.lst" "SftTimeSeries.mfsim.lst"
Copy /y "SftTimeSeries.Chem.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.8.1_win64\bin\mf6.exe -n mfsim.nam -mv 6
Copy /y "mfsim.lst" "SftTimeSeries.Chem.mfsim.lst"
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SftTimeSeries.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SftTimeSeries.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SftTimeSeries.Chem.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SftTimeSeries.Chem.lst
pause
