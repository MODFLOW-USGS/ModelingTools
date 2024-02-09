Copy /Y /A "HfbMf6Test2.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.3_win64\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" HfbMf6Test2.Mf6WriteIns
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe HfbMf6Test2.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
