Copy /Y /A "RechAddTestMf6.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.1\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" RechAddTestMf6.Mf6WriteIns
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe RechAddTestMf6.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
