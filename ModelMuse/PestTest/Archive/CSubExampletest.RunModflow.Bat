Copy /Y /A "CSubExampletest.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.5.0_win64\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" CSubExampletest.Mf6WriteIns
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe CSubExampletest.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
