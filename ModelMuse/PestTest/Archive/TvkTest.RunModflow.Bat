Copy /Y /A "TvkTest.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.2_win64\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" TvkTest.Mf6WriteIns
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe TvkTest.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
