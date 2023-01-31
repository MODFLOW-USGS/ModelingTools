Copy /Y /A "MF6_TestSfrMawLakMvrUzf.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.1\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" MF6_TestSfrMawLakMvrUzf.Mf6WriteIns
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe MF6_TestSfrMawLakMvrUzf.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
