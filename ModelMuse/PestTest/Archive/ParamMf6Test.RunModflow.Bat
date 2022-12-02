call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.0\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" ParamMf6Test.Mf6WriteIns
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe ParamMf6Test.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
