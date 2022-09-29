Copy /Y /A "CSubExampletest.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.3.0\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "CSubExampletest.mfsim.lst"
"Mf6ObsExtractor.exe" CSubExampletest.Mf6WriteIns
C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe CSubExampletest.lst
pause
