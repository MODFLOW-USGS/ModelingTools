Copy /Y /A "Prediction.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.3.0\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "Prediction.mfsim.lst"
"Mf6ObsExtractor.exe" Prediction.Mf6WriteIns
C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Prediction.lst
pause
