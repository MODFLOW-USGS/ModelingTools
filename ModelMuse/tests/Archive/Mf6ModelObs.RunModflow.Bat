Copy /Y /A "Mf6ModelObs.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.5.0_win64\bin\mf6.exe -n mfsim.nam -mv 6
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Mf6ModelObs.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Mf6ModelObs.Chem.lst
pause
