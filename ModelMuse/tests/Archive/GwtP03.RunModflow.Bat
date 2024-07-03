Copy /Y /A "GwtP03.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.5.0_win64\bin\mf6.exe -n mfsim.nam -mv 6
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe GwtP03.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe GwtP03.Chem.lst
pause
