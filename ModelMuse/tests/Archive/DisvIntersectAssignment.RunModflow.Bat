Copy /Y /A "DisvIntersectAssignment.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.6.1_win64\bin\mf6.exe -n mfsim.nam -mv 6
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe DisvIntersectAssignment.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
