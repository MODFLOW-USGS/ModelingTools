Copy /Y /A "MF6_TestSfrMawLak.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.3.0\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "MF6_TestSfrMawLak.mfsim.lst"
C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe MF6_TestSfrMawLak.lst
pause
