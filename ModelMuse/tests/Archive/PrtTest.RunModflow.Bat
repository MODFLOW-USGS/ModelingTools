Copy /y "PrtTest.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.8.1_win64\bin\mf6.exe -n mfsim.nam -mv 6
Copy /y "mfsim.lst" "PrtTest.mfsim.lst"
Copy /y "PrtTest.PRT1.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.8.1_win64\bin\mf6.exe -n mfsim.nam -mv 6
Copy /y "mfsim.lst" "PrtTest.PRT1.mfsim.lst"
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe PrtTest.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe PrtTest.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe PrtTest.PRT1.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe PrtTest.PRT1.lst
pause
