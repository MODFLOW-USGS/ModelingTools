Copy /Y /A "IstImportTest.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.6.1_win64\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "IstImportTest.mfsim.lst"
Copy /Y /A "IstImportTest.Chem1.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.6.1_win64\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "IstImportTest.Chem1.mfsim.lst"
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe IstImportTest.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe IstImportTest.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe IstImportTest.Chem1.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe IstImportTest.Chem1.lst
pause
