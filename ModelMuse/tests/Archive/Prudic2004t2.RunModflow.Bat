Copy /Y /A "Prudic2004t2.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.3.0\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "Prudic2004t2.mfsim.lst"
Copy /Y /A "Prudic2004t2.Chem.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.3.0\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "Prudic2004t2.Chem.mfsim.lst"
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Prudic2004t2.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Prudic2004t2.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Prudic2004t2.Chem.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Prudic2004t2.Chem.lst
pause
