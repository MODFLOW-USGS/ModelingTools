Copy /Y /A "mt3dsupp82.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.6.1_win64\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "mt3dsupp82.mfsim.lst"
Copy /Y /A "mt3dsupp82.Chem.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.6.1_win64\bin\mf6.exe -n mfsim.nam -mv 6
Copy /Y /A "mfsim.lst" "mt3dsupp82.Chem.mfsim.lst"
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mt3dsupp82.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mt3dsupp82.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mt3dsupp82.Chem.mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mt3dsupp82.Chem.lst
pause
