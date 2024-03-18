Copy /Y /A "ArrayObsMF6.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.4_win64\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" ArrayObsMF6.Mf6WriteIns
"plproc.exe" ArrayObsMF6.Kx.krig_factors_script
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe ArrayObsMF6.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
