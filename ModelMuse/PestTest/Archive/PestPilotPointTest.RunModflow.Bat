Copy /Y /A "PestPilotPointTest.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.6.0_win64\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" PestPilotPointTest.Mf6WriteIns
"plproc.exe" PestPilotPointTest.Kx.krig_factors_script
"plproc.exe" PestPilotPointTest.Ky.krig_factors_script
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe PestPilotPointTest.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
