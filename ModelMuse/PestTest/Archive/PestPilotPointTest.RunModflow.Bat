call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.3.0\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" PestPilotPointTest.Mf6WriteIns
"plproc.exe" PestPilotPointTest.Kx.krig_factors_script
"plproc.exe" PestPilotPointTest.Ky.krig_factors_script
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe PestPilotPointTest.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
