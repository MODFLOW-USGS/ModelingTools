call C:\ModelingTools\ModelMuse\Debug\Win64\ModelMonitor.exe -m C:\WRDAPP\mf6.2.2\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" PestPilotPointTest.Mf6WriteIns
"plproc.exe" PestPilotPointTest.Kx.krig_factors_script
"plproc.exe" PestPilotPointTest.Ky.krig_factors_script
C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe PestPilotPointTest.lst
pause
