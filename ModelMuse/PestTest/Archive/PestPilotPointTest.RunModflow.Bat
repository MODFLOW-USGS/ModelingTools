call C:\ModelingTools\ModelMuse\Debug\Win64\ModelMonitor.exe -m C:\WRDAPP\mf6.2.2\bin\mf6.exe -n C:\ModelingTools\ModelMuse\PestTest\mfsim.nam -mv 6
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf6ObsExtractor.exe" PestPilotPointTest.Mf6WriteIns
"C:\Pest17.3\plproc.exe" PestPilotPointTest.Kx.krig_factors_script
"C:\Pest17.3\plproc.exe" PestPilotPointTest.Ky.krig_factors_script
C:\WRDAPP\ListingAnalyst_1_2\bin\ListingAnalyst.exe C:\ModelingTools\ModelMuse\PestTest\PestPilotPointTest.lst
pause
