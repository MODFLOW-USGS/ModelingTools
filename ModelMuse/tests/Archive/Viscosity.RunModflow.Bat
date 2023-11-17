Copy /Y /A "Viscosity.mfsim.nam" "mfsim.nam"
call C:\ModelingTools\ModelMuse\Debug\Win32\ModelMonitor.exe -m C:\WRDAPP\mf6.4.2_win64\bin\mf6.exe -n mfsim.nam -mv 6
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Viscosity.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Viscosity.Temperature.lst
pause
