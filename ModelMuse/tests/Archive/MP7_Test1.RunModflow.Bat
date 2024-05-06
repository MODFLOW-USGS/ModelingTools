Copy /Y /A "MP7_Test1.mfsim.nam" "mfsim.nam"
call C:\ModelingTools\ModelMuse\Release\Win64\ModelMonitor.exe -m C:\WRDAPP\mf6.4.4_win64\bin\mf6.exe -n mfsim.nam -mv 6
call RunModpath.bat
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe MP7_Test1.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
pause
