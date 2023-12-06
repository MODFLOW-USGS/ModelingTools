Copy /Y /A "GncTest.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.4.2_win64\bin\mf6.exe -n mfsim.nam -mv 6
Start Notepad.exe GncTest.lst
Start Notepad.exe mfsim.lst
pause
