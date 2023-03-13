if exist "SutraGenBoundInactivate.SUTRA.FIL" copy /Y "SutraGenBoundInactivate.SUTRA.FIL" "SUTRA.FIL"
"C:\SutraSuite\SUTRA_3_0\bin\sutra_3_0.exe"
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SutraGenBoundInactivate.lst
pause
