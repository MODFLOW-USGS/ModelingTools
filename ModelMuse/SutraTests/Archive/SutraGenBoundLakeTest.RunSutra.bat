if exist "SutraGenBoundLakeTest.SUTRA.FIL" copy /Y "SutraGenBoundLakeTest.SUTRA.FIL" "SUTRA.FIL"
"C:\SutraSuite\SUTRA_3_0\bin\sutra_3_0.exe"
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SutraGenBoundLakeTest.lst
pause
