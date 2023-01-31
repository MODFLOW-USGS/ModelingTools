if exist "SutraGenBoundLakeTest.SUTRA.FIL" copy /Y "SutraGenBoundLakeTest.SUTRA.FIL" "SUTRA.FIL"
"sutra_3_0.exe"
"SutraObsExtractor.exe" SutraGenBoundLakeTest.soe_i
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SutraGenBoundLakeTest.lst
pause
