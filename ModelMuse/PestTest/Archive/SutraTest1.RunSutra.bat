if exist "SutraTest1.SUTRA.FIL" copy /Y "SutraTest1.SUTRA.FIL" "SUTRA.FIL"
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraTest1.soe_i
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SutraTest1.lst
pause
