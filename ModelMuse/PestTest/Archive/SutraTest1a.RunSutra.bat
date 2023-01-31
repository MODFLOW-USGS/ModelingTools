if exist "SutraTest1a.SUTRA.FIL" copy /Y "SutraTest1a.SUTRA.FIL" "SUTRA.FIL"
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraTest1a.soe_i
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SutraTest1a.lst
pause
