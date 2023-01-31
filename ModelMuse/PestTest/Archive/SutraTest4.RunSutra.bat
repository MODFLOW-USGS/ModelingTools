if exist "SutraTest4.SUTRA.FIL" copy /Y "SutraTest4.SUTRA.FIL" "SUTRA.FIL"
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraTest4.soe_i
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SutraTest4.lst
pause
