if exist "SutraTest2a.SUTRA.FIL" copy /Y "SutraTest2a.SUTRA.FIL" "SUTRA.FIL"
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraTest2a.soe_i
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SutraTest2a.lst
pause
