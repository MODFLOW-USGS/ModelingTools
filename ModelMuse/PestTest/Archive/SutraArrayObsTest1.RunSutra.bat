if exist "SutraArrayObsTest1.SUTRA.FIL" copy /Y "SutraArrayObsTest1.SUTRA.FIL" "SUTRA.FIL"
"sutra_2_2.exe"
"SutraObsExtractor.exe" SutraArrayObsTest1.soe_i
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe SutraArrayObsTest1.lst
pause
