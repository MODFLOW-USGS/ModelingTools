if exist "FreezingWallPest.SUTRA.FIL" copy /Y "FreezingWallPest.SUTRA.FIL" "SUTRA.FIL"
"sutra_4_0.exe"
"SutraObsExtractor.exe" FreezingWallPest.soe_i
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe FreezingWallPest.lst
pause
