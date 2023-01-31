if exist "Rocky2D_IrregularS4NoInterpolate.SUTRA.FIL" copy /Y "Rocky2D_IrregularS4NoInterpolate.SUTRA.FIL" "SUTRA.FIL"
"sutra_4_0.exe"
"SutraObsExtractor.exe" Rocky2D_IrregularS4NoInterpolate.soe_i
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Rocky2D_IrregularS4NoInterpolate.lst
pause
