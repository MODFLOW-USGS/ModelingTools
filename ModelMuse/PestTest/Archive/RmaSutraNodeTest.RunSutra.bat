if exist "RmaSutraNodeTest.SUTRA.FIL" copy /Y "RmaSutraNodeTest.SUTRA.FIL" "SUTRA.FIL"
"plproc.exe" 'RmaSutraNodeTest.Maximum_Hydraulic_Conductivity.krig_factors_script'
"sutra_3_0.exe"
"SutraObsExtractor.exe" RmaSutraNodeTest.soe_i
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe RmaSutraNodeTest.lst
pause
