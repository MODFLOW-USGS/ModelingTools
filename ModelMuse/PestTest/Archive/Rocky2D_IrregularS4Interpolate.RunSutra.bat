if exist "Rocky2D_IrregularS4Interpolate.SUTRA.FIL" copy /Y "Rocky2D_IrregularS4Interpolate.SUTRA.FIL" "SUTRA.FIL"
"plproc.exe" 'Rocky2D_IrregularS4Interpolate.Nodal_Porosity.krig_factors_script'
"plproc.exe" 'Rocky2D_IrregularS4Interpolate.Solid_Matrix_Compressibility.krig_factors_script'
"plproc.exe" 'Rocky2D_IrregularS4Interpolate.Solid_Grain_Specific_Heat.krig_factors_script'
"plproc.exe" 'Rocky2D_IrregularS4Interpolate.14B.krig_factors_script'
"plproc.exe" 'Rocky2D_IrregularS4Interpolate.Maximum_Permeability.krig_factors_script'
"sutra_4_0_0_PROVISIONAL_2023-02-28_8b569f6.exe"
"SutraObsExtractor.exe" Rocky2D_IrregularS4Interpolate.soe_i
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe Rocky2D_IrregularS4Interpolate.lst
pause
