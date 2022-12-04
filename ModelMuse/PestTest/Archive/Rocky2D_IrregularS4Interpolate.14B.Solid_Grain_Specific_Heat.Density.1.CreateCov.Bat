rem Create Variogram file.

"C:\PEST17_4\mkppstat.exe" <Rocky2D_IrregularS4Interpolate.14B.Solid_Grain_Specific_Heat.Density.1.Mkppstat_Input.txt

rem Create Covariance file.

"C:\PEST17_4\ppcov_sva.exe" <Rocky2D_IrregularS4Interpolate.14B.Solid_Grain_Specific_Heat.Density.1.Ppcov_SVA_Input.txt

rem After the previous line executes, sppcov_sva should show that a covariance file was written.
