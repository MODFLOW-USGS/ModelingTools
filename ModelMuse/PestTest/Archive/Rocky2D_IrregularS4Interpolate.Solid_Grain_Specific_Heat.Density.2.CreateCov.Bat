rem Create Variogram file.

"C:\Pest17.3\mkppstat.exe" <Rocky2D_IrregularS4Interpolate.Solid_Grain_Specific_Heat.Density.2.Mkppstat_Input.txt

rem Create Covariance file.

"C:\Pest17.3\ppcov_sva.exe" <Rocky2D_IrregularS4Interpolate.Solid_Grain_Specific_Heat.Density.2.Ppcov_SVA_Input.txt

rem After the previous line executes, sppcov_sva should show that a covariance file was written.
