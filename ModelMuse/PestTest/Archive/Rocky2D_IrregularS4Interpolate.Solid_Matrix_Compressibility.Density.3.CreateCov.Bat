rem Create Variogram file.

"C:\Pest17_5\mkppstat.exe" <Rocky2D_IrregularS4Interpolate.Solid_Matrix_Compressibility.Density.3.Mkppstat_Input.txt

rem Create Covariance file.

"C:\Pest17_5\ppcov_sva.exe" <Rocky2D_IrregularS4Interpolate.Solid_Matrix_Compressibility.Density.3.Ppcov_SVA_Input.txt

rem After the previous line executes, sppcov_sva should show that a covariance file was written.
