rem Create Variogram file.

"C:\Pest18\mkppstat.exe" <RmaSutraNodeTest.Maximum_Hydraulic_Conductivity.K.1.Mkppstat_Input.txt

rem Create Covariance file.

"C:\Pest18\ppcov_sva.exe" <RmaSutraNodeTest.Maximum_Hydraulic_Conductivity.K.1.Ppcov_SVA_Input.txt

rem After the previous line executes, sppcov_sva should show that a covariance file was written.
