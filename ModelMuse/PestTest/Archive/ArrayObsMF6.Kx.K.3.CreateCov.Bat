rem Create Variogram file.

"C:\Pest17_5\mkppstat.exe" <ArrayObsMF6.Kx.K.3.Mkppstat_Input.txt

rem Create Covariance file.

"C:\Pest17_5\ppcov_sva.exe" <ArrayObsMF6.Kx.K.3.Ppcov_SVA_Input.txt

rem After the previous line executes, sppcov_sva should show that a covariance file was written.
