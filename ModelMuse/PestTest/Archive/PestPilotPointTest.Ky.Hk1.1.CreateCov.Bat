rem Create Variogram file.

"C:\Pest17.3\mkppstat.exe" <PestPilotPointTest.Ky.Hk1.1.Mkppstat_Input.txt

rem Create Covariance file.

"C:\Pest17.3\ppcov_sva.exe" <PestPilotPointTest.Ky.Hk1.1.Ppcov_SVA_Input.txt

rem After the previous line executes, sppcov_sva should show that a covariance file was written.
