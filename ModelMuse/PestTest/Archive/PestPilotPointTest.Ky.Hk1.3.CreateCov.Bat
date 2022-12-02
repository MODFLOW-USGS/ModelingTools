rem Create Variogram file.

"C:\PEST17_4\mkppstat.exe" <PestPilotPointTest.Ky.Hk1.3.Mkppstat_Input.txt

rem Create Covariance file.

"C:\PEST17_4\ppcov_sva.exe" <PestPilotPointTest.Ky.Hk1.3.Ppcov_SVA_Input.txt

rem After the previous line executes, sppcov_sva should show that a covariance file was written.
