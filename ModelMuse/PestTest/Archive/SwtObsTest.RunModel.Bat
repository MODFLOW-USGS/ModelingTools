if exist "arrays\SwtObsTest.Specific_Gravity_Saturated_1.arrays" del "arrays\SwtObsTest.Specific_Gravity_Saturated_1.arrays"
if exist "SwtObsTest.Mf2005Values" del "SwtObsTest.Mf2005Values"
if exist "SwtObsTest.cbc" del "SwtObsTest.cbc"
if exist "SwtObsTest.fdn" del "SwtObsTest.fdn"
if exist "SwtObsTest.fhd" del "SwtObsTest.fhd"
if exist "SwtObsTest.lst" del "SwtObsTest.lst"
if exist "SwtObsTest.Swt_Out" del "SwtObsTest.Swt_Out"
"plproc.exe" SwtObsTest.Specific_Gravity_Saturated.script
mf2005.exe SwtObsTest.nam /wait
"Mf2005ObsExtractor.exe" SwtObsTest.Mf2005ExtractValues
