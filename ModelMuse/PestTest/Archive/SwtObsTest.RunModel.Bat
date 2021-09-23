if exist "arrays\SwtObsTest.Specific_Gravity_Saturated_1.arrays" del "arrays\SwtObsTest.Specific_Gravity_Saturated_1.arrays"
if exist "SwtObsTest.Mf2005Values" del "SwtObsTest.Mf2005Values"
if exist "SwtObsTest.cbc" del "SwtObsTest.cbc"
if exist "SwtObsTest.fdn" del "SwtObsTest.fdn"
if exist "SwtObsTest.fhd" del "SwtObsTest.fhd"
if exist "SwtObsTest.lst" del "SwtObsTest.lst"
if exist "SwtObsTest.Swt_Out" del "SwtObsTest.Swt_Out"
"C:\Pest17.3\plproc.exe" SwtObsTest.Specific_Gravity_Saturated.script
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe SwtObsTest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" SwtObsTest.Mf2005ExtractValues
