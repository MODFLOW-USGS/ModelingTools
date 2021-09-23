if exist "arrays\Mnw2ObsTest.Kx_1.arrays" del "arrays\Mnw2ObsTest.Kx_1.arrays"
if exist "arrays\Mnw2ObsTest.Kx_2.arrays" del "arrays\Mnw2ObsTest.Kx_2.arrays"
if exist "arrays\Mnw2ObsTest.Kx_3.arrays" del "arrays\Mnw2ObsTest.Kx_3.arrays"
if exist "Mnw2ObsTest.Mf2005Values" del "Mnw2ObsTest.Mf2005Values"
if exist "Mnw2ObsTest.cbc" del "Mnw2ObsTest.cbc"
if exist "Mnw2ObsTest.fdn" del "Mnw2ObsTest.fdn"
if exist "Mnw2ObsTest.fhd" del "Mnw2ObsTest.fhd"
if exist "Mnw2ObsTest.lst" del "Mnw2ObsTest.lst"
if exist "Mnw2ObsTest_Well3.mnwi_out" del "Mnw2ObsTest_Well3.mnwi_out"
"C:\Pest17.3\plproc.exe" Mnw2ObsTest.Kx.script
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe Mnw2ObsTest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" Mnw2ObsTest.Mf2005ExtractValues
