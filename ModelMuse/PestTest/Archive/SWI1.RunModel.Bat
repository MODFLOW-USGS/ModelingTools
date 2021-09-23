if exist "arrays\SWI1.Kx_1.arrays" del "arrays\SWI1.Kx_1.arrays"
if exist "SWI1.Mf2005Values" del "SWI1.Mf2005Values"
if exist "SWI1.cbc" del "SWI1.cbc"
if exist "SWI1.fdn" del "SWI1.fdn"
if exist "SWI1.fhd" del "SWI1.fhd"
if exist "SWI1.lst" del "SWI1.lst"
if exist "SWI1.swi_obs" del "SWI1.swi_obs"
if exist "SWI1.zta" del "SWI1.zta"
"C:\Pest17.3\plproc.exe" SWI1.Kx.script
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe SWI1.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" SWI1.Mf2005ExtractValues
