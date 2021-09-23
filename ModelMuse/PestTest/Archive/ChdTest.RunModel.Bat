if exist "arrays\ChdTest.Kx_1.arrays" del "arrays\ChdTest.Kx_1.arrays"
if exist "arrays\ChdTest.Kx_2.arrays" del "arrays\ChdTest.Kx_2.arrays"
if exist "arrays\ChdTest.Kx_3.arrays" del "arrays\ChdTest.Kx_3.arrays"
if exist "ChdTest.chd" del "ChdTest.chd"
if exist "ChdTest.Mf2005Values" del "ChdTest.Mf2005Values"
if exist "ChdTest.cbc" del "ChdTest.cbc"
if exist "ChdTest.fdn" del "ChdTest.fdn"
if exist "ChdTest.fhd" del "ChdTest.fhd"
if exist "ChdTest.hob_out" del "ChdTest.hob_out"
if exist "ChdTest.lst" del "ChdTest.lst"
"C:\Pest17.3\plproc.exe" ChdTest.Kx.script
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" ChdTest.chd.tpl ChdTest.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe ChdTest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" ChdTest.Mf2005ExtractValues
