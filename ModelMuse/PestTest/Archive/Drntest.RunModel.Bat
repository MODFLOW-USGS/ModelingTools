if exist "arrays\Drntest.Kx_1.arrays" del "arrays\Drntest.Kx_1.arrays"
if exist "arrays\Drntest.Kx_2.arrays" del "arrays\Drntest.Kx_2.arrays"
if exist "arrays\Drntest.Kx_3.arrays" del "arrays\Drntest.Kx_3.arrays"
if exist "arrays\Drntest.test_1.arrays" del "arrays\Drntest.test_1.arrays"
if exist "Drntest.drn" del "Drntest.drn"
if exist "Drntest.Mf2005Values" del "Drntest.Mf2005Values"
if exist "Drntest.bdn" del "Drntest.bdn"
if exist "Drntest.hob_out" del "Drntest.hob_out"
if exist "Drntest.lst" del "Drntest.lst"
"C:\Pest17.3\plproc.exe" Drntest.Kx.script
"C:\Pest17.3\plproc.exe" Drntest.test.script
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" Drntest.drn.tpl Drntest.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe Drntest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" Drntest.Mf2005ExtractValues
