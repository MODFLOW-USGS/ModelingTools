if exist "arrays\lakeTestObs.Lakebed_Leakance_1.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_1.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_2.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_2.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_3.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_3.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_4.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_4.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_5.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_5.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_7.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_7.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_8.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_8.arrays"
if exist "arrays\lakeTestObs.Lakebed_Leakance_9.arrays" del "arrays\lakeTestObs.Lakebed_Leakance_9.arrays"
if exist "lakeTestObs.lak" del "lakeTestObs.lak"
if exist "lakeTestObs.Mf2005Values" del "lakeTestObs.Mf2005Values"
if exist "lakeTestObs.cbc" del "lakeTestObs.cbc"
if exist "lakeTestObs.fdn" del "lakeTestObs.fdn"
if exist "lakeTestObs.fhd" del "lakeTestObs.fhd"
if exist "lakeTestObs.hob_out" del "lakeTestObs.hob_out"
if exist "lakeTestObs.lakg1" del "lakeTestObs.lakg1"
if exist "lakeTestObs.lakg2" del "lakeTestObs.lakg2"
if exist "lakeTestObs.lst" del "lakeTestObs.lst"
"C:\Pest17.3\plproc.exe" lakeTestObs.Lakebed_Leakance.script
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" lakeTestObs.lak.tpl lakeTestObs.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe lakeTestObs.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" lakeTestObs.Mf2005ExtractValues
