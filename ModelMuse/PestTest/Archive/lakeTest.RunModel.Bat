if exist "arrays\lakeTest.Lakebed_Leakance_1.arrays" del "arrays\lakeTest.Lakebed_Leakance_1.arrays"
if exist "arrays\lakeTest.Lakebed_Leakance_2.arrays" del "arrays\lakeTest.Lakebed_Leakance_2.arrays"
if exist "arrays\lakeTest.Lakebed_Leakance_3.arrays" del "arrays\lakeTest.Lakebed_Leakance_3.arrays"
if exist "arrays\lakeTest.Lakebed_Leakance_4.arrays" del "arrays\lakeTest.Lakebed_Leakance_4.arrays"
if exist "arrays\lakeTest.Lakebed_Leakance_5.arrays" del "arrays\lakeTest.Lakebed_Leakance_5.arrays"
if exist "arrays\lakeTest.Lakebed_Leakance_7.arrays" del "arrays\lakeTest.Lakebed_Leakance_7.arrays"
if exist "arrays\lakeTest.Lakebed_Leakance_8.arrays" del "arrays\lakeTest.Lakebed_Leakance_8.arrays"
if exist "arrays\lakeTest.Lakebed_Leakance_9.arrays" del "arrays\lakeTest.Lakebed_Leakance_9.arrays"
if exist "lakeTest.lak" del "lakeTest.lak"
if exist "lakeTest.Mf2005Values" del "lakeTest.Mf2005Values"
if exist "lakeTest.cbc" del "lakeTest.cbc"
if exist "lakeTest.fdn" del "lakeTest.fdn"
if exist "lakeTest.fhd" del "lakeTest.fhd"
if exist "lakeTest.hob_out" del "lakeTest.hob_out"
if exist "lakeTest.lakg1" del "lakeTest.lakg1"
if exist "lakeTest.lst" del "lakeTest.lst"
"plproc.exe" lakeTest.Lakebed_Leakance.script
"EnhancedTemplateProcessor.exe" lakeTest.lak.tpl lakeTest.pval
mf2005.exe lakeTest.nam /wait
"Mf2005ObsExtractor.exe" lakeTest.Mf2005ExtractValues
