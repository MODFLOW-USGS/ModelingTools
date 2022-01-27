if exist "ChdTest.chd" del "ChdTest.chd"
if exist "ChdTest.Mf2005Values" del "ChdTest.Mf2005Values"
if exist "arrays\ChdTest.Kx_1.arrays" del "arrays\ChdTest.Kx_1.arrays"
if exist "arrays\ChdTest.Kx_2.arrays" del "arrays\ChdTest.Kx_2.arrays"
if exist "arrays\ChdTest.Kx_3.arrays" del "arrays\ChdTest.Kx_3.arrays"
if exist "ChdTest.cbc" del "ChdTest.cbc"
if exist "ChdTest.fdn" del "ChdTest.fdn"
if exist "ChdTest.fhd" del "ChdTest.fhd"
if exist "ChdTest.hob_out" del "ChdTest.hob_out"
if exist "ChdTest.lst" del "ChdTest.lst"
"plproc.exe" ChdTest.Kx.script
"EnhancedTemplateProcessor.exe" ChdTest.chd.tpl ChdTest.pval
mf2005.exe ChdTest.nam /wait
"Mf2005ObsExtractor.exe" ChdTest.Mf2005ExtractValues
