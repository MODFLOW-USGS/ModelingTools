if exist "arrays\EtsTest.Kx_1.arrays" del "arrays\EtsTest.Kx_1.arrays"
if exist "arrays\EtsTest.Kx_2.arrays" del "arrays\EtsTest.Kx_2.arrays"
if exist "arrays\EtsTest.Kx_3.arrays" del "arrays\EtsTest.Kx_3.arrays"
if exist "EtsTest.ets" del "EtsTest.ets"
if exist "EtsTest.Mf2005Values" del "EtsTest.Mf2005Values"
if exist "EtsTest.cbc" del "EtsTest.cbc"
if exist "EtsTest.fdn" del "EtsTest.fdn"
if exist "EtsTest.fhd" del "EtsTest.fhd"
if exist "EtsTest.hob_out" del "EtsTest.hob_out"
if exist "EtsTest.lst" del "EtsTest.lst"
"plproc.exe" EtsTest.Kx.script
"EnhancedTemplateProcessor.exe" EtsTest.ets.tpl EtsTest.pval
mf2005.exe EtsTest.nam /wait
"Mf2005ObsExtractor.exe" EtsTest.Mf2005ExtractValues
