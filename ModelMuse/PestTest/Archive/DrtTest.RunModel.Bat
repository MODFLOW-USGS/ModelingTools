if exist "DrtTest.drt" del "DrtTest.drt"
if exist "DrtTest.Mf2005Values" del "DrtTest.Mf2005Values"
if exist "DrtTest.cbc" del "DrtTest.cbc"
if exist "DrtTest.chob_out" del "DrtTest.chob_out"
if exist "DrtTest.fdn" del "DrtTest.fdn"
if exist "DrtTest.fhd" del "DrtTest.fhd"
if exist "DrtTest.lst" del "DrtTest.lst"
"EnhancedTemplateProcessor.exe" DrtTest.drt.tpl DrtTest.pval
mf2005.exe DrtTest.nam /wait
"Mf2005ObsExtractor.exe" DrtTest.Mf2005ExtractValues
