if exist "GhbTest.ghb" del "GhbTest.ghb"
if exist "GhbTest.Mf2005Values" del "GhbTest.Mf2005Values"
if exist "GhbTest.cbc" del "GhbTest.cbc"
if exist "GhbTest.fdn" del "GhbTest.fdn"
if exist "GhbTest.fhd" del "GhbTest.fhd"
if exist "GhbTest.hob_out" del "GhbTest.hob_out"
if exist "GhbTest.lst" del "GhbTest.lst"
"EnhancedTemplateProcessor.exe" GhbTest.ghb.tpl GhbTest.pval
mf2005.exe GhbTest.nam /wait
"Mf2005ObsExtractor.exe" GhbTest.Mf2005ExtractValues
