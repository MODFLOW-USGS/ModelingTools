if exist "HfbTest.hfb" del "HfbTest.hfb"
if exist "HfbTest.Mf2005Values" del "HfbTest.Mf2005Values"
if exist "HfbTest.cbc" del "HfbTest.cbc"
if exist "HfbTest.fdn" del "HfbTest.fdn"
if exist "HfbTest.fhd" del "HfbTest.fhd"
if exist "HfbTest.hob_out" del "HfbTest.hob_out"
if exist "HfbTest.lst" del "HfbTest.lst"
"EnhancedTemplateProcessor.exe" HfbTest.hfb.tpl HfbTest.pval
mf2005.exe HfbTest.nam /wait
"Mf2005ObsExtractor.exe" HfbTest.Mf2005ExtractValues
