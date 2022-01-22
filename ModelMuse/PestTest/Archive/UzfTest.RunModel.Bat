if exist "UzfTest.Mf2005Values" del "UzfTest.Mf2005Values"
if exist "UzfTest.uzf" del "UzfTest.uzf"
if exist "UzfTest.cbc" del "UzfTest.cbc"
if exist "UzfTest.chob_out" del "UzfTest.chob_out"
if exist "UzfTest.fdn" del "UzfTest.fdn"
if exist "UzfTest.fhd" del "UzfTest.fhd"
if exist "UzfTest.lst" del "UzfTest.lst"
if exist "UzfTest.sfrg1" del "UzfTest.sfrg1"
if exist "UzfTest.sfrg2" del "UzfTest.sfrg2"
"EnhancedTemplateProcessor.exe" UzfTest.uzf.tpl UzfTest.pval
mf2005.exe UzfTest.nam /wait
"Mf2005ObsExtractor.exe" UzfTest.Mf2005ExtractValues
