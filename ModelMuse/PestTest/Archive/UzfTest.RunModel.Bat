if exist "UzfTest.Mf2005Values" del "UzfTest.Mf2005Values"
if exist "UzfTest.sfr" del "UzfTest.sfr"
if exist "UzfTest.uzf" del "UzfTest.uzf"
if exist "UzfTest.cbc" del "UzfTest.cbc"
if exist "UzfTest.chob_out" del "UzfTest.chob_out"
if exist "UzfTest.fdn" del "UzfTest.fdn"
if exist "UzfTest.fhd" del "UzfTest.fhd"
if exist "UzfTest.lst" del "UzfTest.lst"
if exist "UzfTest.sfrg1" del "UzfTest.sfrg1"
if exist "UzfTest.sfrg2" del "UzfTest.sfrg2"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" UzfTest.sfr.tpl UzfTest.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" UzfTest.uzf.tpl UzfTest.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe UzfTest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" UzfTest.Mf2005ExtractValues
