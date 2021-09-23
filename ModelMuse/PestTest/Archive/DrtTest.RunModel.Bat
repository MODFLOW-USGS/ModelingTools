if exist "DrtTest.drt" del "DrtTest.drt"
if exist "DrtTest.Mf2005Values" del "DrtTest.Mf2005Values"
if exist "DrtTest.cbc" del "DrtTest.cbc"
if exist "DrtTest.chob_out" del "DrtTest.chob_out"
if exist "DrtTest.fdn" del "DrtTest.fdn"
if exist "DrtTest.fhd" del "DrtTest.fhd"
if exist "DrtTest.lst" del "DrtTest.lst"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" DrtTest.drt.tpl DrtTest.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe DrtTest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" DrtTest.Mf2005ExtractValues
