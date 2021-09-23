if exist "HfbTest.hfb" del "HfbTest.hfb"
if exist "HfbTest.Mf2005Values" del "HfbTest.Mf2005Values"
if exist "HfbTest.cbc" del "HfbTest.cbc"
if exist "HfbTest.fdn" del "HfbTest.fdn"
if exist "HfbTest.fhd" del "HfbTest.fhd"
if exist "HfbTest.hob_out" del "HfbTest.hob_out"
if exist "HfbTest.lst" del "HfbTest.lst"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" HfbTest.hfb.tpl HfbTest.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe HfbTest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" HfbTest.Mf2005ExtractValues
