if exist "RechAddTest.Mf2005Values" del "RechAddTest.Mf2005Values"
if exist "RechAddTest.rch" del "RechAddTest.rch"
if exist "RechAddTest.cbc" del "RechAddTest.cbc"
if exist "RechAddTest.fdn" del "RechAddTest.fdn"
if exist "RechAddTest.fhd" del "RechAddTest.fhd"
if exist "RechAddTest.hob_out" del "RechAddTest.hob_out"
if exist "RechAddTest.lst" del "RechAddTest.lst"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" RechAddTest.rch.tpl RechAddTest.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe RechAddTest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" RechAddTest.Mf2005ExtractValues
