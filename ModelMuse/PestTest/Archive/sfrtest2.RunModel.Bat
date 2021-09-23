if exist "sfrtest2.Mf2005Values" del "sfrtest2.Mf2005Values"
if exist "sfrtest2.sfr" del "sfrtest2.sfr"
if exist "sfrtest2.cbc" del "sfrtest2.cbc"
if exist "sfrtest2.fdn" del "sfrtest2.fdn"
if exist "sfrtest2.fhd" del "sfrtest2.fhd"
if exist "sfrtest2.hob_out" del "sfrtest2.hob_out"
if exist "sfrtest2.lst" del "sfrtest2.lst"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" sfrtest2.sfr.tpl sfrtest2.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe sfrtest2.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" sfrtest2.Mf2005ExtractValues
