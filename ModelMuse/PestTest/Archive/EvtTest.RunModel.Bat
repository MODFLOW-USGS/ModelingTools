if exist "EvtTest.evt" del "EvtTest.evt"
if exist "EvtTest.Mf2005Values" del "EvtTest.Mf2005Values"
if exist "EvtTest.cbc" del "EvtTest.cbc"
if exist "EvtTest.fdn" del "EvtTest.fdn"
if exist "EvtTest.fhd" del "EvtTest.fhd"
if exist "EvtTest.lst" del "EvtTest.lst"
if exist "EvtTest.rvob_out" del "EvtTest.rvob_out"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" EvtTest.evt.tpl EvtTest.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe EvtTest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" EvtTest.Mf2005ExtractValues
