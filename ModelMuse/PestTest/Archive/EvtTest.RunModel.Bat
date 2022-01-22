if exist "EvtTest.evt" del "EvtTest.evt"
if exist "EvtTest.Mf2005Values" del "EvtTest.Mf2005Values"
if exist "EvtTest.cbc" del "EvtTest.cbc"
if exist "EvtTest.fdn" del "EvtTest.fdn"
if exist "EvtTest.fhd" del "EvtTest.fhd"
if exist "EvtTest.lst" del "EvtTest.lst"
if exist "EvtTest.rvob_out" del "EvtTest.rvob_out"
"EnhancedTemplateProcessor.exe" EvtTest.evt.tpl EvtTest.pval
mf2005.exe EvtTest.nam /wait
"Mf2005ObsExtractor.exe" EvtTest.Mf2005ExtractValues
