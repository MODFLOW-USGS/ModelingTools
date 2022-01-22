if exist "Mnw1Test.Mf2005Values" del "Mnw1Test.Mf2005Values"
if exist "Mnw1Test.mnw1" del "Mnw1Test.mnw1"
if exist "Mnw1Test.cbc" del "Mnw1Test.cbc"
if exist "Mnw1Test.drob_out" del "Mnw1Test.drob_out"
if exist "Mnw1Test.fdn" del "Mnw1Test.fdn"
if exist "Mnw1Test.fhd" del "Mnw1Test.fhd"
if exist "Mnw1Test.hob_out" del "Mnw1Test.hob_out"
if exist "Mnw1Test.lst" del "Mnw1Test.lst"
"EnhancedTemplateProcessor.exe" Mnw1Test.mnw1.tpl Mnw1Test.pval
mf2005.exe Mnw1Test.nam /wait
"Mf2005ObsExtractor.exe" Mnw1Test.Mf2005ExtractValues
