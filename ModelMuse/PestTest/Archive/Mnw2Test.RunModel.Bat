if exist "Mnw2Test.Mf2005Values" del "Mnw2Test.Mf2005Values"
if exist "Mnw2Test.mnw2" del "Mnw2Test.mnw2"
if exist "Mnw2Test.cbc" del "Mnw2Test.cbc"
if exist "Mnw2Test.fdn" del "Mnw2Test.fdn"
if exist "Mnw2Test.fhd" del "Mnw2Test.fhd"
if exist "Mnw2Test.hob_out" del "Mnw2Test.hob_out"
if exist "Mnw2Test.lst" del "Mnw2Test.lst"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" Mnw2Test.mnw2.tpl Mnw2Test.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe Mnw2Test.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" Mnw2Test.Mf2005ExtractValues
