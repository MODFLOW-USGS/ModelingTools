if exist "arrays\WellTest.Horizontal_Anisotropy_1.arrays" del "arrays\WellTest.Horizontal_Anisotropy_1.arrays"
if exist "arrays\WellTest.Horizontal_Anisotropy_2.arrays" del "arrays\WellTest.Horizontal_Anisotropy_2.arrays"
if exist "arrays\WellTest.Horizontal_Anisotropy_3.arrays" del "arrays\WellTest.Horizontal_Anisotropy_3.arrays"
if exist "WellTest.Mf2005Values" del "WellTest.Mf2005Values"
if exist "WellTest.wel" del "WellTest.wel"
if exist "WellTest.cbc" del "WellTest.cbc"
if exist "WellTest.chob_out" del "WellTest.chob_out"
if exist "WellTest.fdn" del "WellTest.fdn"
if exist "WellTest.fhd" del "WellTest.fhd"
if exist "WellTest.lst" del "WellTest.lst"
"plproc.exe" WellTest.Horizontal_Anisotropy.script
"EnhancedTemplateProcessor.exe" WellTest.wel.tpl WellTest.pval
mf2005.exe WellTest.nam /wait
"Mf2005ObsExtractor.exe" WellTest.Mf2005ExtractValues
