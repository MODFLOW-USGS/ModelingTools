if exist "arrays\ResTest.Reservoir_Bed_Thickness_1.arrays" del "arrays\ResTest.Reservoir_Bed_Thickness_1.arrays"
if exist "ResTest.Mf2005Values" del "ResTest.Mf2005Values"
if exist "ResTest.resv" del "ResTest.resv"
if exist "ResTest.cbc" del "ResTest.cbc"
if exist "ResTest.fdn" del "ResTest.fdn"
if exist "ResTest.fhd" del "ResTest.fhd"
if exist "ResTest.hob_out" del "ResTest.hob_out"
if exist "ResTest.lst" del "ResTest.lst"
"C:\Pest17.3\plproc.exe" ResTest.Reservoir_Bed_Thickness.script
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" ResTest.resv.tpl ResTest.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe ResTest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" ResTest.Mf2005ExtractValues
