if exist "arrays\StreamTest1.Kx_1.arrays" del "arrays\StreamTest1.Kx_1.arrays"
if exist "arrays\StreamTest1.Kx_2.arrays" del "arrays\StreamTest1.Kx_2.arrays"
if exist "arrays\StreamTest1.Kx_3.arrays" del "arrays\StreamTest1.Kx_3.arrays"
if exist "StreamTest1.Mf2005Values" del "StreamTest1.Mf2005Values"
if exist "StreamTest1.str" del "StreamTest1.str"
if exist "StreamTest1.cbc" del "StreamTest1.cbc"
if exist "StreamTest1.fdn" del "StreamTest1.fdn"
if exist "StreamTest1.fhd" del "StreamTest1.fhd"
if exist "StreamTest1.lst" del "StreamTest1.lst"
if exist "StreamTest1.stob_out" del "StreamTest1.stob_out"
if exist "StreamTest1.str_flow" del "StreamTest1.str_flow"
"C:\Pest17.3\plproc.exe" StreamTest1.Kx.script
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" StreamTest1.str.tpl StreamTest1.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe StreamTest1.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" StreamTest1.Mf2005ExtractValues
