if exist "StrFhbTobTest.fhb" del "StrFhbTobTest.fhb"
if exist "StrFhbTobTest.Mf2005Values" del "StrFhbTobTest.Mf2005Values"
if exist "StrFhbTobTest.cbc" del "StrFhbTobTest.cbc"
if exist "StrFhbTobTest.fdn" del "StrFhbTobTest.fdn"
if exist "StrFhbTobTest.fhd" del "StrFhbTobTest.fhd"
if exist "StrFhbTobTest.ftl" del "StrFhbTobTest.ftl"
if exist "StrFhbTobTest.lst" del "StrFhbTobTest.lst"
if exist "StrFhbTobTest.stob_out" del "StrFhbTobTest.stob_out"
if exist "StrFhbTobTest.str_flow" del "StrFhbTobTest.str_flow"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" StrFhbTobTest.fhb.tpl StrFhbTobTest.pval
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe StrFhbTobTest.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" StrFhbTobTest.Mf2005ExtractValues
