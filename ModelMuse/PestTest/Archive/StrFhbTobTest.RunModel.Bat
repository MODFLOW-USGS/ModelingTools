if exist "StrFhbTobTest.fhb" del "StrFhbTobTest.fhb"
if exist "StrFhbTobTest.Mf2005Values" del "StrFhbTobTest.Mf2005Values"
if exist "StrFhbTobTest.cbc" del "StrFhbTobTest.cbc"
if exist "StrFhbTobTest.fdn" del "StrFhbTobTest.fdn"
if exist "StrFhbTobTest.fhd" del "StrFhbTobTest.fhd"
if exist "StrFhbTobTest.ftl" del "StrFhbTobTest.ftl"
if exist "StrFhbTobTest.lst" del "StrFhbTobTest.lst"
if exist "StrFhbTobTest.stob_out" del "StrFhbTobTest.stob_out"
if exist "StrFhbTobTest.str_flow" del "StrFhbTobTest.str_flow"
"EnhancedTemplateProcessor.exe" StrFhbTobTest.fhb.tpl StrFhbTobTest.pval
mf2005.exe StrFhbTobTest.nam /wait
"Mf2005ObsExtractor.exe" StrFhbTobTest.Mf2005ExtractValues
