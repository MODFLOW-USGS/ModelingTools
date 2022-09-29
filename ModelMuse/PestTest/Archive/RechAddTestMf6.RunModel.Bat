if exist "RechAddTestMf6.chd" del "RechAddTestMf6.chd"
if exist "RechAddTestMf6.Mf6Values" del "RechAddTestMf6.Mf6Values"
if exist "RechAddTestMf6.rch" del "RechAddTestMf6.rch"
if exist "mfsim.lst" del "mfsim.lst"
if exist "RechAddTestMf6.bhd" del "RechAddTestMf6.bhd"
if exist "RechAddTestMf6.cbc" del "RechAddTestMf6.cbc"
if exist "RechAddTestMf6.InnerSolution.CSV" del "RechAddTestMf6.InnerSolution.CSV"
if exist "RechAddTestMf6.lst" del "RechAddTestMf6.lst"
if exist "RechAddTestMf6.ob_gw_out_head.csv" del "RechAddTestMf6.ob_gw_out_head.csv"
if exist "RechAddTestMf6.OuterSolution.CSV" del "RechAddTestMf6.OuterSolution.CSV"
"EnhancedTemplateProcessor.exe" RechAddTestMf6.chd.tpl RechAddTestMf6.pval
"EnhancedTemplateProcessor.exe" RechAddTestMf6.rch.tpl RechAddTestMf6.pval
Copy /Y /A "RechAddTestMf6.mfsim.nam" "mfsim.nam"
mf6.exe
Copy /Y /A "mfsim.lst" "RechAddTestMf6.mfsim.lst"
"Mf6ObsExtractor.exe" RechAddTestMf6.Mf6ExtractValues
