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
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" RechAddTestMf6.chd.tpl RechAddTestMf6.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" RechAddTestMf6.rch.tpl RechAddTestMf6.pval
C:\WRDAPP\mf6.2.2\bin\mf6.exe
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf6ObsExtractor.exe" RechAddTestMf6.Mf6ExtractValues
