if exist "HfbMf6Test2.chd" del "HfbMf6Test2.chd"
if exist "HfbMf6Test2.hfb" del "HfbMf6Test2.hfb"
if exist "HfbMf6Test2.Mf6Values" del "HfbMf6Test2.Mf6Values"
if exist "HfbMf6Test2.bhd" del "HfbMf6Test2.bhd"
if exist "HfbMf6Test2.cbc" del "HfbMf6Test2.cbc"
if exist "HfbMf6Test2.InnerSolution.CSV" del "HfbMf6Test2.InnerSolution.CSV"
if exist "HfbMf6Test2.lst" del "HfbMf6Test2.lst"
if exist "HfbMf6Test2.ob_gw_out_head.csv" del "HfbMf6Test2.ob_gw_out_head.csv"
if exist "HfbMf6Test2.OuterSolution.CSV" del "HfbMf6Test2.OuterSolution.CSV"
if exist "mfsim.lst" del "mfsim.lst"
"EnhancedTemplateProcessor.exe" HfbMf6Test2.chd.tpl HfbMf6Test2.pval
"EnhancedTemplateProcessor.exe" HfbMf6Test2.hfb.tpl HfbMf6Test2.pval
Copy /Y /A "HfbMf6Test2.mfsim.nam" "mfsim.nam"
mf6.exe
Copy /Y /A "mfsim.lst" "HfbMf6Test2.mfsim.lst"
"Mf6ObsExtractor.exe" HfbMf6Test2.Mf6ExtractValues
