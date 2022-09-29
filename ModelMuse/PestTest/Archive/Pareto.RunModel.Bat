if exist "Pareto.chd" del "Pareto.chd"
if exist "Pareto.drn" del "Pareto.drn"
if exist "Pareto.evt" del "Pareto.evt"
if exist "Pareto.ghb" del "Pareto.ghb"
if exist "Pareto.Mf6Values" del "Pareto.Mf6Values"
if exist "Pareto.rch" del "Pareto.rch"
if exist "Pareto.riv" del "Pareto.riv"
if exist "Pareto.wel" del "Pareto.wel"
if exist "mfsim.lst" del "mfsim.lst"
if exist "Pareto.bhd" del "Pareto.bhd"
if exist "Pareto.cbc" del "Pareto.cbc"
if exist "Pareto.InnerSolution.CSV" del "Pareto.InnerSolution.CSV"
if exist "Pareto.lst" del "Pareto.lst"
if exist "Pareto.ob_gw_out_head.csv" del "Pareto.ob_gw_out_head.csv"
if exist "Pareto.OuterSolution.CSV" del "Pareto.OuterSolution.CSV"
"EnhancedTemplateProcessor.exe" Pareto.chd.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.ghb.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.wel.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.riv.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.drn.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.rch.tpl Pareto.pval
"EnhancedTemplateProcessor.exe" Pareto.evt.tpl Pareto.pval
Copy /Y /A "Pareto.mfsim.nam" "mfsim.nam"
mf6.exe
Copy /Y /A "mfsim.lst" "Pareto.mfsim.lst"
"Mf6ObsExtractor.exe" Pareto.Mf6ExtractValues
