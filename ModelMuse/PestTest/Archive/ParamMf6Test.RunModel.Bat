if exist "ParamMf6Test.chd" del "ParamMf6Test.chd"
if exist "ParamMf6Test.drn" del "ParamMf6Test.drn"
if exist "ParamMf6Test.evt" del "ParamMf6Test.evt"
if exist "ParamMf6Test.ghb" del "ParamMf6Test.ghb"
if exist "ParamMf6Test.Mf6Values" del "ParamMf6Test.Mf6Values"
if exist "ParamMf6Test.rch" del "ParamMf6Test.rch"
if exist "ParamMf6Test.riv" del "ParamMf6Test.riv"
if exist "ParamMf6Test.wel" del "ParamMf6Test.wel"
if exist "mfsim.lst" del "mfsim.lst"
if exist "ParamMf6Test.bhd" del "ParamMf6Test.bhd"
if exist "ParamMf6Test.cbc" del "ParamMf6Test.cbc"
if exist "ParamMf6Test.InnerSolution.CSV" del "ParamMf6Test.InnerSolution.CSV"
if exist "ParamMf6Test.lst" del "ParamMf6Test.lst"
if exist "ParamMf6Test.ob_gw_out_head.csv" del "ParamMf6Test.ob_gw_out_head.csv"
if exist "ParamMf6Test.OuterSolution.CSV" del "ParamMf6Test.OuterSolution.CSV"
"EnhancedTemplateProcessor.exe" ParamMf6Test.chd.tpl ParamMf6Test.pval
"EnhancedTemplateProcessor.exe" ParamMf6Test.ghb.tpl ParamMf6Test.pval
"EnhancedTemplateProcessor.exe" ParamMf6Test.wel.tpl ParamMf6Test.pval
"EnhancedTemplateProcessor.exe" ParamMf6Test.riv.tpl ParamMf6Test.pval
"EnhancedTemplateProcessor.exe" ParamMf6Test.drn.tpl ParamMf6Test.pval
"EnhancedTemplateProcessor.exe" ParamMf6Test.rch.tpl ParamMf6Test.pval
"EnhancedTemplateProcessor.exe" ParamMf6Test.evt.tpl ParamMf6Test.pval
mf6.exe
"Mf6ObsExtractor.exe" ParamMf6Test.Mf6ExtractValues
