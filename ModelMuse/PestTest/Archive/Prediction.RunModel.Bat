if exist "Prediction.chd" del "Prediction.chd"
if exist "Prediction.drn" del "Prediction.drn"
if exist "Prediction.evt" del "Prediction.evt"
if exist "Prediction.ghb" del "Prediction.ghb"
if exist "Prediction.Mf6Values" del "Prediction.Mf6Values"
if exist "Prediction.rch" del "Prediction.rch"
if exist "Prediction.riv" del "Prediction.riv"
if exist "Prediction.wel" del "Prediction.wel"
if exist "mfsim.lst" del "mfsim.lst"
if exist "Prediction.bhd" del "Prediction.bhd"
if exist "Prediction.cbc" del "Prediction.cbc"
if exist "Prediction.InnerSolution.CSV" del "Prediction.InnerSolution.CSV"
if exist "Prediction.lst" del "Prediction.lst"
if exist "Prediction.ob_gw_out_head.csv" del "Prediction.ob_gw_out_head.csv"
if exist "Prediction.OuterSolution.CSV" del "Prediction.OuterSolution.CSV"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" Prediction.chd.tpl Prediction.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" Prediction.ghb.tpl Prediction.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" Prediction.wel.tpl Prediction.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" Prediction.riv.tpl Prediction.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" Prediction.drn.tpl Prediction.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" Prediction.rch.tpl Prediction.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" Prediction.evt.tpl Prediction.pval
C:\WRDAPP\mf6.2.2\bin\mf6.exe
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf6ObsExtractor.exe" Prediction.Mf6ExtractValues
