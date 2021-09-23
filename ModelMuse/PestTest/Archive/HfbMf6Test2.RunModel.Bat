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
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" HfbMf6Test2.chd.tpl HfbMf6Test2.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" HfbMf6Test2.hfb.tpl HfbMf6Test2.pval
C:\WRDAPP\mf6.2.2\bin\mf6.exe
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf6ObsExtractor.exe" HfbMf6Test2.Mf6ExtractValues
