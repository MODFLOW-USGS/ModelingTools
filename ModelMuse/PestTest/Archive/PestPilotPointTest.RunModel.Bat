if exist "arrays\PestPilotPointTest.Kx_1.arrays" del "arrays\PestPilotPointTest.Kx_1.arrays"
if exist "arrays\PestPilotPointTest.Kx_2.arrays" del "arrays\PestPilotPointTest.Kx_2.arrays"
if exist "arrays\PestPilotPointTest.Kx_3.arrays" del "arrays\PestPilotPointTest.Kx_3.arrays"
if exist "arrays\PestPilotPointTest.Ky_1.arrays" del "arrays\PestPilotPointTest.Ky_1.arrays"
if exist "arrays\PestPilotPointTest.Ky_2.arrays" del "arrays\PestPilotPointTest.Ky_2.arrays"
if exist "arrays\PestPilotPointTest.Ky_3.arrays" del "arrays\PestPilotPointTest.Ky_3.arrays"
if exist "PestPilotPointTest.drn" del "PestPilotPointTest.drn"
if exist "PestPilotPointTest.Mf6Values" del "PestPilotPointTest.Mf6Values"
if exist "mfsim.lst" del "mfsim.lst"
if exist "PestPilotPointTest.bhd" del "PestPilotPointTest.bhd"
if exist "PestPilotPointTest.cbc" del "PestPilotPointTest.cbc"
if exist "PestPilotPointTest.drob_out_drn.csv" del "PestPilotPointTest.drob_out_drn.csv"
if exist "PestPilotPointTest.InnerSolution.CSV" del "PestPilotPointTest.InnerSolution.CSV"
if exist "PestPilotPointTest.lst" del "PestPilotPointTest.lst"
if exist "PestPilotPointTest.ob_gw_out_head.csv" del "PestPilotPointTest.ob_gw_out_head.csv"
if exist "PestPilotPointTest.OuterSolution.CSV" del "PestPilotPointTest.OuterSolution.CSV"
"plproc.exe" PestPilotPointTest.Kx.script
"plproc.exe" PestPilotPointTest.Ky.script
"EnhancedTemplateProcessor.exe" PestPilotPointTest.drn.tpl PestPilotPointTest.pval
Copy /Y /A "PestPilotPointTest.mfsim.nam" "mfsim.nam"
mf6.exe
Copy /Y /A "mfsim.lst" "PestPilotPointTest.mfsim.lst"
"Mf6ObsExtractor.exe" PestPilotPointTest.Mf6ExtractValues
