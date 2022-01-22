if exist "arrays\Drntest2.Kx_1.arrays" del "arrays\Drntest2.Kx_1.arrays"
if exist "arrays\Drntest2.Kx_2.arrays" del "arrays\Drntest2.Kx_2.arrays"
if exist "arrays\Drntest2.Kx_3.arrays" del "arrays\Drntest2.Kx_3.arrays"
if exist "arrays\Drntest2.test_1.arrays" del "arrays\Drntest2.test_1.arrays"
if exist "Drntest2.drn" del "Drntest2.drn"
if exist "Drntest2.Mf2005Values" del "Drntest2.Mf2005Values"
if exist "Drntest2.bdn" del "Drntest2.bdn"
if exist "Drntest2.hob_out" del "Drntest2.hob_out"
if exist "Drntest2.lst" del "Drntest2.lst"
"plproc.exe" Drntest2.Kx.script
"plproc.exe" Drntest2.test.script
"EnhancedTemplateProcessor.exe" Drntest2.drn.tpl Drntest2.pval
mf2005.exe Drntest2.nam /wait
"Mf2005ObsExtractor.exe" Drntest2.Mf2005ExtractValues
