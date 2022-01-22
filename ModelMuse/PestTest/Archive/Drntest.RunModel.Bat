if exist "arrays\Drntest.Kx_1.arrays" del "arrays\Drntest.Kx_1.arrays"
if exist "arrays\Drntest.Kx_2.arrays" del "arrays\Drntest.Kx_2.arrays"
if exist "arrays\Drntest.Kx_3.arrays" del "arrays\Drntest.Kx_3.arrays"
if exist "arrays\Drntest.test_1.arrays" del "arrays\Drntest.test_1.arrays"
if exist "Drntest.drn" del "Drntest.drn"
if exist "Drntest.Mf2005Values" del "Drntest.Mf2005Values"
if exist "Drntest.bdn" del "Drntest.bdn"
if exist "Drntest.hob_out" del "Drntest.hob_out"
if exist "Drntest.lst" del "Drntest.lst"
"plproc.exe" Drntest.Kx.script
"plproc.exe" Drntest.test.script
"EnhancedTemplateProcessor.exe" Drntest.drn.tpl Drntest.pval
mf2005.exe Drntest.nam /wait
"Mf2005ObsExtractor.exe" Drntest.Mf2005ExtractValues
