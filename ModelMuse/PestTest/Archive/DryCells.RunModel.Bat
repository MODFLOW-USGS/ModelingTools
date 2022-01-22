if exist "arrays\DryCells.Kx_1.arrays" del "arrays\DryCells.Kx_1.arrays"
if exist "arrays\DryCells.Kx_2.arrays" del "arrays\DryCells.Kx_2.arrays"
if exist "arrays\DryCells.Kx_3.arrays" del "arrays\DryCells.Kx_3.arrays"
if exist "DryCells.Mf2005Values" del "DryCells.Mf2005Values"
if exist "DryCells.bhd" del "DryCells.bhd"
if exist "DryCells.cbc" del "DryCells.cbc"
if exist "DryCells.drob_out" del "DryCells.drob_out"
if exist "DryCells.fdn" del "DryCells.fdn"
if exist "DryCells.hob_out" del "DryCells.hob_out"
if exist "DryCells.lst" del "DryCells.lst"
if exist "DryCells.sfrg1" del "DryCells.sfrg1"
"plproc.exe" DryCells.Kx.script
mf2005.exe DryCells.nam /wait
"Mf2005ObsExtractor.exe" DryCells.Mf2005ExtractValues
