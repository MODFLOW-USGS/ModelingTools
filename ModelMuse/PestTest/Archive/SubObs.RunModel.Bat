if exist "arrays\SubObs.Layer1_D_Delay_Interbed_Equivalent_Thickness_1.arrays" del "arrays\SubObs.Layer1_D_Delay_Interbed_Equivalent_Thickness_1.arrays"
if exist "SubObs.Mf2005Values" del "SubObs.Mf2005Values"
if exist "SubObs.cbc" del "SubObs.cbc"
if exist "SubObs.fdn" del "SubObs.fdn"
if exist "SubObs.fhd" del "SubObs.fhd"
if exist "SubObs.lst" del "SubObs.lst"
if exist "SubObs.SubComIsOut" del "SubObs.SubComIsOut"
if exist "SubObs.SubComMlOut" del "SubObs.SubComMlOut"
if exist "SubObs.SubSubOut" del "SubObs.SubSubOut"
"C:\Pest17.3\plproc.exe" SubObs.Layer1_D_Delay_Interbed_Equivalent_Thickness.script
C:\WRDAPP\MF2005.1_12\bin\mf2005.exe SubObs.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" SubObs.Mf2005ExtractValues
