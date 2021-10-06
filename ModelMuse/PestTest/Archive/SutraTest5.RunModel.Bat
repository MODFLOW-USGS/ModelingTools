if exist "arrays\SutraTest5.Maximum_Permeability_1.arrays" del "arrays\SutraTest5.Maximum_Permeability_1.arrays"
if exist "arrays\SutraTest5.Maximum_Permeability_2.arrays" del "arrays\SutraTest5.Maximum_Permeability_2.arrays"
if exist "arrays\SutraTest5.Maximum_Permeability_3.arrays" del "arrays\SutraTest5.Maximum_Permeability_3.arrays"
if exist "arrays\SutraTest5.Middle_Permeability_1.arrays" del "arrays\SutraTest5.Middle_Permeability_1.arrays"
if exist "arrays\SutraTest5.Middle_Permeability_2.arrays" del "arrays\SutraTest5.Middle_Permeability_2.arrays"
if exist "arrays\SutraTest5.Middle_Permeability_3.arrays" del "arrays\SutraTest5.Middle_Permeability_3.arrays"
if exist "arrays\SutraTest5.Minimum_Permeability_1.arrays" del "arrays\SutraTest5.Minimum_Permeability_1.arrays"
if exist "arrays\SutraTest5.Minimum_Permeability_2.arrays" del "arrays\SutraTest5.Minimum_Permeability_2.arrays"
if exist "arrays\SutraTest5.Minimum_Permeability_3.arrays" del "arrays\SutraTest5.Minimum_Permeability_3.arrays"
if exist "SutraTest5.15B_1" del "SutraTest5.15B_1"
if exist "SutraTest5.15B_2" del "SutraTest5.15B_2"
if exist "SutraTest5.15B_3" del "SutraTest5.15B_3"
if exist "SutraTest5.bcof" del "SutraTest5.bcof"
if exist "SutraTest5.bcop" del "SutraTest5.bcop"
if exist "SutraTest5.bcos" del "SutraTest5.bcos"
if exist "SutraTest5.bcou" del "SutraTest5.bcou"
if exist "SutraTest5.ele" del "SutraTest5.ele"
if exist "SutraTest5.lst" del "SutraTest5.lst"
if exist "SutraTest5.nod" del "SutraTest5.nod"
if exist "SutraTest5.rst" del "SutraTest5.rst"
if exist "SutraTest5.smy" del "SutraTest5.smy"
if exist "SutraTest5_Observatio.OBC" del "SutraTest5_Observatio.OBC"
if exist "SutraTest5_ObsSchec.OBS" del "SutraTest5_ObsSchec.OBS"
"C:\Pest17.3\plproc.exe" SutraTest5.Maximum_Permeability.script
"C:\Pest17.3\plproc.exe" SutraTest5.Minimum_Permeability.script
"C:\Pest17.3\plproc.exe" SutraTest5.Middle_Permeability.script
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest5.Middle_Permeability_1.arrays.tpl
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest5.Middle_Permeability_2.arrays.tpl
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest5.Middle_Permeability_3.arrays.tpl
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest5.Minimum_Permeability_1.arrays.tpl
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest5.Minimum_Permeability_2.arrays.tpl
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest5.Minimum_Permeability_3.arrays.tpl
"C:\Pest17.3\plproc.exe" 'SutraTest5.15B.script'
"C:\SutraSuite\SUTRA_2_2\bin\sutra_2_2.exe"
"C:\ModelingTools\ModelMuse\Debug\Win32\SutraObsExtractor.exe" SutraTest5.soe_ev
