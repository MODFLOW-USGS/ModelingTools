if exist "arrays\SutraTest1a.Maximum_Permeability_1.arrays" del "arrays\SutraTest1a.Maximum_Permeability_1.arrays"
if exist "arrays\SutraTest1a.Maximum_Permeability_2.arrays" del "arrays\SutraTest1a.Maximum_Permeability_2.arrays"
if exist "arrays\SutraTest1a.Maximum_Permeability_3.arrays" del "arrays\SutraTest1a.Maximum_Permeability_3.arrays"
if exist "arrays\SutraTest1a.Middle_Permeability_1.arrays" del "arrays\SutraTest1a.Middle_Permeability_1.arrays"
if exist "arrays\SutraTest1a.Middle_Permeability_2.arrays" del "arrays\SutraTest1a.Middle_Permeability_2.arrays"
if exist "arrays\SutraTest1a.Middle_Permeability_3.arrays" del "arrays\SutraTest1a.Middle_Permeability_3.arrays"
if exist "arrays\SutraTest1a.Minimum_Permeability_1.arrays" del "arrays\SutraTest1a.Minimum_Permeability_1.arrays"
if exist "arrays\SutraTest1a.Minimum_Permeability_2.arrays" del "arrays\SutraTest1a.Minimum_Permeability_2.arrays"
if exist "arrays\SutraTest1a.Minimum_Permeability_3.arrays" del "arrays\SutraTest1a.Minimum_Permeability_3.arrays"
if exist "arrays\SutraTest1a.MyMult_1.arrays" del "arrays\SutraTest1a.MyMult_1.arrays"
if exist "arrays\SutraTest1a.MyMult_2.arrays" del "arrays\SutraTest1a.MyMult_2.arrays"
if exist "arrays\SutraTest1a.MyMult_3.arrays" del "arrays\SutraTest1a.MyMult_3.arrays"
if exist "arrays\SutraTest1a.MyMult_4.arrays" del "arrays\SutraTest1a.MyMult_4.arrays"
if exist "SutraTest1a.14B_1" del "SutraTest1a.14B_1"
if exist "SutraTest1a.14B_2" del "SutraTest1a.14B_2"
if exist "SutraTest1a.14B_3" del "SutraTest1a.14B_3"
if exist "SutraTest1a.14B_4" del "SutraTest1a.14B_4"
if exist "SutraTest1a.15B_1" del "SutraTest1a.15B_1"
if exist "SutraTest1a.15B_2" del "SutraTest1a.15B_2"
if exist "SutraTest1a.15B_3" del "SutraTest1a.15B_3"
if exist "SutraTest1a.FluxBcs" del "SutraTest1a.FluxBcs"
if exist "SutraTest1a.inp" del "SutraTest1a.inp"
if exist "SutraTest1a.SPecPBcs" del "SutraTest1a.SPecPBcs"
if exist "SutraTest1a.bcof" del "SutraTest1a.bcof"
if exist "SutraTest1a.bcop" del "SutraTest1a.bcop"
if exist "SutraTest1a.bcos" del "SutraTest1a.bcos"
if exist "SutraTest1a.bcou" del "SutraTest1a.bcou"
if exist "SutraTest1a.ele" del "SutraTest1a.ele"
if exist "SutraTest1a.lst" del "SutraTest1a.lst"
if exist "SutraTest1a.nod" del "SutraTest1a.nod"
if exist "SutraTest1a.rst" del "SutraTest1a.rst"
if exist "SutraTest1a.smy" del "SutraTest1a.smy"
if exist "SutraTest1a_FluidFlux1.OBC" del "SutraTest1a_FluidFlux1.OBC"
if exist "SutraTest1a_Observatio.OBC" del "SutraTest1a_Observatio.OBC"
if exist "SutraTest1a_ObsSched1.OBS" del "SutraTest1a_ObsSched1.OBS"
if exist "SutraTest1a_ObsSched2.OBS" del "SutraTest1a_ObsSched2.OBS"
if exist "SutraTest1a_SCHED_1.OBC" del "SutraTest1a_SCHED_1.OBC"
if exist "SutraTest1a_TIME_STEPS.OBC" del "SutraTest1a_TIME_STEPS.OBC"
if exist "SutraTest1a_TIME_STEPS.OBS" del "SutraTest1a_TIME_STEPS.OBS"
"C:\Pest17.3\plproc.exe" SutraTest1a.Nodal_Porosity.script
"C:\Pest17.3\plproc.exe" SutraTest1a.Maximum_Permeability.script
"C:\Pest17.3\plproc.exe" SutraTest1a.MyMult.script
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest1a.FluxBcs.tpl SutraTest1a.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest1a.SPecPBcs.tpl SutraTest1a.pval
"C:\Pest17.3\plproc.exe" 'SutraTest1a.14B.script'
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Middle_Permeability_1.arrays.tpl
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Middle_Permeability_2.arrays.tpl
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Middle_Permeability_3.arrays.tpl
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Minimum_Permeability_1.arrays.tpl
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Minimum_Permeability_2.arrays.tpl
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\SutraTest1a.Minimum_Permeability_3.arrays.tpl
"C:\Pest17.3\plproc.exe" 'SutraTest1a.15B.script'
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest1a.inp.tpl SutraTest1a.pval
"C:\SutraSuite\SUTRA_2_2\bin\sutra_2_2.exe"
"C:\ModelingTools\ModelMuse\Debug\Win32\SutraObsExtractor.exe" SutraTest1a.soe_ev