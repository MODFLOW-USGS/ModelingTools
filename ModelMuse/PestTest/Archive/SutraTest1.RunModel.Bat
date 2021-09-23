if exist "SutraTest1.FluxBcs" del "SutraTest1.FluxBcs"
if exist "SutraTest1.inp" del "SutraTest1.inp"
if exist "SutraTest1.SPecPBcs" del "SutraTest1.SPecPBcs"
if exist "SutraTest1.bcof" del "SutraTest1.bcof"
if exist "SutraTest1.bcop" del "SutraTest1.bcop"
if exist "SutraTest1.bcos" del "SutraTest1.bcos"
if exist "SutraTest1.bcou" del "SutraTest1.bcou"
if exist "SutraTest1.ele" del "SutraTest1.ele"
if exist "SutraTest1.lst" del "SutraTest1.lst"
if exist "SutraTest1.nod" del "SutraTest1.nod"
if exist "SutraTest1.rst" del "SutraTest1.rst"
if exist "SutraTest1.smy" del "SutraTest1.smy"
if exist "SutraTest1_FluidFlux1.OBC" del "SutraTest1_FluidFlux1.OBC"
if exist "SutraTest1_Observatio.OBC" del "SutraTest1_Observatio.OBC"
if exist "SutraTest1_ObsSched1.OBS" del "SutraTest1_ObsSched1.OBS"
if exist "SutraTest1_ObsSched2.OBS" del "SutraTest1_ObsSched2.OBS"
if exist "SutraTest1_SCHED_1.OBC" del "SutraTest1_SCHED_1.OBC"
if exist "SutraTest1_TIME_STEPS.OBC" del "SutraTest1_TIME_STEPS.OBC"
if exist "SutraTest1_TIME_STEPS.OBS" del "SutraTest1_TIME_STEPS.OBS"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest1.FluxBcs.tpl SutraTest1.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest1.SPecPBcs.tpl SutraTest1.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest1.inp.tpl SutraTest1.pval
"C:\SutraSuite\SUTRA_2_2\bin\sutra_2_2.exe"
"C:\ModelingTools\ModelMuse\Debug\Win32\SutraObsExtractor.exe" SutraTest1.soe_ev
