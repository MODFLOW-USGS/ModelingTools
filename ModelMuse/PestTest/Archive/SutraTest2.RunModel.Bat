if exist "SutraTest2.FluxBcs" del "SutraTest2.FluxBcs"
if exist "SutraTest2.inp" del "SutraTest2.inp"
if exist "SutraTest2.SPecPBcs" del "SutraTest2.SPecPBcs"
if exist "SutraTest2.SPecUBcs" del "SutraTest2.SPecUBcs"
if exist "SutraTest2.UFluxBcs" del "SutraTest2.UFluxBcs"
if exist "SutraTest2.bcof" del "SutraTest2.bcof"
if exist "SutraTest2.bcop" del "SutraTest2.bcop"
if exist "SutraTest2.bcos" del "SutraTest2.bcos"
if exist "SutraTest2.bcou" del "SutraTest2.bcou"
if exist "SutraTest2.ele" del "SutraTest2.ele"
if exist "SutraTest2.lst" del "SutraTest2.lst"
if exist "SutraTest2.nod" del "SutraTest2.nod"
if exist "SutraTest2.rst" del "SutraTest2.rst"
if exist "SutraTest2.smy" del "SutraTest2.smy"
if exist "SutraTest2_Object13.OBC" del "SutraTest2_Object13.OBC"
if exist "SutraTest2_Observatio.OBC" del "SutraTest2_Observatio.OBC"
if exist "SutraTest2_ObsSchec.OBS" del "SutraTest2_ObsSchec.OBS"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest2.FluxBcs.tpl SutraTest2.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest2.UFluxBcs.tpl SutraTest2.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest2.SPecPBcs.tpl SutraTest2.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest2.SPecUBcs.tpl SutraTest2.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest2.inp.tpl SutraTest2.pval
"C:\SutraSuite\SUTRA_2_2\bin\sutra_2_2.exe"
"C:\ModelingTools\ModelMuse\Debug\Win32\SutraObsExtractor.exe" SutraTest2.soe_ev
