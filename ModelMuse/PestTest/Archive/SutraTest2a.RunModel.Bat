if exist "SutraTest2a.FluxBcs" del "SutraTest2a.FluxBcs"
if exist "SutraTest2a.inp" del "SutraTest2a.inp"
if exist "SutraTest2a.SPecPBcs" del "SutraTest2a.SPecPBcs"
if exist "SutraTest2a.bcof" del "SutraTest2a.bcof"
if exist "SutraTest2a.bcop" del "SutraTest2a.bcop"
if exist "SutraTest2a.bcos" del "SutraTest2a.bcos"
if exist "SutraTest2a.bcou" del "SutraTest2a.bcou"
if exist "SutraTest2a.ele" del "SutraTest2a.ele"
if exist "SutraTest2a.lst" del "SutraTest2a.lst"
if exist "SutraTest2a.nod" del "SutraTest2a.nod"
if exist "SutraTest2a.rst" del "SutraTest2a.rst"
if exist "SutraTest2a.smy" del "SutraTest2a.smy"
if exist "SutraTest2a_Object13.OBC" del "SutraTest2a_Object13.OBC"
if exist "SutraTest2a_Observatio.OBC" del "SutraTest2a_Observatio.OBC"
if exist "SutraTest2a_ObsSchec.OBS" del "SutraTest2a_ObsSchec.OBS"
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest2a.FluxBcs.tpl SutraTest2a.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest2a.SPecPBcs.tpl SutraTest2a.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" SutraTest2a.inp.tpl SutraTest2a.pval
"C:\SutraSuite\SUTRA_2_2\bin\sutra_2_2.exe"
"C:\ModelingTools\ModelMuse\Debug\Win32\SutraObsExtractor.exe" SutraTest2a.soe_ev
