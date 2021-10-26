if exist "arrays\RmaSutraCompleted.Maximum_Hydraulic_Conductivity_1.arrays" del "arrays\RmaSutraCompleted.Maximum_Hydraulic_Conductivity_1.arrays"
if exist "arrays\RmaSutraCompleted.Minimum_Hydraulic_Conductivity_1.arrays" del "arrays\RmaSutraCompleted.Minimum_Hydraulic_Conductivity_1.arrays"
if exist "RmaSutraCompleted.15B" del "RmaSutraCompleted.15B"
if exist "RmaSutraCompleted.FluxBcs" del "RmaSutraCompleted.FluxBcs"
if exist "RmaSutraCompleted.inp" del "RmaSutraCompleted.inp"
if exist "RmaSutraCompleted.bcof" del "RmaSutraCompleted.bcof"
if exist "RmaSutraCompleted.bcop" del "RmaSutraCompleted.bcop"
if exist "RmaSutraCompleted.ele" del "RmaSutraCompleted.ele"
if exist "RmaSutraCompleted.lst" del "RmaSutraCompleted.lst"
if exist "RmaSutraCompleted.nod" del "RmaSutraCompleted.nod"
if exist "RmaSutraCompleted.rst" del "RmaSutraCompleted.rst"
if exist "RmaSutraCompleted.smy" del "RmaSutraCompleted.smy"
if exist "RmaSutraCompleted_Head_Obs1.OBC" del "RmaSutraCompleted_Head_Obs1.OBC"
if exist "RmaSutraCompleted_Head_Obs2.OBC" del "RmaSutraCompleted_Head_Obs2.OBC"
if exist "RmaSutraCompleted_Head_Obs3.OBC" del "RmaSutraCompleted_Head_Obs3.OBC"
if exist "RmaSutraCompleted_Head_Obs4.OBC" del "RmaSutraCompleted_Head_Obs4.OBC"
if exist "RmaSutraCompleted_Head_Obs5.OBC" del "RmaSutraCompleted_Head_Obs5.OBC"
if exist "RmaSutraCompleted_Head_Obs6.OBC" del "RmaSutraCompleted_Head_Obs6.OBC"
if exist "RmaSutraCompleted_Head_Obs7.OBC" del "RmaSutraCompleted_Head_Obs7.OBC"
if exist "RmaSutraCompleted_Head_Obs8.OBC" del "RmaSutraCompleted_Head_Obs8.OBC"
if exist "RmaSutraCompleted_TIME_STEPS.OBS" del "RmaSutraCompleted_TIME_STEPS.OBS"
"C:\Pest17.3\plproc.exe" RmaSutraCompleted.Maximum_Hydraulic_Conductivity.script
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" RmaSutraCompleted.FluxBcs.tpl RmaSutraCompleted.pval
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" arrays\RmaSutraCompleted.Minimum_Hydraulic_Conductivity_1.arrays.tpl
"C:\Pest17.3\plproc.exe" 'RmaSutraCompleted.15B.script'
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" RmaSutraCompleted.inp.tpl RmaSutraCompleted.pval
"C:\SutraSuite\SUTRA_2_2\bin\sutra_2_2.exe"
"C:\ModelingTools\ModelMuse\Debug\Win32\SutraObsExtractor.exe" RmaSutraCompleted.soe_ev
