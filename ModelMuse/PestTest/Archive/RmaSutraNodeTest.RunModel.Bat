if exist "arrays\RmaSutraNodeTest.Maximum_Hydraulic_Conductivity_1.arrays" del "arrays\RmaSutraNodeTest.Maximum_Hydraulic_Conductivity_1.arrays"
if exist "arrays\RmaSutraNodeTest.Minimum_Hydraulic_Conductivity_1.arrays" del "arrays\RmaSutraNodeTest.Minimum_Hydraulic_Conductivity_1.arrays"
if exist "RmaSutraNodeTest.15B" del "RmaSutraNodeTest.15B"
if exist "RmaSutraNodeTest.FluxBcs" del "RmaSutraNodeTest.FluxBcs"
if exist "RmaSutraNodeTest.inp" del "RmaSutraNodeTest.inp"
if exist "RmaSutraNodeTest.bcof" del "RmaSutraNodeTest.bcof"
if exist "RmaSutraNodeTest.bcop" del "RmaSutraNodeTest.bcop"
if exist "RmaSutraNodeTest.ele" del "RmaSutraNodeTest.ele"
if exist "RmaSutraNodeTest.lst" del "RmaSutraNodeTest.lst"
if exist "RmaSutraNodeTest.nod" del "RmaSutraNodeTest.nod"
if exist "RmaSutraNodeTest.rst" del "RmaSutraNodeTest.rst"
if exist "RmaSutraNodeTest.smy" del "RmaSutraNodeTest.smy"
if exist "RmaSutraNodeTest_Head_Obs1.OBC" del "RmaSutraNodeTest_Head_Obs1.OBC"
if exist "RmaSutraNodeTest_Head_Obs2.OBC" del "RmaSutraNodeTest_Head_Obs2.OBC"
if exist "RmaSutraNodeTest_Head_Obs3.OBC" del "RmaSutraNodeTest_Head_Obs3.OBC"
if exist "RmaSutraNodeTest_Head_Obs4.OBC" del "RmaSutraNodeTest_Head_Obs4.OBC"
if exist "RmaSutraNodeTest_Head_Obs5.OBC" del "RmaSutraNodeTest_Head_Obs5.OBC"
if exist "RmaSutraNodeTest_Head_Obs6.OBC" del "RmaSutraNodeTest_Head_Obs6.OBC"
if exist "RmaSutraNodeTest_Head_Obs7.OBC" del "RmaSutraNodeTest_Head_Obs7.OBC"
if exist "RmaSutraNodeTest_Head_Obs8.OBC" del "RmaSutraNodeTest_Head_Obs8.OBC"
"C:\Pest17.3\plproc.exe" RmaSutraNodeTest.Maximum_Hydraulic_Conductivity.script
"C:\Pest17.3\plproc.exe" RmaSutraNodeTest.Minimum_Hydraulic_Conductivity.script
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" RmaSutraNodeTest.FluxBcs.tpl RmaSutraNodeTest.pval
"C:\Pest17.3\plproc.exe" 'RmaSutraNodeTest.15B.script'
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" RmaSutraNodeTest.inp.tpl RmaSutraNodeTest.pval
"C:\SutraSuite\SUTRA_3_0\bin\sutra_3_0.exe"
"C:\ModelingTools\ModelMuse\Debug\Win32\SutraObsExtractor.exe" RmaSutraNodeTest.soe_ev
