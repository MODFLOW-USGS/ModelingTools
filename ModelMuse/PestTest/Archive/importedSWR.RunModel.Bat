if exist "arrays\importedSWR.MyVertOffset_1.arrays" del "arrays\importedSWR.MyVertOffset_1.arrays"
if exist "importedSWR.Mf2005Values" del "importedSWR.Mf2005Values"
if exist "importedSWR.swr" del "importedSWR.swr"
if exist "importedSWR.cbc" del "importedSWR.cbc"
if exist "importedSWR.fdn" del "importedSWR.fdn"
if exist "importedSWR.fhd" del "importedSWR.fhd"
if exist "importedSWR.hob_out" del "importedSWR.hob_out"
if exist "importedSWR.lst" del "importedSWR.lst"
if exist "importedSWR.Swr_Convergence" del "importedSWR.Swr_Convergence"
if exist "importedSWR.Swr_Obs_A" del "importedSWR.Swr_Obs_A"
if exist "importedSWR.Swr_ReachExchange_B" del "importedSWR.Swr_ReachExchange_B"
if exist "importedSWR.Swr_ReachGroupFlows_B" del "importedSWR.Swr_ReachGroupFlows_B"
if exist "importedSWR.Swr_ReachStage_A" del "importedSWR.Swr_ReachStage_A"
if exist "importedSWR.Swr_RIV" del "importedSWR.Swr_RIV"
"C:\Pest17.3\plproc.exe" importedSWR.MyVertOffset.script
"C:\ModelingTools\ModelMuse\Debug\Win32\EnhancedTemplateProcessor.exe" importedSWR.swr.tpl importedSWR.pval
C:\WRDAPP\MODFLOW-NWT_1.2.0\bin\MODFLOW-NWT.exe importedSWR.nam /wait
"C:\ModelingTools\ModelMuse\Debug\Win32\Mf2005ObsExtractor.exe" importedSWR.Mf2005ExtractValues
