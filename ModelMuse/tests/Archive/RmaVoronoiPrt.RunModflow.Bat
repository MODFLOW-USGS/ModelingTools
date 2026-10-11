Copy /y "RmaVoronoiPrt.mfsim.nam" "mfsim.nam"
call "C:\Program Files\USGS\ModelMuse5\bin\ModelMonitor.exe" -m C:\WRDAPP\mf6.8.1_win64\bin\mf6.exe -n mfsim.nam -mv 6
"Mf6ObsExtractor.exe" RmaVoronoiPrt.Mf6WriteIns
"plproc.exe" RmaVoronoiPrt.Kx.krig_factors_script
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe RmaVoronoiPrt.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe mfsim.lst
Start C:\ModelingTools\ModelMonitor\Release\Win64\ListingAnalyst.exe RmaVoronoiPrt.PRT1.lst
pause
