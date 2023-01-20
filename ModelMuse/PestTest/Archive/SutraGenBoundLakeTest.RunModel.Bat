@echo off
rem Get start time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "start=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)
@echo on
if exist "arrays\SutraGenBoundLakeTest.Lake_Bottom_1.arrays" del "arrays\SutraGenBoundLakeTest.Lake_Bottom_1.arrays"
if exist "arrays\SutraGenBoundLakeTest.TestDS_1.arrays" del "arrays\SutraGenBoundLakeTest.TestDS_1.arrays"
if exist "SutraGenBoundLakeTest.GfbBCS" del "SutraGenBoundLakeTest.GfbBCS"
if exist "SutraGenBoundLakeTest.GtbBCS" del "SutraGenBoundLakeTest.GtbBCS"
if exist "SutraGenBoundLakeTest.inp" del "SutraGenBoundLakeTest.inp"
if exist "SutraGenBoundLakeTest.lkar" del "SutraGenBoundLakeTest.lkar"
if exist "SutraGenBoundLakeTest.lkin" del "SutraGenBoundLakeTest.lkin"
if exist "SutraGenBoundLakeTest.bcopg" del "SutraGenBoundLakeTest.bcopg"
if exist "SutraGenBoundLakeTest.bcoug" del "SutraGenBoundLakeTest.bcoug"
if exist "SutraGenBoundLakeTest.ele" del "SutraGenBoundLakeTest.ele"
if exist "SutraGenBoundLakeTest.lkbu" del "SutraGenBoundLakeTest.lkbu"
if exist "SutraGenBoundLakeTest.lkh" del "SutraGenBoundLakeTest.lkh"
if exist "SutraGenBoundLakeTest.lkn" del "SutraGenBoundLakeTest.lkn"
if exist "SutraGenBoundLakeTest.lkrs" del "SutraGenBoundLakeTest.lkrs"
if exist "SutraGenBoundLakeTest.lkst" del "SutraGenBoundLakeTest.lkst"
if exist "SutraGenBoundLakeTest.lst" del "SutraGenBoundLakeTest.lst"
if exist "SutraGenBoundLakeTest.nod" del "SutraGenBoundLakeTest.nod"
if exist "SutraGenBoundLakeTest.rst" del "SutraGenBoundLakeTest.rst"
if exist "SutraGenBoundLakeTest.smy" del "SutraGenBoundLakeTest.smy"
if exist "SutraGenBoundLakeTest_Object7.OBC" del "SutraGenBoundLakeTest_Object7.OBC"
if exist "SutraGenBoundLakeTest_Object7.OBS" del "SutraGenBoundLakeTest_Object7.OBS"
"plproc.exe" SutraGenBoundLakeTest.TestDS.script
"plproc.exe" SutraGenBoundLakeTest.Lake_Bottom.script
"EnhancedTemplateProcessor.exe" SutraGenBoundLakeTest.GfbBCS.tpl SutraGenBoundLakeTest.pval
"EnhancedTemplateProcessor.exe" SutraGenBoundLakeTest.GtbBCS.tpl SutraGenBoundLakeTest.pval
"EnhancedTemplateProcessor.exe" SutraGenBoundLakeTest.lkin.tpl SutraGenBoundLakeTest.pval
"EnhancedTemplateProcessor.exe" SutraGenBoundLakeTest.lkar.tpl SutraGenBoundLakeTest.pval
"EnhancedTemplateProcessor.exe" SutraGenBoundLakeTest.inp.tpl SutraGenBoundLakeTest.pval
"sutra_3_0.exe"
"SutraObsExtractor.exe" SutraGenBoundLakeTest.soe_ev
@echo off
rem Get end time:
for /F "tokens=1-4 delims=:.," %%a in ("%time%") do (
   set /A "end=(((%%a*60)+1%%b %% 100)*60+1%%c %% 100)*100+1%%d %% 100"
)

rem Get elapsed time:
set /A elapsed=end-start

rem Show elapsed time:
set /A hh=elapsed/(60*60*100), rest=elapsed%%(60*60*100), mm=rest/(60*100), rest%%=60*100, ss=rest/100, cc=rest%%100
if %mm% lss 10 set mm=0%mm%
if %ss% lss 10 set ss=0%ss%
if %cc% lss 10 set cc=0%cc%
echo elapsed time: %hh% hours, %mm% minutes %ss%.%cc% seconds

