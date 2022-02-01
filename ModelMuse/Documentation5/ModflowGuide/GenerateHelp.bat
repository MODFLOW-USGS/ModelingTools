rem delete and recreate directories for CHM output
if exist Modflow2000Guide rmdir /s /q  Modflow2000Guide
mkdir Modflow2000Guide
if exist Modflow2005Guide rmdir /s /q  Modflow2005Guide
mkdir Modflow2005Guide
if exist ModflowLgrGuide rmdir /s /q  ModflowLgrGuide
mkdir ModflowLgrGuide
if exist ModflowNwtGuide rmdir /s /q  ModflowNwtGuide
mkdir ModflowNwtGuide
if exist ModflowOwhmGuide rmdir /s /q  ModflowOwhmGuide
mkdir ModflowOwhmGuide
if exist ModflowGuide rmdir /s /q  ModflowGuide
mkdir ModflowGuide
rem delete and recreate directories for Webhelp output
if exist WebModflow2000Guide rmdir /s /q  WebModflow2000Guide
mkdir WebModflow2000Guide
if exist WebModflow2005Guide rmdir /s /q  WebModflow2005Guide
mkdir WebModflow2005Guide
if exist WebModflowLgrGuide rmdir /s /q  WebModflowLgrGuide
mkdir WebModflowLgrGuide
if exist WebModflowNwtGuide rmdir /s /q  WebModflowNwtGuide
mkdir WebModflowNwtGuide
if exist WebModflowOwhmGuide rmdir /s /q  WebModflowOwhmGuide
mkdir WebModflowOwhmGuide
if exist WebModflowGuide rmdir /s /q  WebModflowGuide
mkdir WebModflowGuide
rem Generate help output.
"C:\Program Files (x86)\EC Software\HelpAndManual8\HELPMAN.EXE" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\ModflowGuide.hmxp" GenerateHelpIni.ini
rem "C:\Program Files (x86)\EC Software\HelpAndManual5\HELPMAN.EXE" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\ModflowGuide.hmxp" GenerateHelpIni.ini
rem "C:\Program Files\EC Software\HelpAndManual5\HELPMAN.EXE" "C:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\ModflowGuide.hmxp" GenerateHelpIni.ini
rem
rem modify gnerated webhelp output
"C:\Projects\GoogleAnalytics\Win32\Debug\GoogleAnalytics.exe" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\Modflow2000Guide" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\ModflowGuide"
"C:\Projects\OgwAnalytics\Win32\Debug\OgwAnalytics.exe" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\Modflow2005Guide" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\ModflowNwtGuide" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\ModflowLgrGuide"
"C:\Projects\HttpReplace\Win32\Debug\HttpReplace.exe" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\Modflow2000Guide" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\ModflowGuide" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\ModflowNwtGuide" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\ModflowOwhmGuide" "C:\ModelingTools\ModelMuse\Documentation5\ModflowGuide\ModflowLgrGuide"
rem "C:\Users\rbwinst-pr\Documents\RAD Studio\Projects\GoogleAnalytics\Win32\Release\GoogleAnalytics.exe" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\Modflow2000Guide" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\ModflowAllGuide"
rem "C:\Users\rbwinst-pr\Documents\RAD Studio\Projects\OgwAnalytics\Win32\Release\OgwAnalytics.exe" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\Modflow2005Guide" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\ModflowNwtGuide" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\ModflowLgrGuide"
rem "C:\Users\rbwinst\Documents\Embarcadero\Studio\Projects\ReplaceHttp\Win32\Debug\ReplaceHttp.exe" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\Modflow2000Guide" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\ModflowAllGuide" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\ModflowNwtGuide" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\ModflowOwhmGuide" "D:\Colab\GWModelTools\ModelMuse\Documentation5\ModflowGuide\ModflowLgrGuide"
rem
rem copy baggage files
copy Baggage\*.* Modflow2000Guide\
copy Baggage\*.* Modflow2005Guide\
copy Baggage\*.* ModflowLgrGuide\
copy Baggage\*.* ModflowNwtGuide\
copy Baggage\*.* ModflowOwhmGuide\
copy Baggage\*.* ModflowGuide\
rem make zip files of chm help in webhelp directories
"C:\Program Files\7-Zip\7z.exe" a WebModflow2000Guide\guide2000.zip Modflow2000Guide\
"C:\Program Files\7-Zip\7z.exe" a WebModflow2005Guide\guide2005.zip Modflow2005Guide\
"C:\Program Files\7-Zip\7z.exe" a WebModflowLgrGuide\GuideLGR.zip ModflowLgrGuide\
"C:\Program Files\7-Zip\7z.exe" a WebModflowNwtGuide\guideNWT.zip ModflowNwtGuide\
"C:\Program Files\7-Zip\7z.exe" a WebModflowOwhmGuide\guideOWHM.zip ModflowOwhmGuide\
"C:\Program Files\7-Zip\7z.exe" a WebModflowGuide\guide.zip ModflowGuide\
pause
