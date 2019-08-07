# Microsoft Developer Studio Project File - Name="MF2005_Importer" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=MF2005_Importer - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "MF2005_Importer.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MF2005_Importer.mak" CFG="MF2005_Importer - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MF2005_Importer - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "MF2005_Importer - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "MF2005_Importer - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /browser /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /stack:0x4000000 /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "MF2005_Importer - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib DFOR.LIB /nologo /stack:0x4000000 /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "MF2005_Importer - Win32 Release"
# Name "MF2005_Importer - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\de47.f
DEP_F90_DE47_=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gmg7.f
DEP_F90_GMG7_=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2bas7.f
DEP_F90_GWF2B=\
	".\openspec.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2bcf7.f
DEP_F90_GWF2BC=\
	".\Release\GLOBAL.mod"\
	".\Release\GWFBASMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2chd7.f
DEP_F90_GWF2C=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2drn7.f
DEP_F90_GWF2D=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2drt7.f
DEP_F90_GWF2DR=\
	".\Release\GLOBAL.mod"\
	".\Release\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2ets7.f
DEP_F90_GWF2E=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2evt7.f
DEP_F90_GWF2EV=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2fhb7.f
DEP_F90_GWF2F=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2gag7.f
DEP_F90_GWF2G=\
	".\Release\GLOBAL.mod"\
	".\Release\GWFLAKMODULE.mod"\
	".\Release\GWFSFRMODULE.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2ghb7.f
DEP_F90_GWF2GH=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2hfb7.f
DEP_F90_GWF2H=\
	".\openspec.inc"\
	".\Release\GLOBAL.mod"\
	".\Release\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2huf7.f
DEP_F90_GWF2HU=\
	".\Release\GLOBAL.mod"\
	".\Release\GWFBASMODULE.mod"\
	".\Release\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2hydmod7.f
DEP_F90_GWF2HY=\
	".\Release\GLOBAL.mod"\
	".\Release\GWFIBSMODULE.MOD"\
	".\Release\GWFSFRMODULE.MOD"\
	".\Release\GWFSTRMODULE.MOD"\
	".\Release\GWFSUBMODULE.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2ibs7.f
DEP_F90_GWF2I=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2lak7.f
DEP_F90_GWF2L=\
	".\Release\GLOBAL.mod"\
	".\Release\GWFSFRMODULE.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2lpf7.f
DEP_F90_GWF2LP=\
	".\Release\GLOBAL.mod"\
	".\Release\GWFBASMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2mnw27.f
DEP_F90_GWF2M=\
	".\Release\DE4MODULE.mod"\
	".\Release\GLOBAL.mod"\
	".\Release\GWFBASMODULE.mod"\
	".\Release\PCGMODULE.MOD"\
	".\Release\SIPMODULE.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2mnw2i7.f
DEP_F90_GWF2MN=\
	".\Release\GLOBAL.mod"\
	".\Release\GWFMNW2MODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2mnw7.f
DEP_F90_GWF2MNW=\
	".\Release\DE4MODULE.mod"\
	".\Release\GLOBAL.mod"\
	".\Release\GMGMODULE.mod"\
	".\Release\GWFBASMODULE.mod"\
	".\Release\PCGMODULE.MOD"\
	".\Release\SIPMODULE.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2rch7.f
DEP_F90_GWF2R=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2res7.f
DEP_F90_GWF2RE=\
	".\Release\GLOBAL.mod"\
	".\Release\GWFBASMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2riv7.f
DEP_F90_GWF2RI=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2sfr7.f
DEP_F90_GWF2S=\
	".\Release\GLOBAL.mod"\
	".\Release\GWFBCFMODULE.mod"\
	".\Release\GWFHUFMODULE.mod"\
	".\Release\GWFLPFMODULE.mod"\
	".\Release\GWFRCHMODULE.mod"\
	".\Release\GWFSFRMODULE.MOD"\
	".\Release\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2str7.f
DEP_F90_GWF2ST=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2sub7.f
DEP_F90_GWF2SU=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2swt7.f
DEP_F90_GWF2SW=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2uzf1.f
DEP_F90_GWF2U=\
	".\Release\GLOBAL.mod"\
	".\Release\GWFBCFMODULE.mod"\
	".\Release\GWFHUFMODULE.mod"\
	".\Release\GWFLPFMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2wel7.f
DEP_F90_GWF2W=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwfsfrmodule.f
# End Source File
# Begin Source File

SOURCE=.\hufutl7.f
DEP_F90_HUFUT=\
	".\Release\GWFHUFMODULE.mod"\
	".\Release\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\lmt7.f
DEP_F90_LMT7_=\
	".\openspec.inc"\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\mf2005.f
DEP_F90_MF200=\
	".\openspec.inc"\
	".\Release\DE4MODULE.mod"\
	".\Release\GLOBAL.mod"\
	".\Release\GMGMODULE.mod"\
	".\Release\GWFBASMODULE.mod"\
	".\Release\GWFEVTMODULE.mod"\
	".\Release\GWFHUFMODULE.mod"\
	".\Release\GWFLAKMODULE.mod"\
	".\Release\GWFRCHMODULE.mod"\
	".\Release\GWFUZFMODULE.mod"\
	".\Release\PCGMODULE.MOD"\
	".\Release\PCGN.MOD"\
	".\Release\SIPMODULE.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2bas7.f
DEP_F90_OBS2B=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2chd7.f
DEP_F90_OBS2C=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2drn7.f
DEP_F90_OBS2D=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2ghb7.f
DEP_F90_OBS2G=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2riv7.f
DEP_F90_OBS2R=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2str7.f
DEP_F90_OBS2S=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\parutl7.f
DEP_F90_PARUT=\
	".\Release\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\pcg7.f
DEP_F90_PCG7_=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\pcgn2.f90
DEP_F90_PCGN2=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sip7.f
DEP_F90_SIP7_=\
	".\Release\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utl7.f
DEP_F90_UTL7_=\
	".\openspec.inc"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
