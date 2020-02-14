E. COMPILING

The GwMounding is compiled with Delphi 10.3.3 from Embarcadero.
http://www.embarcadero.com/ 

The help system for GwMounding is compiled with Help and Manual version 5
from EC Software. http://www.helpandmanual.com/

ModelMuse uses a number of custom components that must be installed 
in Delphi 10.3.3 before compiling ModelMuse.  Some are included  
with the ModelMuse source code.  Additional required files or components are 
listed below.  In some cases, the files must be altered before they 
can be used with ModelMuse.  The required changes are listed below.

General instructions for installing packages in Delphi 10.3.3 and the
Delphi Community Edition.
1. If the component can be installed with the "Tools|GetIt Package Manager", 
do so.
2. If the component comes with an installer, run the installer.
3. If you are compiling the components from source code, you need to add the 
directories containing the source code to the Library path for both the 
Windows 32 and Windows 64 bit platforms. Select "Tools|Options"
then look in "Environment Options|Delphi Options|Library".
4. If you are compiling the components from source code and the components 
are separated into run-time and design-time packages, you build the runtime 
package and then build and install the design-time package.

Install JCL and JVCL. They can be obtained from http://www.delphi-jedi.org/
or from the "Tools|GetIt Package Manager".
If installing with the GetIt Package Manager, the installer source code will 
be downloaded and built and started. You must close Delphi before starting 
the installation process with the installer. With JCL, you must accept the 
MPL 1.1 Licence on the MPL 1.1 License tab before the installation starts.
Add the following JCL directories to the Library path if they are not added
automatically when installing the JCL.
source\common
source\windows
JVCL Version 3.50 was used in compiling ModelMuse. 

Installing Graphics32
https://sourceforge.net/projects/graphics32/
Download version graphics32-1-9-1 of Graphics32. 
See also http://graphics32.org/wiki/
A more recent version of Graphics32 is available but was not used in 
ModelMuse.

Make the following changes in Graphics32.
Comment out MouseUp(mbLeft, [], 0, 0); in TCustomImage32.DblClick in 
GR32_Image.pas.

Add the following lines to GR32_Compiler.inc near the beginning.
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 23}
    {$DEFINE COMPILERXE2}
    {$IFNDEF BCB}
      {$DEFINE DELPHIXE1}
      {$DEFINE DELPHIXE2}
    {$ELSE}
      {$DEFINE BCB7}
    {$ENDIF}
  {$IFEND}
{$ENDIF}

{$IFDEF VER320}
  {$DEFINE COMPILERXE5}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE5}
  {$ELSE}
    {$DEFINE BCBXE5}
  {$ENDIF}
{$ENDIF}

in GR32.pas locate TCustomBitmap32.LineXS. In it change
if n > 0 then
to
if (n > 0) and (hyp <> 0) then

Installing GLScene
http://glscene.sourceforge.net
For compiling with Delphi 10.3, use GLScene_v1.8.
This version of GLScene comes as an installer. It copies files to the 
hard drive but do not neccesarily install the components in the the DELPHI IDE.

Graphics 32 support must be added by modifying GLScene.inc so make the 
required change. This is done by changing 
{.$DEFINE USE_GRAPHICS32}
to
{$DEFINE USE_GRAPHICS32}

In the file GLScene.Pas, locate TGLLightSource.Create.
In it, replace 
  FListHandle := nil;
with
  FreeAndNil(FListHandle);

This change fixes a memory leak.

Open and read the readme.txt file in the top level directory of GLScene. 
It directs you to a pdf on installing GLScene. Follow those instructions. 
The portions of the instructions related to C++ do not apply if GLScene 
is only being installed in Delphi. If asked whether to add the 
Graphics 32 to the project, select yes.

To compile the design-time package, you will need to edit the search path for 
the design time Project (Project|Options|Delphi compiler|Search path) so that 
it includes the dcp and/or bpl
output directories. For example:
C:\Users\Public\Documents\Embarcadero\Studio\20.0\DCP
and 
C:\Users\Public\Documents\Embarcadero\Studio\20.0\Bpl
Then edit the "requires" section of the GLScene Design-time package to 
add a requirement for the Graphics32 design-time package. To add a 
requirement, right-click on the "requires" section of the package in the 
Projects window and select reference. Then navigate to the DCP directory
and a the .dcp file for the Graphics32 design-time package.

Build the runtime package first. Then install the design time package.  

TurboPower Abbrevia
http://sourceforge.net/projects/tpabbrevia/
Deactivate UnzipZipxSupport in AbDefine.inc
TurboPower Abbervia is also available through the GetIt Package Manager in Delphi 10.3.3 and later.

The Components directory has additional components that need to be installed. 
They are in the following subdirectories of the Components directory.

Note that the Design-time package for Graphics32 is a requirement for the ZoomBox 
component in QZoomBox2

addbtn95
ade
ButtonEdit
datagrid
GLWidget
MMJLabel
ModelCube
QMostRecentlyUsedFiles
Quadtree
QZoomBox2
RbwController
RbwDataGrid
RbwDynamicCursor
RbwEdit
RbwParser
RbwRollupPanel
RbwRuler
xbase

The GraphicEX and xygraph directories do not have packages but they need to be
added to the search path.





