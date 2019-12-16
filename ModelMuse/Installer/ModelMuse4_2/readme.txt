ModelMuse – Version 4.2.0.0

NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This software has been approved for release by the U.S. Geological Survey (USGS). 
Although the software has been subjected to rigorous review, the USGS reserves 
the right to update the software as needed pursuant to further analysis and 
review. No warranty, expressed or implied, is made by the USGS or the U.S. 
Government as to the functionality of the software and related material nor shall 
the fact of release constitute any such warranty. Furthermore, the software is 
released on condition that neither the USGS nor the U.S. Government shall be held 
liable for any damages resulting from its authorized or unauthorized use.

This version of ModelMuse is packaged for personal computers using
the Microsoft Windows 7, 8, 8.1, or 10 operating systems.  Executable 
files for personal computers are provided as well as the source code. Although 
not a supported operating system, some users have reported success on using 
ModelMuse on MacIntosh computers under WINE.

Instructions for installation, execution, and compiling are provided below.

                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. EXAMPLES
                         E. COMPILING


A. DISTRIBUTION FILES

ModelMuse is distributed as either an installer or a zip file.
Either version may be used for installing ModelMuse.  Both contain
the same version of ModelMuse for use on personal computers:

For 32 or 64-bit operating systems:
         ModelMuseSetup32_4_2.exe
         ModelMuse32_4_2.zip
For 64-bit operating systems:
         ModelMuseSetup64_4_2.exe
         ModelMuse64_4_2.zip

Both distribution files contain:

          Compiled runfiles for ModelMuse, ModelMonitor,
            and MF2005_Importer.exe.
          ModelMuse documentation in PDF files.
          Example models
          Supplementary materials

The file ModelMuseSource4_2.zip contains the source code for ModelMuse, 
         ModelMonitor, and MF2005_Importer.exe.

B. INSTALLING
Installing ModelMuse does not also install the modeling programs, 
such as MODFLOW, with which ModelMuse works. They must be installed 
separately.

The recommended method of installing ModelMuse is to run one of the 
installers. Execution of the installer will install ModelMuse in a 
directory chosen by the user. If no previous version of ModelMuse 
is installed, the default installation directory will be
C:\Program Files\USGS\ModelMuse4 or
C:\Program Files (x86)\USGS\ModelMuse4. If a previous version of ModelMuse 
is installed, the default installation directory will be the directory for 
the previous version of ModelMuse.
The installer will associate files with the extensions .gpt, 
.gpb, and .mmZlib with ModelMuse.  The following directory structure will 
be created in the installation directory:

   |--ModelMuse4
   |  |--bin          ; ModelMuse, ModelMonitor, and MF2005_Importer 
   |  |                   executables.
   |  |--doc          ; Documentation files

ModelMuse will also create a subdirectory of the "Public Documents" directory named 
"ModelMuse Examples" containing sample models and data.

   |  |--data         ; Data files and example models described in the 
   |  |                   documentation or the ModelMuse help.
   |  |--examples     ; Sample models.

Included in the ModelMuse4\doc directory are the reports on ModelMuse as 
Portable Document Format (PDF) files. The PDF files are readable and 
printable on various computer platforms using Acrobat Reader from Adobe.
The Acrobat Reader is freely available from the following World Wide Web
site:
      http://www.adobe.com/

The zip files contain the same files in the same directory structure. 
To install it, unzip the file retaining the directory structure of the zip
file. Unlike the installer, unzipping the files from a zip file will not 
associate ModelMuse project files with extensions .gpt, .gpb, or .mmZLib 
with ModelMuse.  This can be done manually.  The manual method used to 
associate ModelMuse project files with ModelMuse varies among the various 
Windows operating systems.  Consult your operating system help for more 
details.  Searching the operating system help for "To associate a file 
name extension with a file type" may give the required information.

C. EXECUTING THE SOFTWARE

There are several ways to execute the software.
1. Double click on it in Windows Exporer.
2. Double-click on the desktop short cut for ModelMuse that is optionally
   created by the installer.
3. Double click on a file with one of the extensions associated with 
   ModelMuse (.gpt, gpb, mmZlib).

D. EXAMPLES

Several example models are included in the "ModelMuse Examples\examples" 
folder under Public Documents. Many of the ones for PHAST reproduce sample 
models distributed with PHAST or described in the ModelMuse help. Most of 
the ones for MODFLOW are described in the ModelMuse help.  Data files used 
in the examples described in the ModelMuse documentation or help are in the 
"ModelMuse Examples\data" folder.

E. COMPILING

The 32 and 64-bit version of ModelMuse and ModelMonitor are compiled with 
Delphi 10.2 from Embarcadero.
http://www.embarcadero.com/ 
The 32-bit version can also be compiled using the free community edition
of Delphi 10.3.1 from Embarcadero.
https://www.embarcadero.com/products/delphi/starter

The help system for ModelMuse is compiled with Help and Manual version 5
from EC Software. http://www.helpandmanual.com/

MF2005_Importer is compiled with the Intel Fortran compiler.

ModelMuse uses a number of custom components that must be installed 
in Delphi 10.2 before compiling ModelMuse.  Some are included  
with the ModelMuse source code.  Additional required files or components are 
listed below.  In some cases, the files must be altered before they 
can be used with ModelMuse.  The required changes are listed below.

General instructions for installing packages in Delphi 10.2 and the
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


Get and install VirtualTreeView version 6.6 or later.
http://www.jam-software.com/virtual-treeview/
https://github.com/Virtual-TreeView/Virtual-TreeView-XE2
It can also be installed via the Delphi Get It package manager.

MadExcept version 4 or later must be installed.  It can be obtained from 
http://www.madshi.net/

TurboPower Abbrevia
http://sourceforge.net/projects/tpabbrevia/
Deactivate UnzipZipxSupport in AbDefine.inc
TurboPower Abbervia is also available through the GetIt Package Manager in Delphi 10.2

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





