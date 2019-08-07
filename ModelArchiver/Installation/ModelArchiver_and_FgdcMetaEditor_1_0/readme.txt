README.TXT

ModelArchiver version 1.0 
and FgdcMetaEditor version 1.0

Software to aid in the creation of model archives and FGDC metadata.

NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This software has been approved for release by the U.S. Geological Survey 
(USGS). Although the software has been subjected to rigorous review, the 
USGS reserves the right to update the software as needed pursuant to 
further analysis and review. No warranty, expressed or implied, is made 
by the USGS or the U.S. Government as to the functionality of the 
software and related material nor shall the fact of release constitute 
any such warranty. Furthermore, the software is released on condition that 
neither the USGS nor the U.S. Government shall be held liable for any 
damages resulting from its authorized or unauthorized use.

These versions of ModelArchiver and FgdcMetaEditor are packaged for personal 
computers using the Microsoft Windows 7, 8, 8.1, or 10 operating systems.  
Executable files for personal computers are provided as well as the source code. 

Instructions for installation, execution, and testing of ModelArchiver and 
FgdcMetaEditor are provided below.

                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING ModelArchiver and FgdcMetaEditor
                         D. TESTING
                         E. COMPILING
                         F. HISTORY

A. DISTRIBUTION FILE
ModelArchiver and FgdcMetaEditor are distributed as either an installer or a
zip file. The installer will install either 32-bit or 64-bit versions of the 
programs depending on the operating system. The zip file includes both the 
32-bit and 64-bit versions of the programs.
Either an installer or zip file may be used for installing ModelArchiver and 
FgdcMetaEditor. Both contain the same version of WellFootprint for use on 
personal computers:

Installer:
         ModelArchiver_and_FgdcMetaEditor_Setup_1_0.exe
Zip file:
         ModelArchiver_and_FgdcMetaEditor_1_0.zip

The distribution files contain:

          Compiled runfiles for ModelArchiver and FgdcMetaEditor.
          ModelArchiver and FgdcMetaEditor documentation in a PDF file.
          An example of a series of models to be placed in a model archive.
          including FGDC meta data along with the archive created by 
          ModelArchiver.
          Source code

B. INSTALLING
If installed from the installer, ModelArchiver and FgdcMetaEditor will be installed
in a directory chosen by the user. By default, the model programs and 
documentation will be installed in 
C:\Program Files\USGS\ModelArchiver_and_FgdcMetaEditor or
C:\Program Files (x86)\USGS\ModelArchiver_and_FgdcMetaEditor. 64-bit versions 
of the programs will be installed in the computers running a 64-bit operating 
system. 32-bit versions of the programs will be installed in the computers running 
a 32-bit operating system. The example input and output will be installed in the 
"Public Documents\Documents" folder of the user.

The following directory structure will be created in the installation 
directory:

Program Files
   |--ModelArchiver and FgdcMetaEditor
   |  |--bin          ; ModelArchiver.exe and FgdcMetaEditor.exe
   |  |--doc          ; Documentation file
   |  |--src          ; Source code for ModelArchiver and FgdcMetaEditor

In addition, the following will be created in the Public Documents directory.
Public Documents\Documents
   |--ModelArchiver and FgdcMetaEditor
   |  |--test-out     ; Example model archive created by ModelArchiver
   |  |--test-run     ; Model input and output files, browse graphic, readme file 
                        metadata file for the model archive, and a ModelArchiver 
                        project file.

The installer will associate the extension ".asxml" with ModelArchiver so that 
ModelArchiver can be started by double-clicking on files with that extension.

Included in directory ModelArchiver and FgdcMetaEditor\doc is the report on 
the programs as a Portable Document Format (PDF) file. The PDF file is readable 
and printable on various computer platforms using Acrobat Reader from Adobe.
The Acrobat Reader is freely available from the following World Wide Web
site:
      http://www.adobe.com/

The zip file contains the same files as the installer. 
To install it, unzip the file retaining the directory structure of the zip
file. However, .asxml files will not be automatically associated with 
ModelArchiver when installed manually. the zip file contains both 32 and 
64 bit versions of the programs.

   |--ModelArchiver and FgdcMetaEditor
   |  |--bin32        ; 32-bit versions of ModelArchiver.exe and FgdcMetaEditor.exe
   |  |--bin64        ; 32-bit versions of ModelArchiver.exe and FgdcMetaEditor.exe
   |  |--doc          ; Documentation file
   |  |--src          ; Source code for ModelArchiver and FgdcMetaEditor
   |  |--test-out     ; Example model archive created by ModelArchiver
   |  |--test-run     ; Model input and output files, browse graphic, readme file 
                        metadata file for the model archive, and a ModelArchiver 
                        project file.

The report can be cited as 
Winston, R.B., 2018, ModelArchiver—A program for facilitating the creation of 
groundwater model archives: U.S. Geological Survey Open-File Report 
2017–1149, 15 p., https://doi.org/10.3133/ofr20171149.

C. EXECUTING THE SOFTWARE

There are several ways to execute the ModelArchiver and FgdcMetaEditor software.
1. Click a shortcut on the start menu or double-click a shortcut on the desktop.
2. Double click on it in Windows Explorer.
3. Open a command-line window and type path for ModelArchiver.exe or 
   FgdcMetaEditor.exe optionally followed by the name of an ModelArchiver 
   project file or and FGDC XML file. If no file is specified, the user can select 
   the name of the project file or XML file using the menus within the programs. 
4. For ModelArchiver, double click on a file with the extension associated with 
   ModelArchiver (.asxml). (FgdcMetaEditor is not associated with any file
   extensions.) The installer will associate .asxml files with ModelArchiver. 

D. TESTING
A test data set is provided to verify that ModelArchiver and FgdcMetaEditor are 
correctly installed and running on the system.  The test may also be looked
at as an example of how to use the program.  The directory
ModelArchiver\test-run contains the model files for the archive. 
the directory ModelArchiver\test-out contains the completed model archive.

E. COMPILING

The 32 and 64-bit versions of ModelArchiver and FgdcMetaEditor are compiled 
with Delphi 10.2 from Embarcadero. http://www.embarcadero.com/ 

The Turbopower Abbrevia components must be installed in Delphi to compile 
the software. They are available through the Delphi "GetIt Package Manager" or 
from https://github.com/TurboPack-Tokyo/Abbrevia.
The Turbopower Abbrevia components are licensed under the Mozilla Public 
License Version 1.1.

F. HISTORY
Initial release March 1, 2018