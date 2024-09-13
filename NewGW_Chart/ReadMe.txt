GW_Chart Version 1.31.0.0

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

This version of GW_Chart is packaged for personal computers using
the Microsoft Windows 7, 8, 8.1, 10, or 11 operating systems.  Executable 
files for personal computers are provided as well as the source code.

Instructions for installation, execution, and compiling are provided below.

                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. COMPILING

A. DISTRIBUTION FILES

GW_Chart is distributed as either an installer or a zip file.
Either version may be used for installing GW_Chart.  Both contain
the same version of GW_Chart for use on personal computers:

For 32 or 64-bit operating systems:
         setup_GW_Chart_1.31.0.exe
         GW_Chart_1_31.zip

Both distribution files contain:

          Compiled runfiles for GW_Chart.
          Compiled HTML Help (.chm) files.

The file GW_ChartSource_1_31.zip contains the source code for GW_Chart. 
The Help folder in GW_ChartSource_1_31.zip contains the source code for 
the GW_Chart Help.
The help is compiled with HelpScribble (https://www.helpscribble.com/index.html).

B. INSTALLING
The recommended method of installing GW_Chart is to the 
installer. Execution of the installer will install GW_Chart in a 
directory chosen by the user. If no previous version of GW_Chart 
is installed, the default installation directory will be
C:\Program Files\USGS\GW_Chart or
C:\Program Files (x86)\USGS\GW_Chart. If a previous version of GW_Chart 
is installed, the default installation directory will be the directory for 
the previous version of GW_Chart.

The zip files contain the same files. 
To install it, unzip the file into an empty directory. You may also need 
to copy the .chm files to a flash drive and back to prevent Windows from 
blocking their content.

C. EXECUTING THE SOFTWARE

There are at least two ways to execute the software.
1. Double click on it in Windows Exporer.
2. Double-click on the desktop short cut for GW_Chart that is optionally
   created by the installer.

D. COMPILING
GW_Chart is compiled with Embarcadero Delphi version XE2.

Compiling GW_Chart requires making a change to the source code of the TChart (version 9.16) component from Steema Software (http://www.steema.com).  The required change is as follows.

In the implementation of TChartEditor.Execute; after the line containing "CheckHelpFile;" add the following line .
  HelpFile := Application.HelpFile;

