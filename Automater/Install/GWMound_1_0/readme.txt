GWMound version 1.0

GWMound: Software for the simulation  of groundwater mounding beneath stormwater infiltration and artificial recharge  basins

NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.
	  
	  This software has been approved for release by the U.S. Geological Survey (USGS). Although the software has been subjected to rigorous review, the USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use. Also refer to the USGS Water Resources Software User Rights Notice for complete use, copyright, and distribution information. 

                           TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING GWMound
                         D. COMPILING


A. DISTRIBUTION FILE
The following distribution file is for use on personal computers:

         GWMound_1_0.zip

The distribution file contains:

          Compiled runfiles and source code for GWMound.
          GWMound documentation in a PDF file.
          An example data set.
 |
 |--GWMound_1_0
    |--bin32            ; GWMound executables for Windows computers using 32 or 64-bit operating systems
    |--bin64            ; GWMound executables for Windows computers using 64-bit operating systems
    |--doc            ; Documentation file
    |--example        ; An example GWMound file.
    |--Source         ; GWMound source code

B. INSTALLING

To install GWMound unzip the distribution file into an empty directory.

C. EXECUTING GWMound
To run GWMound, double-click on GwMound.exe in either the bin32 or bin64 directories. The version in the bin64 directory will only work on 64-bit versions of Windows. Addition instructions can be found in the doc directory of the distribution file.

D. COMPILING
GWMound can be compiled with Delphi version 11.3 or later. A license for Delphi can be obtained from Embarcadero (https://www.embarcadero.com/).

GWMound uses several components that are not included with Delphi. These include the JEDI Code Library and JEDI Visual Component Library. Both of these may be obtained from the Project JEDI (https://wiki.delphi-jedi.org/wiki/Main_Page). Additional components are included in the Components subdirectory of the Source directory of the distribution file. 
After installing the components, the two projects in the Dmath and GwMound subdirectories of the Source directory can be compiled with Delphi. 
The help for GWMound is in the HelpSource directory. It can be compiled with Help and Manual version 8 from https://www.helpandmanual.com/.
