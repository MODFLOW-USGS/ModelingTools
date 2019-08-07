README.TXT

WellFootprint version 1.0.1
Software to compute the "footprint" of well withdrawals as
an aid to visualizing the magnitude of well withdrawals.

NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This software has been approved for release by the U.S. Geological Survey 
(USGS). Although the software has been subjected to rigorous review, the 
USGS reserves the right to update the software as needed pursuant to 
further analysis and review. No warranty, expressed or implied, is made by 
the USGS or the U.S. Government as to the functionality of the software 
and related material nor shall the fact of release constitute any such 
warranty. Furthermore, the software is released on condition that neither 
the USGS nor the U.S. Government shall be held liable for any damages 
resulting from its authorized or unauthorized use.

This version of WellFootprint is packaged for personal computers using
the Microsoft Windows 7, 8, 8.1, or 10 operating systems.  Executable 
files for personal computers are provided as well as the source code. 

Instructions for installation, execution, and testing of WellFootprint are
provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING
                         D. TESTING
                         E. COMPILING
                         F. HISTORY


A. DISTRIBUTION FILE
WellFootprint is distributed as an installer file or a zip file
with versions for both 32-bit and 64-bit operating systems.
Either an installer or zip file may be used for installing WellFootprint.
Both contain the same version of WellFootprint for use on personal computers:

For 32 or 64-bit operating systems:
         WellFootprintSetup32_1.exe
         WellFootprint32_1.zip
For 64-bit operating systems:
         WellFootprintSetup64_1.exe
         WellFootprint64_1.zip

The distribution files contain:

          A Compiled runfile for WellFootprint.
          WellFootprint documentation in a PDF file.
          An example model with input and output.
          A ModelMuse file used to create the example model.
          Source code

B. INSTALLING
If installed from the installer, WellFootprint will be installed
in a directory chosen by the user. By default, WellFootprint will be 
installed in C:\Program Files\USGS\WellFootprint or
C:\Program Files (x86)\USGS\WellFootprint. 
The following directory structure will be created in the installation 
directory:
   |--WellFootprint
   |  |--bin          ; WellFootprint.exe
   |  |--doc          ; Documentation file
   |  |--src          ; Source code for WellFootprint
   |  |--test-out     ; Output from example model
   |  |--test-run     ; Input for example model and corresponding ModelMuse file.

The installer will associate the extension ".fpi" with WellFootprint so that 
WellFootprint can be started by double-clicking on files with that extension.

Included in directory WellFootprint\doc is the WellFootprint report as a 
Portable Document Format (PDF) file. The PDF file is readable and 
printable on various computer platforms using Acrobat Reader from Adobe.
The Acrobat Reader is freely available from the following web site:
      http://www.adobe.com/

The zip file contains the same files in the same directory structure except 
that the root directory is named WellFootprint1_0_1. 
To install it, unzip the file retaining the directory structure of the zip
file. However, .fpi files will not automatically be associated with 
WellFootprint when installed manually.

The report can be cited as 
Winston, R.B., and Goode, D.J., 2017, Visualization of groundwater withdrawals: 
U.S. Geological Survey Open-File Report 2017–1137, 8 p., 
https://doi.org/10.3133/ofr20171137.


C. EXECUTING THE SOFTWARE

There are several ways to execute the software.
1. Double click on it in Windows Explorer.
2. Open a command-line window and type the file path followed by 
   "WellFootprint.exe" and optionally followed by the name of an input 
   file for WellFootprint. If no input file is specified, the user is 
   prompted to enter the name of the input file.  
3. Double click on a file with the extension associated with 
   WellFootprint (.fpi). 
4. Create a batch file that runs the program in a command-line window. An
   example of such a batch file is described in the "TESTING" section of 
   this file.

D. TESTING
A test data set is provided to verify that WellFootprint is correctly
installed and running on the system.  The tests also may be looked
at as examples of how to use the program.  The directory
WellFootprint\test-run contains the input data for running the test. 
Directory WellFootprint\test-out contains the output files from running
the test. A ModelMuse file corresponding to the example also is included 
in the WellFootprint\test-run directory.

The directory WellFootprint\test-run can be used to conveniently run the
tests without destroying the original results in the WellFootprint\test-out
directory.  The test-run directory contains WellFootprint input file, which ends
with ".fpi", for running the tests.  The test can be run by double-clicking 
the RunFootprint.bat file in the directory.  WellFootprint should
be run in a command-prompt window with the current directory being the
test-run directory.  The output files that are created in the test-run
directory can then be compared to those in WellFootprint\test-out.

E. COMPILING

The 32 and 64-bit version of WellFootprint are both compiled with Delphi 10.2 
from Embarcadero. http://www.embarcadero.com/. To compile, open 
WellFootprint.dproj with Delphi 10.2, ensure that the file QuadTreeClass.pas is 
on the search path and compile.

F. HISTORY
Version 1.0.1: April 6, 2018
  If WellFootprint failed to reach a solution within the user specified number of 
iterations, it would prompt the user to enter any number of additional iterations
to continue solving. If a value of zero was entered, the software quit immediately. 
However, instead of continuing with additional iterations WellFootprint would 
incorrectly restart at iteration 1 of the well footprint calculation. In version 1.0.1, 
WellFootprint performs additional iterations rather than restarting at iteration 1.

Version 1.0: Initial release, Dec. 19, 2017