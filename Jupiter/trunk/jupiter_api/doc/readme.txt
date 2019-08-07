readme.txt


                   JUPITER API - Version: 1.7.3 8/6/2013
    Joint Universal Parameter IdenTification and Estimation of Reliability
                      Application Programming Interface


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

The files in this distribution contain the JUPITER API, as documented in:

Banta, E.R., Poeter, E.P., Doherty, J.E., and Hill, M.C., 2006, JUPITER:
     Joint Universal Parameter IdenTification and Evaluation of Reliability--
     An application programming interface (API) for model analysis:  
     U.S. Geological Survey Techniques and Methods Book 6, Section E, 
     Chapter 1, 268 p.  
     <http://water.usgs.gov/nrp/gwsoftware/jupiter/jupiter_api.html>

The software and related documentation referenced herein were developed by the 
U.S. Geological Survey (USGS) for use by the USGS in fulfilling its mission.  
The software can be used, copied, modified, and distributed without any fee or 
cost. Use of appropriate credit is requested.  The software is provided as a 
minimum in source code form as used on USGS computers.  In many cases, the 
executable runfiles also are provided for these computers.

The USGS provides no warranty, expressed or implied, as to the correctness of 
the furnished software or the suitability for any purpose.  The software has 
been tested, but as with any complex software, there could be undetected errors.  
Users who find errors are requested to report them to the USGS.  The USGS has 
limited resources to assist non-USGS users; however, we make an attempt to fix 
reported problems and help whenever possible.

This version of the JUPITER API is packaged for personal computers using
Microsoft Windows 95, 98, ME, NT, 2000, XP, or Windows 7.

See the file jupiter_api\doc\release.txt for a description of the latest version 
of the JUPITER API.  Instructions for installation and testing are provided 
below.


                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. EXTRACTING FILES
                         C. COMPILING
                         D. RUNNING THE EXAMPLE APPLICATIONS


A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal 
computers:

         jupiter_api_1_7_3.exe

The distribution file contains:

         Source code for the JUPITER API modules.
         
         Source code and compiled executable files for example applications 
         GROUP_EXAMPLE, JRUNNER, and SENSITIVITY_EXAMPLE.
         
         Supplementary documentation in text and/or PDF files.
         
         Test data sets for the example applications.


B. EXTRACTING FILES

The distribution file is a self-extracting program.  Execution of the 
distribution file creates numerous individual files.  The extraction program 
allows you to specify the directory in which the files should be restored.  
By default, the files will be extracted into directory C:\WRDAPP.  The following 
directory structure will be created in C:\WRDAPP or in a directory specified at 
the time of extraction:

DIRECTORY                    FILES
---------------------------  ---------------------------------------------------
jupiter_api
|-- bin                      executables for example applications
|-- doc                      documentation files
|-- mod                      empty; provided for module files
|-- obj                      empty; provided for object files
|-- src
|   |-- api_modules          source code for JUPITER API modules
|   |-- group_example        source code for GROUP_EXAMPLE
|   |-- jrunner              source code for JRUNNER
|   |-- sensitivity_example  source code for SENSITIVITY_EXAMPLE
|
|-- test             
    |-- group_example        test input and output files for GROUP_EXAMPLE
    |-- sensitivity_example  test input and output files for SENSITIVITY_EXAMPLE
        |-- runner1          files for running SENSITIVITY_EXAMPLE in parallel
        |-- runner2          files for running SENSITIVITY_EXAMPLE in parallel
    

Included in directory jupiter_api\doc are various documentation files.  Some of 
them may be Portable Document Format (PDF) files. The PDF files are readable
and printable on various computer platforms using Acrobat Reader from Adobe.
The Acrobat Reader is freely available from the following World Wide Web sites:
      http://www.adobe.com/
      http://www.shareware.com/

Updates to the JUPITER API, which will be made available at the URL listed 
above, will be described in release notes in jupiter_api\doc\release.txt.


C. COMPILING

In general, the requirements for compiling the example applications are a 
Fortran compiler and the knowledge of using the compiler.  The Fortran source-
code files (files ending in .f90) must be compiled with a Fortran (90 or 95) 
compiler.  

The modules of the JUPITER API have been written to conform as closely as 
possible to the ANSI Fortran-90 standard.  As a result, few modifications are 
expected to be needed to allow the modules to be compiled by any Fortran-90 
compliant compiler on any operating system supported by the compiler.  Please 
see Chapter 18 of the JUPITER API documentation (USGS Techniques and Methods, 
Book 6, Chapter E1) for a discussion of non-standard code.  In addition, 
version 1.6.0 introduces code that uses the DELFILESQQ function.  For a
compiler that does not support DELFILESQQ or similar function, see comment
at lines in pll.f90 where DELFILESQQ is invoked for instructions to enable
compilation.


D. RUNNING THE EXAMPLE APPLICATIONS

Test data sets are provided to verify that the example applications are 
correctly installed and running on the system.  The test directory contains 
subdirectories that contain test-case input files for the GROUP_EXAMPLE and 
SENSITIVITY_EXAMPLE applications.  

To run the GROUP_EXAMPLE test case, open a command-prompt window and change to
directory jupiter_api\test\group_example, then type "runtest".  The batch file 
runtest.bat invokes group_example.exe, using as input the file gp.in, and it 
writes output to file gp.out.

To run the SENSITIVITY_EXAMPLE test case, open a command-prompt window and 
change to directory jupiter_api\test\sensitivity_example, then type "runtest".
the batch file runtest.bat invokes sensitivity_example.exe, using as input the 
file sensitivity_example_input.txt.  It generates output files 
SENSITIVITY_EXAMPLE.out and tc1_sen_example_out._su.

To run the SENSITIVITY_EXAMPLE test case in parallel-processing mode: (1) edit 
the PARALLEL_CONTROL input block of file sensitivity_example_input.txt to change 
PARALLEL=no to PARALLEL=yes; (2) open two additional command-prompt windows and 
change to runner directory runner1 in one window and to runner2 in the other; 
(3) start jrunner.exe in each of the runner directories by typing 
"..\..\..\bin\jrunner"; (4) start sensitivity_example.exe, as above.  On a 
multiple-processor computer, running JRUNNER and SENSITIVITY_EXAMPLE in this way 
will use the available processors simultaneously.  The same steps can be 
followed on a single-processor computer; however, the available CPU time will be 
shared between the processes running JRUNNER, and the process-model runs will 
not actually be executed simultaneously.
               