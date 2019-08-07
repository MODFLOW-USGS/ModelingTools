README_UCODE_2005.TXT


            UCODE_2005 - Version: Version 1.022 01/31/2011
                    Universal inverse modeling code


NOTE: Any use of trade, product or firm names is for descriptive purposes
      only and does not imply endorsement by the U.S. Government.

This version of UCODE_2005 is packaged for personal computers using
one of the Microsoft Windows operating systems.  An executable file for
personal computers is provided as well as the source code.  The executable
file was created using the MicroSoft Visual Studio 2005, Intel Fortran
Compiler 11.0. The source code can be compiled to run on other computers.

This version of UCODE is referred to as UCODE_2005 to distinguish it from
older versions.  See the file doc\UCODE_2005.txt for a description of this
software.  Instructions for installation, execution, and testing are provided
below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal
computers:

         ucode_2005_1.022.exe

For UNIX the ucode_2005_1.022.tgz file is provided

Numbers at the end of ucode_2005_#.### are changed to identify different
modifications. (e.g. ucode_2005_1.000 became ucode_2005_1.001 after the
first modification)

The distribution file contains:

          Compiled runfile and source code for UCODE_2005 and the six
              other programs.
          Supplementary UCODE_2005 documentation in PDF and text files.
          Test data sets.

The distribution file is a self-extracting program.  Execution of the
distribution file creates numerous individual files.  The extraction
program allows the user to specify the directory in which the files should
be restored (the default is C:\WRDAPP.  These installation instructions assume
that the files are restored into directory C:\WRDAPP.  The following directory
structure will be created in C:\WRDAPP:

   |
   |--ucode_2005_#.###
   |    |--bin            ; Compiled executables for personal computers
   |    |--doc            ; UCODE_2005 Documentation files
   |    |--src            ; UCODE_2005 source code for use on any computer
   |    |--test-data-win  ; Input data and batch files for process model runs
   |    |--test-win       ; Input files and batch files for UCODE_2005 and
                            associated codes

Numbers at the end of ucode_2005_#.### are changed to identify different
modifications. (e.g. ucode_2005_1.000 became ucode_2005_1.001 after the
first modification)

When the batch files in the test-win subdirectories are executed, process model
output files are listed in the test-data-win subdirectories and output and
data-exchange files created and used by UCODE_2005 and the six post-processors
are listed in the test-win subdirectories. Additional information is provided in
Appendix D of the pdf file in the doc directory (see below).

It is recommended that no user files are kept in the ucode_2005_#.### directory
structure.  If you choose to put your own files in the ucode_2005_#.###
directory structure, do so only by creating additional subdirectories.

Included in directory ucode_2005_#.###\doc are documentation files.  One
of them is a Portable Document Format (PDF) file. The PDF files are readable
and printable on various computer platforms using Acrobat Reader from Adobe.
The Acrobat Reader is freely available from the following World Wide Web
site:
      http://www.adobe.com/

Included in the directory ucode_2005_#.###\bin are executables for UCODE_2005,
the six other programs, a program named runner needed for parallel execution,
and an executable for MODFLOW-2000, which is used as the process model in the
test cases.


B. INSTALLING

To make the executables in the ucode_2005_#.###\bin directory accessible from
any directory, include the ucode_2005_#.###\bin directory in the PATH
environment variable.  Also, if a prior release of UCODE_2005 is installed on
your system, the directory containing the executables for the prior release
should be removed from the PATH environment variable.

As an alternative, the executable files in the UCODE_2005\bin directory can
be copied into a directory already included in the PATH environment variable.

       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
          WINDOWS9X AND WINDOWS ME SYSTEMS

Add the following line to the AUTOEXEC.BAT file:

  PATH=%PATH%;C:\WRDAPP\ucode_2005_#.###\bin

Reboot your system after modifying AUTOEXEC.BAT.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
               WINDOWS NT SYSTEMS

From the Start menu, select Settings and then Control Panel.  Double click
System and select the Environment tab. To add a new user variable, enter
"PATH" in the Variable field and enter

   %PATH%;C:\WRDAPP\ucode_2005_#.###\bin

in the Value field.  Click Set and then click OK.  If a PATH user variable
already is defined, click on it in the User Variables pane, add
";C:\WRDAPP\ucode_2005_#.###\bin" to its definition in the Value field, and
click OK. Initiate and use a new Windows Command Prompt window after making this
change.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
             WINDOWS 2000 OR XP SYSTEMS

From the Start menu, select Settings and then Control Panel.  Double click
System and select the Advanced tab.  Click on Environment Variables.  If
a PATH user variable already is defined, click on it in the User Variables
pane, then click Edit.  In the Edit User Variable window, add
";C:\WRDAPP\ucode_2005_#.###\bin" to the end of the Variable Value (ensure that
the current contents of the User Value are not deleted) and click OK.  If
a PATH user variable is not already defined, in the User variables pane of
the Environment Variables window, click New.  In the New User Variable
window, define a new variable PATH as shown above.  Click OK.  Click OK
in the Environment Variables window and again in the System Properties
window.  Initiate and use a new Windows Command Prompt window.


C. EXECUTING THE SOFTWARE

After the executable file in the ucode_2005_#.###\bin directory is installed in
a directory that is included in your PATH, UCODE_2005 is initiated in
a Windows Command-Prompt window using the command:

          UCODE_2005 input-file fn

The variable "input file' is the name of the main UCODE_2005 input file.
The "fn" variable is used as a filename prefix for output files.
For example, if the main UCODE_2005 input file is named abc.in and the
filename prefix is chosen to be ex1, then UCODE_2005 can be run by entering:

          UCODE_2005 abc.in  ex1

The data arrays in UCODE-2005 are dynamically allocated, so models
are not limited by hard-coded array limits. However, it is best to have
enough random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than this, the program will use
virtual memory, which slows computations significantly.

D. TESTING

Test data sets are provided to demonstrate that UCODE-2005 is correctly
installed and running on the system.  The tests may also be looked
at as examples of how to use the program.


E. COMPILING

The executable file provided in ucode_2005_1.022\bin was created using the
MicroSoft Visual Studio 2005, Intel Fortran Compiler 11.0. Although an executable
version of the program is provided, the source code is provided in the
ucode_2005_1.020\src directory so that UCODE_2005 and the other programs can be
recompiled if necessary.  However, the USGS cannot provide assistance to those
compiling UCODE. In general, the requirements are a Fortran compiler and knowing
how to use the compiler.

