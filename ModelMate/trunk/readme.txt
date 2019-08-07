readme.txt


                         ModelMate - Version 1.0.3
                Graphical user interface for model analysis
      
NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This directory and its subdirectories contain the distribution for ModelMate 
(Banta, 2011), a graphical user interface for model analysis.  Executable 
files, example files, and a tutorial are included.  Please see file release.txt 
in this directory for release notes.


                            TABLE OF CONTENTS
                            
                         A. Distribution file
                         B. Installing
                         C. Starting ModelMate
                         D. Tutorial
                         E. Source code and executables
                         F. References


A. Distribution file

ModelMate is distributed for use under the Microsoft Windows operating system 
in an installer file named:

    ModelMate_setup_1.0.3.exe
     

B. Installing

Execution of the installer file requires administrator privileges creates a 
directory containing a number of subdirectories and files.  The installer 
program allows the user to specify the directory in which the files should be 
restored.  By default the distribution will be installed into C:\WRDAPP.  
Wherever the file contents are extracted, the following directory structure 
will be created:   

ModelMate_1_0_3
    Bin                (Executable files)
    Completed_Example  (Example, completed according to tutorial)
    Doc                (Documentation file(s), including tutorial)
    Example            (Files needed for example)
    uninstall          (Uninstaller program)

It is recommended that no user files are kept in the ModelMate directory
structure.

The top-level directory contains two text files:
    
    readme.txt         (this file)
    release.txt        (release notes)

The Bin directory contains two executable files:
  
    ModelMate.exe
    accjupiter.dll
  
These two files need to be located in the same directory.  At run time, 
ModelMate links to accjupiter.dll.  If it is desired to copy ModelMate to 
another directory, ensure that both files are copied.

The Completed_Example, Doc, and Example directories contain files related to 
the tutorial.  (See section D. Tutorial.)


C. Starting ModelMate

Users may find it convenient to let the installer associate the extension 
"mtc" with the ModelMate.exe executable.  When this association is made, 
ModelMate can be started by opening any file with the extension "mtc" in 
Windows Explorer.  

ModelMate and ModelMuse (Winston, 2009) are designed to work together.  
ModelMate can be invoked from ModelMuse, as described in Banta (2011).


D. Tutorial

A Portable Document Format (PDF) file named ModelMate_tutorial.pdf is provided 
in the Doc directory.  This file can be opened with Adobe Reader, which is 
freely available at:

    http://www.adobe.com/

The file ModelMate_tutorial.pdf contains instructions for using ModelMate with 
a set of MODFLOW-2005 (Harbaugh, 2005) files provided in the Example directory.  
The tutorial demonstrates the use of various capabilities of ModelMate, using 
UCODE_2005 (Poeter and others, 2005) to perform analyses, and using GW_Chart 
(Winston, 2000) to graphically illustrate analysis results.  The tutorial also 
can be run using UCODE_2014 (Poeter and others, 2014) in place of UCODE_2005.


E. Source code and executables

ModelMate.exe is the main executable file for ModelMate.  ModelMate.exe is 
compiled from source code written in Delphi, an object-oriented version of the 
Pascal programming language.  It was compiled using Embarcadero Delphi XE 
version 15.0.

The file accjupiter.dll is a dynamic link library required by ModelMate that 
provides access to selected data, subroutines, and functions of the JUPITER API 
(Banta and others, 2006).  Accjupiter.dll is written in Fortran-90 and was 
compiled using Intel Parallel Studio XE 2016 Composer Edition for Fortran, 
version 16.0.

Source code may be obtained from the author:

    Edward (Ned) Banta
    erbanta@usgs.gov

However, the USGS cannot provide assistance for versions of ModelMate other 
than the one provided for download on the USGS water-software web page 
(http://water.usgs.gov/software/).


F. References

Banta, E.R., 2011, ModelMate--A graphical user interface for model analysis: 
U.S. Geological Survey Techniques and Methods, book 6, chap. E4, 31 p.

Banta, E.R., Poeter, E.P., Doherty, J.E., and Hill, M.C., 2006, JUPITER: 
Joint Universal Parameter IdenTification and Evaluation of Reliability--An 
application programming interface (API) for model analysis: U.S. Geological 
Survey Techniques and Methods, book 6, chap. E1, 268 p.  (Also available at 
http://pubs.er.usgs.gov/usgspubs/tm/tm6E1.)

Harbaugh, A.W., 2005, MODFLOW-2005, the U.S. Geological Survey modular 
ground-water model--The Ground-Water Flow Process: U.S. Geological Survey 
Techniques and Methods, book 6, chap. A16, variously paginated.  (Also 
available at http://pubs.usgs.gov/tm/2005/tm6A16/PDF/TM6A16.pdf.)

Poeter, E.P., Hill, M.C., Banta, E.R., Mehl, Steffen, and Christensen, 
Steen, 2005, UCODE_2005 and six other computer codes for universal 
sensitivity analysis, calibration, and uncertainty evaluation constructed 
using the JUPITER API: U.S. Geological Survey Techniques and Methods, book 
6, chap. A11, 283 p.  (Also available at 
http://pubs.er.usgs.gov/usgspubs/tm/tm6A11.)

Poeter, E.P., Hill, M.C., Lu, Dan, Tiedeman, C.R., and Mehl, Steffen, 2014, 
UCODE_2014, with new capabilities to define parameters unique to 
predictions, calculate weights using simulated values, estimate parameters 
with SVD, evaluate uncertainty with MCMC, and more: Integrated Groundwater 
Modeling Center Report Number GWMI 2014-02

Winston, R.B., 2000, Graphical user interface for MODFLOW, Version 4: U.S. 
Geological Survey Open-File Report 2000-315, 27 p., available at 
http://water.usgs.gov/nrp/gwsoftware/mfgui4/modflow-gui.html; the GW_Chart 
graphing program documented in this report is available at 
http://water.usgs.gov/nrp/gwsoftware/GW_Chart/GW_Chart.html.

Winston, R.B., 2009, ModelMuse--A graphical user interface for MODFLOW-2005 
and PHAST: U.S. Geological Survey Techniques and Methods, book 6, chap. A29, 
52 p., available only online at 
http://water.usgs.gov/nrp/gwsoftware/ModelMuse/ModelMuse.html
