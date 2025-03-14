ptf @
etf %
%ReadArrays(MF6_TestSfrMawLakMvrUzf.sfr.txt)%
# SFR: MODFLOW-6 Stream Flow Routing package file created on 2/6/2025 by ModelMuse version 5.3.1.7.
# (and then modified by a parameter estimation program.)
BEGIN OPTIONS
    PRINT_INPUT
    PRINT_STAGE
    PRINT_FLOWS
    BOUNDNAMES
    SAVE_FLOWS
    STAGE FILEOUT MF6_TestSfrMawLakMvrUzf.stage
    BUDGET FILEOUT MF6_TestSfrMawLakMvrUzf.sfr_budget
    BUDGETCSV FILEOUT MF6_TestSfrMawLakMvrUzf.sfr.sfr_budget.csv
  PACKAGE_CONVERGENCE FILEOUT MF6_TestSfrMawLakMvrUzf.SfrConvergence.csv
    MAXIMUM_ITERATIONS   100
    MAXIMUM_DEPTH_CHANGE  1.000000000000E-005 
    OBS6 FILEIN MF6_TestSfrMawLakMvrUzf.ob_sfr
  MOVER
END OPTIONS

BEGIN DIMENSIONS
    NREACHES     13
END DIMENSIONS

BEGIN PACKAGEDATA
# <rno>   <cellid>        <rlen>                <rwid>                <rgrd>                <rtp>                 <rbth>                <rhk>                 <man>                <ncon> <ustrf>                  <ndv> [<aux(naux)>] [<boundname>]
# defined by Sfr1
     1     3     2     3  2.886153787657E+001  %                    1 *  ~                    SfrWidth[1, 2, 3]~%  @                    SfrGrad@   1.000000000000E+000   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      1  1.000000000000E+000      0 Sfr1
     2     3     3     3  1.039015363557E+002  %                    1 *  ~                    SfrWidth[1, 3, 3]~%  @                    SfrGrad@   3.777754878816E-001   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 Sfr1
     3     3     4     3  6.739611320758E+001  %                    1 *  ~                    SfrWidth[1, 4, 3]~%  @                    SfrGrad@  -4.695027196011E-001   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 Sfr1
     4     3     4     4  1.033316283049E+002  %                    1 *  ~                    SfrWidth[1, 4, 4]~%  @                    SfrGrad@  -9.389582652111E-001   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 Sfr1
     5     3     4     5  8.865838114277E+001  %                    1 *  ~                    SfrWidth[1, 4, 5]~%  @                    SfrGrad@  -1.678083874866E+000   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 Sfr1
     6     3     5     5  1.467324716214E+001  %                    1 *  ~                    SfrWidth[1, 5, 5]~%  @                    SfrGrad@  -2.075891307593E+000   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 Sfr1
     7     3     5     6  1.033316283049E+002  %                    1 *  ~                    SfrWidth[1, 5, 6]~%  @                    SfrGrad@  -2.530187996117E+000   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 Sfr1
     8     3     5     7  9.351697543724E+000  %                    1 *  ~                    SfrWidth[1, 5, 7]~%  @                    SfrGrad@  -3.000000000000E+000   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 Sfr1
# defined by SFR2
     9     3     6     7  1.111485853486E+001  @                    SfrPWidth@   1.000000000000E-003  -1.000000000000E+000   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 SFR2
    10     3     7     7  1.000337268138E+002  @                    SfrPWidth@   1.000000000000E-003  -1.000000000000E+000   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 SFR2
    11     3     8     7  1.000337268138E+002  @                    SfrPWidth@   1.000000000000E-003  -1.000000000000E+000   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 SFR2
    12     3     9     7  1.000337268138E+002  @                    SfrPWidth@   1.000000000000E-003  -1.000000000000E+000   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      2  1.000000000000E+000      0 SFR2
    13     3    10     7  3.836267807850E+001  @                    SfrPWidth@   1.000000000000E-003  -1.000000000000E+000   1.000000000000E+000   1.000000000000E-004   3.000000000000E-002      1  1.000000000000E+000      0 SFR2
END PACKAGEDATA

BEGIN CONNECTIONDATA
# defined by Sfr1
     1    -2
     2     1    -3
     3     2    -4
     4     3    -5
     5     4    -6
     6     5    -7
     7     6    -8
     8     7    -9
# defined by SFR2
     9   -10     8
    10     9   -11
    11    10   -12
    12    11   -13
    13    12
END CONNECTIONDATA

BEGIN INITIALSTAGES
# <rno>  <initialstage>
# defined by Sfr1
     1  0.000000000000E+000 
     2  0.000000000000E+000 
     3  0.000000000000E+000 
     4  0.000000000000E+000 
     5  0.000000000000E+000 
     6  0.000000000000E+000 
     7  0.000000000000E+000 
     8  0.000000000000E+000 
# defined by SFR2
     9  0.000000000000E+000 
    10  0.000000000000E+000 
    11  0.000000000000E+000 
    12  0.000000000000E+000 
    13  0.000000000000E+000 
END INITIALSTAGES

BEGIN PERIOD      1
# rno sfrsetting (defined by Sfr1)
     1 STATUS SIMPLE
     1 UPSTREAM_FRACTION  1.000000000000E+000 
     1 STAGE %                    2  +  @                    Sfr1@% 
     1 INFLOW %                    0.1  *  @                    Sfr2@% 
     1 RAINFALL %                    1E-008  *  @                    Sfr2@% 
     1 EVAPORATION  1.000000000000E-009 
     1 RUNOFF  1.000000000000E-001 

     2 STATUS SIMPLE
     2 UPSTREAM_FRACTION  1.000000000000E+000 
     2 STAGE %                    1.37777548788163  +  @                    Sfr1@% 
     2 INFLOW  0.000000000000E+000 
     2 RAINFALL %                    1E-008  *  @                    Sfr2@% 
     2 EVAPORATION  1.000000000000E-009 
     2 RUNOFF  1.000000000000E-001 

     3 STATUS SIMPLE
     3 UPSTREAM_FRACTION  1.000000000000E+000 
     3 STAGE %                    0.530497280398945  +  @                    Sfr1@% 
     3 INFLOW  0.000000000000E+000 
     3 RAINFALL %                    1E-008  *  @                    Sfr2@% 
     3 EVAPORATION  1.000000000000E-009 
     3 RUNOFF  1.000000000000E-001 

     4 STATUS SIMPLE
     4 UPSTREAM_FRACTION  1.000000000000E+000 
     4 STAGE %                    0.0610417347889425  +  @                    Sfr1@% 
     4 INFLOW  0.000000000000E+000 
     4 RAINFALL %                    1E-008  *  @                    Sfr2@% 
     4 EVAPORATION  1.000000000000E-009 
     4 RUNOFF  1.000000000000E-001 

     5 STATUS SIMPLE
     5 UPSTREAM_FRACTION  1.000000000000E+000 
     5 STAGE %                    -0.678083874866066  +  @                    Sfr1@% 
     5 INFLOW  0.000000000000E+000 
     5 RAINFALL %                    1E-008  *  @                    Sfr2@% 
     5 EVAPORATION  1.000000000000E-009 
     5 RUNOFF  1.000000000000E-001 

     6 STATUS SIMPLE
     6 UPSTREAM_FRACTION  1.000000000000E+000 
     6 STAGE %                    -1.07589130759254  +  @                    Sfr1@% 
     6 INFLOW  0.000000000000E+000 
     6 RAINFALL %                    1E-008  *  @                    Sfr2@% 
     6 EVAPORATION  1.000000000000E-009 
     6 RUNOFF  1.000000000000E-001 

     7 STATUS SIMPLE
     7 UPSTREAM_FRACTION  1.000000000000E+000 
     7 STAGE %                    -1.53018799611694  +  @                    Sfr1@% 
     7 INFLOW  0.000000000000E+000 
     7 RAINFALL %                    1E-008  *  @                    Sfr2@% 
     7 EVAPORATION  1.000000000000E-009 
     7 RUNOFF  1.000000000000E-001 

     8 STATUS SIMPLE
     8 UPSTREAM_FRACTION  1.000000000000E+000 
     8 STAGE %                    -2  +  @                    Sfr1@% 
     8 INFLOW  0.000000000000E+000 
     8 RAINFALL %                    1E-008  *  @                    Sfr2@% 
     8 EVAPORATION  1.000000000000E-009 
     8 RUNOFF  1.000000000000E-001 

# rno sfrsetting (defined by SFR2)
     9 STATUS ACTIVE
     9 UPSTREAM_FRACTION  1.000000000000E+000 
     9 INFLOW  0.000000000000E+000 
     9 RAINFALL  0.000000000000E+000 
     9 EVAPORATION  0.000000000000E+000 
     9 RUNOFF  0.000000000000E+000 
     9 MANNING  3.000000000000E-002 

    10 STATUS ACTIVE
    10 UPSTREAM_FRACTION  1.000000000000E+000 
    10 INFLOW  0.000000000000E+000 
    10 RAINFALL  0.000000000000E+000 
    10 EVAPORATION  0.000000000000E+000 
    10 RUNOFF  0.000000000000E+000 
    10 MANNING  3.000000000000E-002 

    11 STATUS ACTIVE
    11 UPSTREAM_FRACTION  1.000000000000E+000 
    11 INFLOW  0.000000000000E+000 
    11 RAINFALL  0.000000000000E+000 
    11 EVAPORATION  0.000000000000E+000 
    11 RUNOFF  0.000000000000E+000 
    11 MANNING  3.000000000000E-002 

    12 STATUS ACTIVE
    12 UPSTREAM_FRACTION  1.000000000000E+000 
    12 INFLOW  0.000000000000E+000 
    12 RAINFALL  0.000000000000E+000 
    12 EVAPORATION  0.000000000000E+000 
    12 RUNOFF  0.000000000000E+000 
    12 MANNING  3.000000000000E-002 

    13 STATUS ACTIVE
    13 UPSTREAM_FRACTION  1.000000000000E+000 
    13 INFLOW  0.000000000000E+000 
    13 RAINFALL  0.000000000000E+000 
    13 EVAPORATION  0.000000000000E+000 
    13 RUNOFF  0.000000000000E+000 
    13 MANNING  3.000000000000E-002 

END PERIOD 

