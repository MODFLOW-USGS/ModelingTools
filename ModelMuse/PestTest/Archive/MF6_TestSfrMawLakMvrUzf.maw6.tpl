ptf @
etf %
# MAW: Multi-Aquifer Well package file created on 7/30/2021 by ModelMuse version 4.3.0.56.
# (and then modified by a parameter estimation program.)
BEGIN OPTIONS
    BOUNDNAMES
    PRINT_INPUT
  PRINT_HEAD
  HEAD FILEOUT MF6_TestSfrMawLakMvrUzf.maw_head
    SAVE_FLOWS
  BUDGET FILEOUT MF6_TestSfrMawLakMvrUzf.maw_bud
  SHUTDOWN_THETA  7.000000000000E-001 
  SHUTDOWN_KAPPA  1.000000000000E-004 
    OBS6 FILEIN MF6_TestSfrMawLakMvrUzf.ob_maw
  MOVER
END OPTIONS

BEGIN DIMENSIONS
  NMAWWELLS     1
END DIMENSIONS

BEGIN PACKAGEDATA
# <wellno> <radius> <bottom> <strt> <condeqn> <ngwfnodes> [<aux(naux)>] [<boundname>]
     1 @                    WelRadius@  -3.000000000000E+001  -1.000000000000E+000  THIEM     3 'MAW' 
END PACKAGEDATA

BEGIN CONNECTIONDATA
# <wellno> <icon> <cellid(ncelldim)> <scrn_top> <scrn_bot> <hk_skin> <radius_skin>
     1     1     1     2     9  0.000000000000E+000  -3.000000000000E+001   1.000000000000E-004  @                    SkinRadius@ 
     1     2     2     2     9  0.000000000000E+000  -3.000000000000E+001   1.000000000000E-004  @                    SkinRadius@ 
     1     3     3     2     9  0.000000000000E+000  -3.000000000000E+001   1.000000000000E-004  @                    SkinRadius@ 
END CONNECTIONDATA

BEGIN PERIOD      1
# <wellno> <mawsetting>
     1 STATUS ACTIVE
     1 RATE %                    -0.0001  *  @                    Sfr1@% 
END PERIOD 

