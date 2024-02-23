ptf @
etf %
# DRN: Drain package file created on 2/23/2024 by ModelMuse version 5.1.1.56.
# (and then modified by a parameter estimation program.)
BEGIN OPTIONS
    AUXILIARY IFACE DDRN
    BOUNDNAMES
    PRINT_INPUT
    SAVE_FLOWS
    OBS6 FILEIN PestPilotPointTest.ob_drob
    AUXDEPTHNAME DDRN
END OPTIONS

BEGIN DIMENSIONS
  MAXBOUND     1
END DIMENSIONS

BEGIN PERIOD      1
     1    10    10 -5.000000000000E+000  %                    @                    DRN_Par1@ * 1%      0  0.000000000000E+000  'Drain'  # Data Set 6: Layer Row Column Elevation Cond IFACE DDRN boundname
END PERIOD 

