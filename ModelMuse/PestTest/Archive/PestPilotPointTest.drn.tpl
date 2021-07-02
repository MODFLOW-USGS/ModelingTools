ptf @
etf %
# DRN: Drain package file created on 7/2/2021 by ModelMuse version 4.3.0.56.
BEGIN OPTIONS
    AUXILIARY IFACE
    BOUNDNAMES
    PRINT_INPUT
    SAVE_FLOWS
    OBS6 FILEIN PestPilotPointTest.ob_drob
END OPTIONS

BEGIN DIMENSIONS
  MAXBOUND     1
END DIMENSIONS

BEGIN PERIOD      1
     1    10    10 -5.000000000000E+000  %                    @                    DRN_Par1@ * 1%      0 'Drain'  # Data Set 6: Layer Row Column Elevation Cond IFACE boundname
END PERIOD 

