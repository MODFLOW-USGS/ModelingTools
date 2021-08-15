ptf @
etf %
# ETS: Evapotranspiration Segments package file created on 8/15/2021 by ModelMuse version 4.3.0.58.
# (and then modified by a parameter estimation program.)
BEGIN OPTIONS
  AUXILIARY IFACE
  PRINT_INPUT
    SAVE_FLOWS
    BOUNDNAMES
  FIXED_CELL
END OPTIONS

BEGIN DIMENSIONS
  MAXBOUND     4
  NSEG     1
END DIMENSIONS

BEGIN PERIOD      1
     1     3     3 -1.000000000000E+000  %                    @                    ETS_Par1@ * 0%   2.000000000000E+000      0 'Object4' 
END PERIOD 

BEGIN PERIOD      2
     1     6     6 -1.000000000000E+000  %                    @                    ETS_Par2@ * 0%   2.000000000000E+000      0 'Object5' 
END PERIOD 

