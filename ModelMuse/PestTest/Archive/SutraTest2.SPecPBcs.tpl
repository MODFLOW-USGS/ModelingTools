ptf @
etf %
# Specified Pressure Boundary Condition File file created on 7/30/2021 by ModelMuse version 4.3.0.56.# (and then modified by a parameter estimation program.)

# Data set 1
'SpecifiedP' # BCSSCH
# Data set 2; Time = 1
'Specified Pressure'     0     0     2     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 5; Time = 1
   263  1.000000000000E+000   1.000000000000E+000  # Data Set 5: IPBC1, PBC1, UBC1
     7 %                    1 *  @                    a@%  %                    (1 *  @                    conc1@) *  @                    a@%  # Data Set 5: IPBC1, PBC1, UBC1
     0
# Data set 2; Time = 2
'Specified Pressure'     0     0     1     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 5; Time = 2
     7  2.000000000000E+000  %                    (1 *  @                    conc2@) *  @                    a@%  # Data Set 5: IPBC1, PBC1, UBC1
     0
