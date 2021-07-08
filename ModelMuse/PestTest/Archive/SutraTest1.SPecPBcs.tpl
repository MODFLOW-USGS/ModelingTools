ptf @
etf %
# Specified Pressure Boundary Condition File file created on 7/8/2021 by ModelMuse version 4.3.0.56.
# Data set 1
'STEP_0' # BCSSCH
# Data set 2; Time = 1
'Specified Pressure'     0     0     2     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 5; Time = 1
     3 %                    1  *  @                    a@%  %                    1  +  @                    conc1@%  # Data Set 5: IPBC1, PBC1, UBC1
   -77 # Data Set 5: IPBC1
     0
# Data set 2; Time = 3
'Specified Pressure'     0     0     2     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 5; Time = 3
     3 %                    0  *  @                    a@%  %                    (1 *  @                    conc2@) +  @                    conc1@%  # Data Set 5: IPBC1, PBC1, UBC1
    77  1.000000000000E+000   1.000000000000E+000  # Data Set 5: IPBC1, PBC1, UBC1
     0
