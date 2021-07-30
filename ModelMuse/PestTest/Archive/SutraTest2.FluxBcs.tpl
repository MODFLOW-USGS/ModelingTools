ptf @
etf %
# Fluid Flux Boundary Condition File file created on 7/30/2021 by ModelMuse version 4.3.0.56.# (and then modified by a parameter estimation program.)

# Data set 1
'FluidFlux' # BCSSCH
# Data set 2; Time = 1
'Fluid sources'     3     0     0     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 3; Time = 1
   171 %                    (0.01  *  @                    conc1@) + (0.02  *  @                    conc1@)%   4.333333333333E+000  # Data Set 3: IQCP1, QINC1, UINC1
    61  1.000000000000E+000   1.000000000000E-003  # Data Set 3: IQCP1, QINC1, UINC1
   189  1.000000000000E-003   1.000000000000E+000  # Data Set 3: IQCP1, QINC1, UINC1
     0
# Data set 2; Time = 2
'Fluid sources'     3     0     0     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 3; Time = 2
   171 %                    (0.011  *  @                    conc1@) + (0.021  *  @                    conc1@)%   2.656250000000E+000  # Data Set 3: IQCP1, QINC1, UINC1
   -61 # Data Set 3: IQCP1
   189  1.000000000000E-003   2.000000000000E+000  # Data Set 3: IQCP1, QINC1, UINC1
     0
# Data set 2; Time = 3.5
'Fluid sources'     2     0     0     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 3; Time = 3.5
   171 %                    (0.011  *  @                    conc1@) + (0.021  *  @                    conc1@)%   2.656250000000E+000  # Data Set 3: IQCP1, QINC1, UINC1
  -189 # Data Set 3: IQCP1
     0
