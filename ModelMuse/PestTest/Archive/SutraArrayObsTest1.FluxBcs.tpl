ptf $
etf %
# Fluid Flux Boundary Condition File file created on 3/18/2024 by ModelMuse version 5.1.1.57.# (and then modified by a parameter estimation program.)

# Data set 1
'STEP_0' # BCSSCH
# Data set 2; Time = 1
'Fluid sources'     1     0     0     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 3; Time = 1
    20 %                    0.01  *  $                    a$%  %                    (((1 *  $                    conc2$) *  $                    conc1$) * (0.01  *  $                    a$)) / (0.01  *  $                    a$)%  # Data Set 3: IQCP1, QINC1, UINC1
     0
