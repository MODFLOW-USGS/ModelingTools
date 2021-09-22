ptf $
etf %
%ReadArrays(SutraTest1a.FluxBcs.txt)%
# Fluid Flux Boundary Condition File file created on 9/22/2021 by ModelMuse version 4.3.0.66.# (and then modified by a parameter estimation program.)

# Data set 1
'STEP_0' # BCSSCH
# Data set 2; Time = 1
'Fluid sources'     1     0     0     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 3; Time = 1
    77 %                    0.01  *  ~                    MyMult[1, 1, 20]~%  %                    (((1 *  $                    conc2$) *  $                    conc1$) * (0.01  *  ~                    MyMult[1, 1, 20]~)) / (0.01  *  ~                    MyMult[1, 1, 20]~)%  # Data Set 3: IQCP1, QINC1, UINC1
     0
