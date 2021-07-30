ptf @
etf %
# Fluid Flux Boundary Condition File file created on 7/30/2021 by ModelMuse version 4.3.0.56.# (and then modified by a parameter estimation program.)

# Data set 1
'FluidFlux' # BCSSCH
# Data set 2; Time = 1
'Fluid sources'     1     0     0     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 3; Time = 1
   171 %                    (0.01  *  @                    Flux1@) + (0.02  *  @                    Flux2@)%  %                    (((5) * (0.01  *  @                    Flux1@)) + (((1 *  @                    conc1@) *  @                    a@) * (0.02  *  @                    Flux2@))) / ((0.01  *  @                    Flux1@) + (0.02  *  @                    Flux2@))%  # Data Set 3: IQCP1, QINC1, UINC1
     0
# Data set 2; Time = 2
'Fluid sources'     1     0     0     0 # Data Set 2: BCSID, NSOP1, NSOU1, NPBC1, NUBC1
# Data set 3; Time = 2
   171 %                    (0.011  *  @                    Flux1@) + (0.021  *  @                    Flux2@)%  %                    (((2) * (0.011  *  @                    Flux1@)) + ((3  *  @                    a@) * (0.021  *  @                    Flux2@))) / ((0.011  *  @                    Flux1@) + (0.021  *  @                    Flux2@))%  # Data Set 3: IQCP1, QINC1, UINC1
     0
