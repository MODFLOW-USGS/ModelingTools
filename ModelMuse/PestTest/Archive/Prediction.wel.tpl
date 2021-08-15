ptf @
etf %
# WEL: Well package file created on 8/15/2021 by ModelMuse version 4.3.0.58.
# (and then modified by a parameter estimation program.)
BEGIN OPTIONS
    AUXILIARY IFACE
    BOUNDNAMES
    PRINT_INPUT
    SAVE_FLOWS
    AUTO_FLOW_REDUCE  1.000000000000E-006 
END OPTIONS

BEGIN DIMENSIONS
  MAXBOUND     1
END DIMENSIONS

BEGIN PERIOD      1
     1     3     7 %                    @                    Q_Par1@ * -0.02%      0 'Object7'  # Data Set 6: Layer Row Column Q IFACE boundname Intersected by Object7 with formula: -0.02 multiplied by the parameter value for "Q_Par1."
END PERIOD 

BEGIN PERIOD      2
     1     3     7 %                    @                    Q_Par1@ * -0.03%      0 'Object7'  # Data Set 6: Layer Row Column Q IFACE boundname Intersected by Object7 with formula: -0.03 multiplied by the parameter value for "Q_Par1."
END PERIOD 

