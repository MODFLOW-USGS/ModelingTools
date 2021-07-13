ptf @
etf %
# WEL: Well package file created on 7/13/2021 by ModelMuse version 4.3.0.56.
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
     1     3     7 %                    @                    Q_Par1@ * -2%      0 'Object7'  # Data Set 6: Layer Row Column Q IFACE boundname Intersected by Object7 with formula: -2 multiplied by the parameter value for "Q_Par1."
END PERIOD 

BEGIN PERIOD      2
     1     3     7 %                    @                    Q_Par1@ * -3%      0 'Object7'  # Data Set 6: Layer Row Column Q IFACE boundname Intersected by Object7 with formula: -3 multiplied by the parameter value for "Q_Par1."
END PERIOD 

