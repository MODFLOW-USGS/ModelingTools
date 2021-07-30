ptf @
etf %
# CHD: Time-Variant Specified-Head package file created on 7/30/2021 by ModelMuse version 4.3.0.56.
# (and then modified by a parameter estimation program.)
BEGIN OPTIONS
    AUXILIARY IFACE
    BOUNDNAMES
    PRINT_INPUT
    SAVE_FLOWS
END OPTIONS

BEGIN DIMENSIONS
  MAXBOUND     4
END DIMENSIONS

BEGIN PERIOD      1
     1     1     1  1.000000000000E+000      0 'Object0'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1     1    10  1.000000000000E+000      0 'Object1'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1     7    10 %                    @                    CHD_Par1@ * 2%      0 'Object2'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1    10    10 %                    @                    CHD_Par1@ * 5%      0 'Object3'  # Data Set 6: Layer Row Column Shead IFACE boundname
END PERIOD 

BEGIN PERIOD      2
     1     1    10  1.000000000000E+000      0 'Object1'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1     7    10 %                    @                    CHD_Par1@ * 3%      0 'Object2'  # Data Set 6: Layer Row Column Shead IFACE boundname
END PERIOD 

