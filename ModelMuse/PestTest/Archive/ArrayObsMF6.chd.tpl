ptf @
etf %
# CHD: Time-Variant Specified-Head package file created on 3/18/2024 by ModelMuse version 5.1.1.57.
# (and then modified by a parameter estimation program.)
BEGIN OPTIONS
    AUXILIARY IFACE
    BOUNDNAMES
    PRINT_INPUT
    SAVE_FLOWS
END OPTIONS

BEGIN DIMENSIONS
  MAXBOUND     7
END DIMENSIONS

BEGIN PERIOD      1
     1     1     1  3.000000000000E+001      0 'Object0'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1    10    10  2.900000000000E+001      0 'Object1'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1     1    10 %                    @                    CHD_Par1@ * 0.05%      0 'Object2'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1     2    10 %                    @                    CHD_Par1@ * 0.15%      0 'Object2'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1     3    10 %                    @                    CHD_Par1@ * 0.25%      0 'Object2'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1     4    10 %                    @                    CHD_Par1@ * 0.35%      0 'Object2'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1     5    10 %                    @                    CHD_Par1@ * 0.45%      0 'Object2'  # Data Set 6: Layer Row Column Shead IFACE boundname
END PERIOD 

