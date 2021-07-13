ptf @
etf %
# CHD: Time-Variant Specified-Head package file created on 7/13/2021 by ModelMuse version 4.3.0.56.
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
     1     4     5 %                    @                    CHD_MF6@ * 2%      0 'Object2'  # Data Set 6: Layer Row Column Shead IFACE boundname
     2     4     5 %                    @                    CHD_MF6@ * 2%      0 'Object2'  # Data Set 6: Layer Row Column Shead IFACE boundname
     1    10    10 %                    @                    CHD_MF6@ * 1%      0 'Object3'  # Data Set 6: Layer Row Column Shead IFACE boundname
     2    10    10 %                    @                    CHD_MF6@ * 1%      0 'Object3'  # Data Set 6: Layer Row Column Shead IFACE boundname
END PERIOD 

