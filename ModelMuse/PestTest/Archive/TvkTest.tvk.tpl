ptf @
etf %
# TVK: Time-Varying Hydraulic Conductivity Package file created on 9/28/2023 by ModelMuse version 5.1.1.39.
# (and then modified by a parameter estimation program.)
BEGIN OPTIONS
    PRINT_INPUT
    TS6 FILEIN TvkTest.tvk.KValues.ts
END OPTIONS

BEGIN PERIOD      1
END PERIOD 

BEGIN PERIOD      2
     1     2     2 K KxTimes
     1     2     2 K22 %                    1E-006  *  @                    KYParam@% 
     1     2     2 K33  1.000000000000E-007 
     1     3     2 K KxTimes
     1     3     2 K22 %                    1E-006  *  @                    KYParam@% 
     1     3     2 K33  1.000000000000E-007 
     1     4     2 K KxTimes
     1     4     2 K22 %                    1E-006  *  @                    KYParam@% 
     1     4     2 K33  1.000000000000E-007 
     1     5     2 K KxTimes
     1     5     2 K22 %                    1E-006  *  @                    KYParam@% 
     1     5     2 K33  1.000000000000E-007 
     1     6     2 K KxTimes
     1     6     2 K22 %                    1E-006  *  @                    KYParam@% 
     1     6     2 K33  1.000000000000E-007 
     1     7     2 K KxTimes
     1     7     2 K22 %                    1E-006  *  @                    KYParam@% 
     1     7     2 K33  1.000000000000E-007 
     1     8     2 K KxTimes
     1     8     2 K22 %                    1E-006  *  @                    KYParam@% 
     1     8     2 K33  1.000000000000E-007 
     1     9     2 K KxTimes
     1     9     2 K22 %                    1E-006  *  @                    KYParam@% 
     1     9     2 K33  1.000000000000E-007 
     1     2     3 K KxTimes
     1     2     3 K22 %                    1E-006  *  @                    KYParam@% 
     1     2     3 K33  1.000000000000E-007 
     1     3     3 K KxTimes
     1     3     3 K22 %                    1E-006  *  @                    KYParam@% 
     1     3     3 K33  1.000000000000E-007 
     1     4     3 K KxTimes
     1     4     3 K22 %                    1E-006  *  @                    KYParam@% 
     1     4     3 K33  1.000000000000E-007 
     1     5     3 K KxTimes
     1     5     3 K22 %                    1E-006  *  @                    KYParam@% 
     1     5     3 K33  1.000000000000E-007 
     1     6     3 K KxTimes
     1     6     3 K22 %                    1E-006  *  @                    KYParam@% 
     1     6     3 K33  1.000000000000E-007 
     1     7     3 K KxTimes
     1     7     3 K22 %                    1E-006  *  @                    KYParam@% 
     1     7     3 K33  1.000000000000E-007 
     1     8     3 K KxTimes
     1     8     3 K22 %                    1E-006  *  @                    KYParam@% 
     1     8     3 K33  1.000000000000E-007 
     1     9     3 K KxTimes
     1     9     3 K22 %                    1E-006  *  @                    KYParam@% 
     1     9     3 K33  1.000000000000E-007 
     1     2     4 K KxTimes
     1     2     4 K22 %                    1E-006  *  @                    KYParam@% 
     1     2     4 K33  1.000000000000E-007 
     1     3     4 K KxTimes
     1     3     4 K22 %                    1E-006  *  @                    KYParam@% 
     1     3     4 K33  1.000000000000E-007 
     1     4     4 K KxTimes
     1     4     4 K22 %                    1E-006  *  @                    KYParam@% 
     1     4     4 K33  1.000000000000E-007 
     1     5     4 K KxTimes
     1     5     4 K22 %                    1E-006  *  @                    KYParam@% 
     1     5     4 K33  1.000000000000E-007 
     1     6     4 K KxTimes
     1     6     4 K22 %                    1E-006  *  @                    KYParam@% 
     1     6     4 K33  1.000000000000E-007 
     1     7     4 K KxTimes
     1     7     4 K22 %                    1E-006  *  @                    KYParam@% 
     1     7     4 K33  1.000000000000E-007 
     1     8     4 K KxTimes
     1     8     4 K22 %                    1E-006  *  @                    KYParam@% 
     1     8     4 K33  1.000000000000E-007 
     1     9     4 K KxTimes
     1     9     4 K22 %                    1E-006  *  @                    KYParam@% 
     1     9     4 K33  1.000000000000E-007 
     1     2     5 K KxTimes
     1     2     5 K22 %                    1E-006  *  @                    KYParam@% 
     1     2     5 K33  1.000000000000E-007 
     1     3     5 K KxTimes
     1     3     5 K22 %                    1E-006  *  @                    KYParam@% 
     1     3     5 K33  1.000000000000E-007 
     1     4     5 K KxTimes
     1     4     5 K22 %                    1E-006  *  @                    KYParam@% 
     1     4     5 K33  1.000000000000E-007 
     1     5     5 K KxTimes
     1     5     5 K22 %                    1E-006  *  @                    KYParam@% 
     1     5     5 K33  1.000000000000E-007 
     1     6     5 K KxTimes
     1     6     5 K22 %                    1E-006  *  @                    KYParam@% 
     1     6     5 K33  1.000000000000E-007 
     1     7     5 K KxTimes
     1     7     5 K22 %                    1E-006  *  @                    KYParam@% 
     1     7     5 K33  1.000000000000E-007 
     1     8     5 K KxTimes
     1     8     5 K22 %                    1E-006  *  @                    KYParam@% 
     1     8     5 K33  1.000000000000E-007 
     1     9     5 K KxTimes
     1     9     5 K22 %                    1E-006  *  @                    KYParam@% 
     1     9     5 K33  1.000000000000E-007 
     1     2     6 K KxTimes
     1     2     6 K22 %                    1E-006  *  @                    KYParam@% 
     1     2     6 K33  1.000000000000E-007 
     1     3     6 K KxTimes
     1     3     6 K22 %                    1E-006  *  @                    KYParam@% 
     1     3     6 K33  1.000000000000E-007 
     1     4     6 K KxTimes
     1     4     6 K22 %                    1E-006  *  @                    KYParam@% 
     1     4     6 K33  1.000000000000E-007 
     1     5     6 K KxTimes
     1     5     6 K22 %                    1E-006  *  @                    KYParam@% 
     1     5     6 K33  1.000000000000E-007 
     1     6     6 K KxTimes
     1     6     6 K22 %                    1E-006  *  @                    KYParam@% 
     1     6     6 K33  1.000000000000E-007 
     1     7     6 K KxTimes
     1     7     6 K22 %                    1E-006  *  @                    KYParam@% 
     1     7     6 K33  1.000000000000E-007 
     1     8     6 K KxTimes
     1     8     6 K22 %                    1E-006  *  @                    KYParam@% 
     1     8     6 K33  1.000000000000E-007 
     1     9     6 K KxTimes
     1     9     6 K22 %                    1E-006  *  @                    KYParam@% 
     1     9     6 K33  1.000000000000E-007 
     1     2     7 K KxTimes
     1     2     7 K22 %                    1E-006  *  @                    KYParam@% 
     1     2     7 K33  1.000000000000E-007 
     1     3     7 K KxTimes
     1     3     7 K22 %                    1E-006  *  @                    KYParam@% 
     1     3     7 K33  1.000000000000E-007 
     1     4     7 K KxTimes
     1     4     7 K22 %                    1E-006  *  @                    KYParam@% 
     1     4     7 K33  1.000000000000E-007 
     1     5     7 K KxTimes
     1     5     7 K22 %                    1E-006  *  @                    KYParam@% 
     1     5     7 K33  1.000000000000E-007 
     1     6     7 K KxTimes
     1     6     7 K22 %                    1E-006  *  @                    KYParam@% 
     1     6     7 K33  1.000000000000E-007 
     1     7     7 K KxTimes
     1     7     7 K22 %                    1E-006  *  @                    KYParam@% 
     1     7     7 K33  1.000000000000E-007 
     1     8     7 K KxTimes
     1     8     7 K22 %                    1E-006  *  @                    KYParam@% 
     1     8     7 K33  1.000000000000E-007 
     1     9     7 K KxTimes
     1     9     7 K22 %                    1E-006  *  @                    KYParam@% 
     1     9     7 K33  1.000000000000E-007 
     1     2     8 K KxTimes
     1     2     8 K22 %                    1E-006  *  @                    KYParam@% 
     1     2     8 K33  1.000000000000E-007 
     1     3     8 K KxTimes
     1     3     8 K22 %                    1E-006  *  @                    KYParam@% 
     1     3     8 K33  1.000000000000E-007 
     1     4     8 K KxTimes
     1     4     8 K22 %                    1E-006  *  @                    KYParam@% 
     1     4     8 K33  1.000000000000E-007 
     1     5     8 K KxTimes
     1     5     8 K22 %                    1E-006  *  @                    KYParam@% 
     1     5     8 K33  1.000000000000E-007 
     1     6     8 K KxTimes
     1     6     8 K22 %                    1E-006  *  @                    KYParam@% 
     1     6     8 K33  1.000000000000E-007 
     1     7     8 K KxTimes
     1     7     8 K22 %                    1E-006  *  @                    KYParam@% 
     1     7     8 K33  1.000000000000E-007 
     1     8     8 K KxTimes
     1     8     8 K22 %                    1E-006  *  @                    KYParam@% 
     1     8     8 K33  1.000000000000E-007 
     1     9     8 K KxTimes
     1     9     8 K22 %                    1E-006  *  @                    KYParam@% 
     1     9     8 K33  1.000000000000E-007 
     1     2     9 K KxTimes
     1     2     9 K22 %                    1E-006  *  @                    KYParam@% 
     1     2     9 K33  1.000000000000E-007 
     1     3     9 K KxTimes
     1     3     9 K22 %                    1E-006  *  @                    KYParam@% 
     1     3     9 K33  1.000000000000E-007 
     1     4     9 K KxTimes
     1     4     9 K22 %                    1E-006  *  @                    KYParam@% 
     1     4     9 K33  1.000000000000E-007 
     1     5     9 K KxTimes
     1     5     9 K22 %                    1E-006  *  @                    KYParam@% 
     1     5     9 K33  1.000000000000E-007 
     1     6     9 K KxTimes
     1     6     9 K22 %                    1E-006  *  @                    KYParam@% 
     1     6     9 K33  1.000000000000E-007 
     1     7     9 K KxTimes
     1     7     9 K22 %                    1E-006  *  @                    KYParam@% 
     1     7     9 K33  1.000000000000E-007 
     1     8     9 K KxTimes
     1     8     9 K22 %                    1E-006  *  @                    KYParam@% 
     1     8     9 K33  1.000000000000E-007 
     1     9     9 K KxTimes
     1     9     9 K22 %                    1E-006  *  @                    KYParam@% 
     1     9     9 K33  1.000000000000E-007 
     2     2     2 K KxTimes
     2     2     2 K22 %                    1E-006  *  @                    KYParam@% 
     2     2     2 K33  1.000000000000E-007 
     2     3     2 K KxTimes
     2     3     2 K22 %                    1E-006  *  @                    KYParam@% 
     2     3     2 K33  1.000000000000E-007 
     2     4     2 K KxTimes
     2     4     2 K22 %                    1E-006  *  @                    KYParam@% 
     2     4     2 K33  1.000000000000E-007 
     2     5     2 K KxTimes
     2     5     2 K22 %                    1E-006  *  @                    KYParam@% 
     2     5     2 K33  1.000000000000E-007 
     2     6     2 K KxTimes
     2     6     2 K22 %                    1E-006  *  @                    KYParam@% 
     2     6     2 K33  1.000000000000E-007 
     2     7     2 K KxTimes
     2     7     2 K22 %                    1E-006  *  @                    KYParam@% 
     2     7     2 K33  1.000000000000E-007 
     2     8     2 K KxTimes
     2     8     2 K22 %                    1E-006  *  @                    KYParam@% 
     2     8     2 K33  1.000000000000E-007 
     2     9     2 K KxTimes
     2     9     2 K22 %                    1E-006  *  @                    KYParam@% 
     2     9     2 K33  1.000000000000E-007 
     2     2     3 K KxTimes
     2     2     3 K22 %                    1E-006  *  @                    KYParam@% 
     2     2     3 K33  1.000000000000E-007 
     2     3     3 K KxTimes
     2     3     3 K22 %                    1E-006  *  @                    KYParam@% 
     2     3     3 K33  1.000000000000E-007 
     2     4     3 K KxTimes
     2     4     3 K22 %                    1E-006  *  @                    KYParam@% 
     2     4     3 K33  1.000000000000E-007 
     2     5     3 K KxTimes
     2     5     3 K22 %                    1E-006  *  @                    KYParam@% 
     2     5     3 K33  1.000000000000E-007 
     2     6     3 K KxTimes
     2     6     3 K22 %                    1E-006  *  @                    KYParam@% 
     2     6     3 K33  1.000000000000E-007 
     2     7     3 K KxTimes
     2     7     3 K22 %                    1E-006  *  @                    KYParam@% 
     2     7     3 K33  1.000000000000E-007 
     2     8     3 K KxTimes
     2     8     3 K22 %                    1E-006  *  @                    KYParam@% 
     2     8     3 K33  1.000000000000E-007 
     2     9     3 K KxTimes
     2     9     3 K22 %                    1E-006  *  @                    KYParam@% 
     2     9     3 K33  1.000000000000E-007 
     2     2     4 K KxTimes
     2     2     4 K22 %                    1E-006  *  @                    KYParam@% 
     2     2     4 K33  1.000000000000E-007 
     2     3     4 K KxTimes
     2     3     4 K22 %                    1E-006  *  @                    KYParam@% 
     2     3     4 K33  1.000000000000E-007 
     2     4     4 K KxTimes
     2     4     4 K22 %                    1E-006  *  @                    KYParam@% 
     2     4     4 K33  1.000000000000E-007 
     2     5     4 K KxTimes
     2     5     4 K22 %                    1E-006  *  @                    KYParam@% 
     2     5     4 K33  1.000000000000E-007 
     2     6     4 K KxTimes
     2     6     4 K22 %                    1E-006  *  @                    KYParam@% 
     2     6     4 K33  1.000000000000E-007 
     2     7     4 K KxTimes
     2     7     4 K22 %                    1E-006  *  @                    KYParam@% 
     2     7     4 K33  1.000000000000E-007 
     2     8     4 K KxTimes
     2     8     4 K22 %                    1E-006  *  @                    KYParam@% 
     2     8     4 K33  1.000000000000E-007 
     2     9     4 K KxTimes
     2     9     4 K22 %                    1E-006  *  @                    KYParam@% 
     2     9     4 K33  1.000000000000E-007 
     2     2     5 K KxTimes
     2     2     5 K22 %                    1E-006  *  @                    KYParam@% 
     2     2     5 K33  1.000000000000E-007 
     2     3     5 K KxTimes
     2     3     5 K22 %                    1E-006  *  @                    KYParam@% 
     2     3     5 K33  1.000000000000E-007 
     2     4     5 K KxTimes
     2     4     5 K22 %                    1E-006  *  @                    KYParam@% 
     2     4     5 K33  1.000000000000E-007 
     2     5     5 K KxTimes
     2     5     5 K22 %                    1E-006  *  @                    KYParam@% 
     2     5     5 K33  1.000000000000E-007 
     2     6     5 K KxTimes
     2     6     5 K22 %                    1E-006  *  @                    KYParam@% 
     2     6     5 K33  1.000000000000E-007 
     2     7     5 K KxTimes
     2     7     5 K22 %                    1E-006  *  @                    KYParam@% 
     2     7     5 K33  1.000000000000E-007 
     2     8     5 K KxTimes
     2     8     5 K22 %                    1E-006  *  @                    KYParam@% 
     2     8     5 K33  1.000000000000E-007 
     2     9     5 K KxTimes
     2     9     5 K22 %                    1E-006  *  @                    KYParam@% 
     2     9     5 K33  1.000000000000E-007 
     2     2     6 K KxTimes
     2     2     6 K22 %                    1E-006  *  @                    KYParam@% 
     2     2     6 K33  1.000000000000E-007 
     2     3     6 K KxTimes
     2     3     6 K22 %                    1E-006  *  @                    KYParam@% 
     2     3     6 K33  1.000000000000E-007 
     2     4     6 K KxTimes
     2     4     6 K22 %                    1E-006  *  @                    KYParam@% 
     2     4     6 K33  1.000000000000E-007 
     2     5     6 K KxTimes
     2     5     6 K22 %                    1E-006  *  @                    KYParam@% 
     2     5     6 K33  1.000000000000E-007 
     2     6     6 K KxTimes
     2     6     6 K22 %                    1E-006  *  @                    KYParam@% 
     2     6     6 K33  1.000000000000E-007 
     2     7     6 K KxTimes
     2     7     6 K22 %                    1E-006  *  @                    KYParam@% 
     2     7     6 K33  1.000000000000E-007 
     2     8     6 K KxTimes
     2     8     6 K22 %                    1E-006  *  @                    KYParam@% 
     2     8     6 K33  1.000000000000E-007 
     2     9     6 K KxTimes
     2     9     6 K22 %                    1E-006  *  @                    KYParam@% 
     2     9     6 K33  1.000000000000E-007 
     2     2     7 K KxTimes
     2     2     7 K22 %                    1E-006  *  @                    KYParam@% 
     2     2     7 K33  1.000000000000E-007 
     2     3     7 K KxTimes
     2     3     7 K22 %                    1E-006  *  @                    KYParam@% 
     2     3     7 K33  1.000000000000E-007 
     2     4     7 K KxTimes
     2     4     7 K22 %                    1E-006  *  @                    KYParam@% 
     2     4     7 K33  1.000000000000E-007 
     2     5     7 K KxTimes
     2     5     7 K22 %                    1E-006  *  @                    KYParam@% 
     2     5     7 K33  1.000000000000E-007 
     2     6     7 K KxTimes
     2     6     7 K22 %                    1E-006  *  @                    KYParam@% 
     2     6     7 K33  1.000000000000E-007 
     2     7     7 K KxTimes
     2     7     7 K22 %                    1E-006  *  @                    KYParam@% 
     2     7     7 K33  1.000000000000E-007 
     2     8     7 K KxTimes
     2     8     7 K22 %                    1E-006  *  @                    KYParam@% 
     2     8     7 K33  1.000000000000E-007 
     2     9     7 K KxTimes
     2     9     7 K22 %                    1E-006  *  @                    KYParam@% 
     2     9     7 K33  1.000000000000E-007 
     2     2     8 K KxTimes
     2     2     8 K22 %                    1E-006  *  @                    KYParam@% 
     2     2     8 K33  1.000000000000E-007 
     2     3     8 K KxTimes
     2     3     8 K22 %                    1E-006  *  @                    KYParam@% 
     2     3     8 K33  1.000000000000E-007 
     2     4     8 K KxTimes
     2     4     8 K22 %                    1E-006  *  @                    KYParam@% 
     2     4     8 K33  1.000000000000E-007 
     2     5     8 K KxTimes
     2     5     8 K22 %                    1E-006  *  @                    KYParam@% 
     2     5     8 K33  1.000000000000E-007 
     2     6     8 K KxTimes
     2     6     8 K22 %                    1E-006  *  @                    KYParam@% 
     2     6     8 K33  1.000000000000E-007 
     2     7     8 K KxTimes
     2     7     8 K22 %                    1E-006  *  @                    KYParam@% 
     2     7     8 K33  1.000000000000E-007 
     2     8     8 K KxTimes
     2     8     8 K22 %                    1E-006  *  @                    KYParam@% 
     2     8     8 K33  1.000000000000E-007 
     2     9     8 K KxTimes
     2     9     8 K22 %                    1E-006  *  @                    KYParam@% 
     2     9     8 K33  1.000000000000E-007 
     2     2     9 K KxTimes
     2     2     9 K22 %                    1E-006  *  @                    KYParam@% 
     2     2     9 K33  1.000000000000E-007 
     2     3     9 K KxTimes
     2     3     9 K22 %                    1E-006  *  @                    KYParam@% 
     2     3     9 K33  1.000000000000E-007 
     2     4     9 K KxTimes
     2     4     9 K22 %                    1E-006  *  @                    KYParam@% 
     2     4     9 K33  1.000000000000E-007 
     2     5     9 K KxTimes
     2     5     9 K22 %                    1E-006  *  @                    KYParam@% 
     2     5     9 K33  1.000000000000E-007 
     2     6     9 K KxTimes
     2     6     9 K22 %                    1E-006  *  @                    KYParam@% 
     2     6     9 K33  1.000000000000E-007 
     2     7     9 K KxTimes
     2     7     9 K22 %                    1E-006  *  @                    KYParam@% 
     2     7     9 K33  1.000000000000E-007 
     2     8     9 K KxTimes
     2     8     9 K22 %                    1E-006  *  @                    KYParam@% 
     2     8     9 K33  1.000000000000E-007 
     2     9     9 K KxTimes
     2     9     9 K22 %                    1E-006  *  @                    KYParam@% 
     2     9     9 K33  1.000000000000E-007 
     3     2     2 K KxTimes
     3     2     2 K22 %                    1E-006  *  @                    KYParam@% 
     3     2     2 K33  1.000000000000E-007 
     3     3     2 K KxTimes
     3     3     2 K22 %                    1E-006  *  @                    KYParam@% 
     3     3     2 K33  1.000000000000E-007 
     3     4     2 K KxTimes
     3     4     2 K22 %                    1E-006  *  @                    KYParam@% 
     3     4     2 K33  1.000000000000E-007 
     3     5     2 K KxTimes
     3     5     2 K22 %                    1E-006  *  @                    KYParam@% 
     3     5     2 K33  1.000000000000E-007 
     3     6     2 K KxTimes
     3     6     2 K22 %                    1E-006  *  @                    KYParam@% 
     3     6     2 K33  1.000000000000E-007 
     3     7     2 K KxTimes
     3     7     2 K22 %                    1E-006  *  @                    KYParam@% 
     3     7     2 K33  1.000000000000E-007 
     3     8     2 K KxTimes
     3     8     2 K22 %                    1E-006  *  @                    KYParam@% 
     3     8     2 K33  1.000000000000E-007 
     3     9     2 K KxTimes
     3     9     2 K22 %                    1E-006  *  @                    KYParam@% 
     3     9     2 K33  1.000000000000E-007 
     3     2     3 K KxTimes
     3     2     3 K22 %                    1E-006  *  @                    KYParam@% 
     3     2     3 K33  1.000000000000E-007 
     3     3     3 K KxTimes
     3     3     3 K22 %                    1E-006  *  @                    KYParam@% 
     3     3     3 K33  1.000000000000E-007 
     3     4     3 K KxTimes
     3     4     3 K22 %                    1E-006  *  @                    KYParam@% 
     3     4     3 K33  1.000000000000E-007 
     3     5     3 K KxTimes
     3     5     3 K22 %                    1E-006  *  @                    KYParam@% 
     3     5     3 K33  1.000000000000E-007 
     3     6     3 K KxTimes
     3     6     3 K22 %                    1E-006  *  @                    KYParam@% 
     3     6     3 K33  1.000000000000E-007 
     3     7     3 K KxTimes
     3     7     3 K22 %                    1E-006  *  @                    KYParam@% 
     3     7     3 K33  1.000000000000E-007 
     3     8     3 K KxTimes
     3     8     3 K22 %                    1E-006  *  @                    KYParam@% 
     3     8     3 K33  1.000000000000E-007 
     3     9     3 K KxTimes
     3     9     3 K22 %                    1E-006  *  @                    KYParam@% 
     3     9     3 K33  1.000000000000E-007 
     3     2     4 K KxTimes
     3     2     4 K22 %                    1E-006  *  @                    KYParam@% 
     3     2     4 K33  1.000000000000E-007 
     3     3     4 K KxTimes
     3     3     4 K22 %                    1E-006  *  @                    KYParam@% 
     3     3     4 K33  1.000000000000E-007 
     3     4     4 K KxTimes
     3     4     4 K22 %                    1E-006  *  @                    KYParam@% 
     3     4     4 K33  1.000000000000E-007 
     3     5     4 K KxTimes
     3     5     4 K22 %                    1E-006  *  @                    KYParam@% 
     3     5     4 K33  1.000000000000E-007 
     3     6     4 K KxTimes
     3     6     4 K22 %                    1E-006  *  @                    KYParam@% 
     3     6     4 K33  1.000000000000E-007 
     3     7     4 K KxTimes
     3     7     4 K22 %                    1E-006  *  @                    KYParam@% 
     3     7     4 K33  1.000000000000E-007 
     3     8     4 K KxTimes
     3     8     4 K22 %                    1E-006  *  @                    KYParam@% 
     3     8     4 K33  1.000000000000E-007 
     3     9     4 K KxTimes
     3     9     4 K22 %                    1E-006  *  @                    KYParam@% 
     3     9     4 K33  1.000000000000E-007 
     3     2     5 K KxTimes
     3     2     5 K22 %                    1E-006  *  @                    KYParam@% 
     3     2     5 K33  1.000000000000E-007 
     3     3     5 K KxTimes
     3     3     5 K22 %                    1E-006  *  @                    KYParam@% 
     3     3     5 K33  1.000000000000E-007 
     3     4     5 K KxTimes
     3     4     5 K22 %                    1E-006  *  @                    KYParam@% 
     3     4     5 K33  1.000000000000E-007 
     3     5     5 K KxTimes
     3     5     5 K22 %                    1E-006  *  @                    KYParam@% 
     3     5     5 K33  1.000000000000E-007 
     3     6     5 K KxTimes
     3     6     5 K22 %                    1E-006  *  @                    KYParam@% 
     3     6     5 K33  1.000000000000E-007 
     3     7     5 K KxTimes
     3     7     5 K22 %                    1E-006  *  @                    KYParam@% 
     3     7     5 K33  1.000000000000E-007 
     3     8     5 K KxTimes
     3     8     5 K22 %                    1E-006  *  @                    KYParam@% 
     3     8     5 K33  1.000000000000E-007 
     3     9     5 K KxTimes
     3     9     5 K22 %                    1E-006  *  @                    KYParam@% 
     3     9     5 K33  1.000000000000E-007 
     3     2     6 K KxTimes
     3     2     6 K22 %                    1E-006  *  @                    KYParam@% 
     3     2     6 K33  1.000000000000E-007 
     3     3     6 K KxTimes
     3     3     6 K22 %                    1E-006  *  @                    KYParam@% 
     3     3     6 K33  1.000000000000E-007 
     3     4     6 K KxTimes
     3     4     6 K22 %                    1E-006  *  @                    KYParam@% 
     3     4     6 K33  1.000000000000E-007 
     3     5     6 K KxTimes
     3     5     6 K22 %                    1E-006  *  @                    KYParam@% 
     3     5     6 K33  1.000000000000E-007 
     3     6     6 K KxTimes
     3     6     6 K22 %                    1E-006  *  @                    KYParam@% 
     3     6     6 K33  1.000000000000E-007 
     3     7     6 K KxTimes
     3     7     6 K22 %                    1E-006  *  @                    KYParam@% 
     3     7     6 K33  1.000000000000E-007 
     3     8     6 K KxTimes
     3     8     6 K22 %                    1E-006  *  @                    KYParam@% 
     3     8     6 K33  1.000000000000E-007 
     3     9     6 K KxTimes
     3     9     6 K22 %                    1E-006  *  @                    KYParam@% 
     3     9     6 K33  1.000000000000E-007 
     3     2     7 K KxTimes
     3     2     7 K22 %                    1E-006  *  @                    KYParam@% 
     3     2     7 K33  1.000000000000E-007 
     3     3     7 K KxTimes
     3     3     7 K22 %                    1E-006  *  @                    KYParam@% 
     3     3     7 K33  1.000000000000E-007 
     3     4     7 K KxTimes
     3     4     7 K22 %                    1E-006  *  @                    KYParam@% 
     3     4     7 K33  1.000000000000E-007 
     3     5     7 K KxTimes
     3     5     7 K22 %                    1E-006  *  @                    KYParam@% 
     3     5     7 K33  1.000000000000E-007 
     3     6     7 K KxTimes
     3     6     7 K22 %                    1E-006  *  @                    KYParam@% 
     3     6     7 K33  1.000000000000E-007 
     3     7     7 K KxTimes
     3     7     7 K22 %                    1E-006  *  @                    KYParam@% 
     3     7     7 K33  1.000000000000E-007 
     3     8     7 K KxTimes
     3     8     7 K22 %                    1E-006  *  @                    KYParam@% 
     3     8     7 K33  1.000000000000E-007 
     3     9     7 K KxTimes
     3     9     7 K22 %                    1E-006  *  @                    KYParam@% 
     3     9     7 K33  1.000000000000E-007 
     3     2     8 K KxTimes
     3     2     8 K22 %                    1E-006  *  @                    KYParam@% 
     3     2     8 K33  1.000000000000E-007 
     3     3     8 K KxTimes
     3     3     8 K22 %                    1E-006  *  @                    KYParam@% 
     3     3     8 K33  1.000000000000E-007 
     3     4     8 K KxTimes
     3     4     8 K22 %                    1E-006  *  @                    KYParam@% 
     3     4     8 K33  1.000000000000E-007 
     3     5     8 K KxTimes
     3     5     8 K22 %                    1E-006  *  @                    KYParam@% 
     3     5     8 K33  1.000000000000E-007 
     3     6     8 K KxTimes
     3     6     8 K22 %                    1E-006  *  @                    KYParam@% 
     3     6     8 K33  1.000000000000E-007 
     3     7     8 K KxTimes
     3     7     8 K22 %                    1E-006  *  @                    KYParam@% 
     3     7     8 K33  1.000000000000E-007 
     3     8     8 K KxTimes
     3     8     8 K22 %                    1E-006  *  @                    KYParam@% 
     3     8     8 K33  1.000000000000E-007 
     3     9     8 K KxTimes
     3     9     8 K22 %                    1E-006  *  @                    KYParam@% 
     3     9     8 K33  1.000000000000E-007 
     3     2     9 K KxTimes
     3     2     9 K22 %                    1E-006  *  @                    KYParam@% 
     3     2     9 K33  1.000000000000E-007 
     3     3     9 K KxTimes
     3     3     9 K22 %                    1E-006  *  @                    KYParam@% 
     3     3     9 K33  1.000000000000E-007 
     3     4     9 K KxTimes
     3     4     9 K22 %                    1E-006  *  @                    KYParam@% 
     3     4     9 K33  1.000000000000E-007 
     3     5     9 K KxTimes
     3     5     9 K22 %                    1E-006  *  @                    KYParam@% 
     3     5     9 K33  1.000000000000E-007 
     3     6     9 K KxTimes
     3     6     9 K22 %                    1E-006  *  @                    KYParam@% 
     3     6     9 K33  1.000000000000E-007 
     3     7     9 K KxTimes
     3     7     9 K22 %                    1E-006  *  @                    KYParam@% 
     3     7     9 K33  1.000000000000E-007 
     3     8     9 K KxTimes
     3     8     9 K22 %                    1E-006  *  @                    KYParam@% 
     3     8     9 K33  1.000000000000E-007 
     3     9     9 K KxTimes
     3     9     9 K22 %                    1E-006  *  @                    KYParam@% 
     3     9     9 K33  1.000000000000E-007 
END PERIOD 

