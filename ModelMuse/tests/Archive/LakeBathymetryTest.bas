# Hypothetical simulator of lake and surficial aquifer interaction. ------------
# Shows convergence to equilibrium in transient mode.
# Basic Package file created on 6/8/2020 by ModelMuse version 4.2.0.17.
# Number of active cells = 1411.
FREE  # OPTIONS
INTERNAL 1 (FREE)        5 # IBOUND Layer 1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     0     0     0     0
     0     1     1     1     1     1     1
     1     1     1     1     1     1     0     0     0     0
     0     1     1     1     1     1     1
     1     1     1     1     1     1     0     0     0     0
     0     1     1     1     1     1     1
     1     1     1     1     1     1     0     0     0     0
     0     1     1     1     1     1     1
     1     1     1     1     1     1     0     0     0     0
     0     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
INTERNAL 1 (FREE)        5 # IBOUND Layer 2
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     0     0     0
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     0     0     0
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     0     0     0
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
CONSTANT        1 # IBOUND Layer 3
CONSTANT        1 # IBOUND Layer 4
CONSTANT        1 # IBOUND Layer 5
 -9.990000000000E+002  # HNOFLO
CONSTANT     1.150000000000E+002  # STRT Layer 1
CONSTANT     1.150000000000E+002  # STRT Layer 2
CONSTANT     1.150000000000E+002  # STRT Layer 3
CONSTANT     1.150000000000E+002  # STRT Layer 4
CONSTANT     1.150000000000E+002  # STRT Layer 5
