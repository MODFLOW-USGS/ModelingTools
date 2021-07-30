ptf @
etf %
# MNW2: Multi-Node Well package file created on 7/30/2021 by ModelMuse version 4.3.0.56.
# (and then modified by a parameter estimation program.)
    -5    13     9     2 AUXILIARY IFACE # DataSet 1: MNWMAX, NODTOT, IWL2CB, MNWPRNT, OPTION
MyWell     -1 # Data Set 2A: WELLID, NNODES
THIEM      0     0     0     0 # Data Set 2B: LOSSTYPE, PUMPLOC, Qlimit, PPFLAG, PUMPCAP
 -1.000000000000E+000  # Data Set 2C; Rw
  0.000000000000E+000  -3.000000000000E+001      7     7 %                    1 *  @                    MnRw@%  # Data Set 2D; Ztop, Zbotm, ROW, COL, Rw
Well2     -2 # Data Set 2A: WELLID, NNODES
SKIN      0     0     0     0 # Data Set 2B: LOSSTYPE, PUMPLOC, Qlimit, PPFLAG, PUMPCAP
 -1.000000000000E+000  -1.000000000000E+000  -1.000000000000E+000  # Data Set 2C; Rw, Rskin, Kskin
  0.000000000000E+000  -5.000000000000E+000      8    17  4.000000000000E-001   1.000000000000E+000   1.000000000000E-003  # Data Set 2D; Ztop, Zbotm, ROW, COL, Rw, Rskin, Kskin
 -1.500000000000E+001  -2.500000000000E+001      8    17  4.000000000000E-001   8.000000000000E-001   1.000000000000E-003  # Data Set 2D; Ztop, Zbotm, ROW, COL, Rw, Rskin, Kskin
Well3     -1 # Data Set 2A: WELLID, NNODES
GENERAL      0     0     0     0 # Data Set 2B: LOSSTYPE, PUMPLOC, Qlimit, PPFLAG, PUMPCAP
 %                    1 *  @                    MnRw@%   1.000000000000E-001   1.000000000000E-001   2.000000000000E+000  # Data Set 2C; Rw, B, C, P
  0.000000000000E+000  -2.000000000000E+001      3    13 # Data Set 2D; Ztop, Zbotm, ROW, COL
well4      1 # Data Set 2A: WELLID, NNODES
NONE      0     0     1     0 # Data Set 2B: LOSSTYPE, PUMPLOC, Qlimit, PPFLAG, PUMPCAP
     1     5    19 %                    1 *  @                    MnwPP@%  # Data Set 2D; LAY, ROW, COL, PP
well5     -1 # Data Set 2A: WELLID, NNODES
SPECIFYcwc      0     0     0     0 # Data Set 2B: LOSSTYPE, PUMPLOC, Qlimit, PPFLAG, PUMPCAP
  1.000000000000E-003  # Data Set 2C; CWC
  0.000000000000E+000  -1.900000000000E+001      3    24 # Data Set 2D; Ztop, Zbotm, ROW, COL
     5 # Data Set 3, Stress Period 1: ITMP
MyWell  %                    (1 *  @                    MnPump2@) *  @                    MnPump@%      0 # Data Set 4A: WELLID, QDes, IFACE
Well2  -3.000000000000E-003      0 # Data Set 4A: WELLID, QDes, IFACE
Well3  -3.000000000000E-003      0 # Data Set 4A: WELLID, QDes, IFACE
well4  -1.000000000000E-004      0 # Data Set 4A: WELLID, QDes, IFACE
well5  -3.000000000000E-004      0 # Data Set 4A: WELLID, QDes, IFACE
     4 # Data Set 3, Stress Period 2: ITMP
Well2  -3.000000000000E-003      0 # Data Set 4A: WELLID, QDes, IFACE
Well3  -3.000000000000E-003      0 # Data Set 4A: WELLID, QDes, IFACE
well4  -1.000000000000E-004      0 # Data Set 4A: WELLID, QDes, IFACE
well5  -3.000000000000E-004      0 # Data Set 4A: WELLID, QDes, IFACE
     5 # Data Set 3, Stress Period 3: ITMP
MyWell  %                    -0.003  *  @                    MnPump@%      0 # Data Set 4A: WELLID, QDes, IFACE
Well2  -3.000000000000E-003      0 # Data Set 4A: WELLID, QDes, IFACE
Well3  -3.000000000000E-003      0 # Data Set 4A: WELLID, QDes, IFACE
well4  -1.000000000000E-004      0 # Data Set 4A: WELLID, QDes, IFACE
well5  -3.000000000000E-004      0 # Data Set 4A: WELLID, QDes, IFACE
