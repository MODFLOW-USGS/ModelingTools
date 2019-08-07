@echo off
REM
REM   Run FAC2REAL multiple times to generate array files to be read
REM   as OPEN/CLOSE files by Modflow-2000
REM

REM   Generate files of HK values for layers 6, 8, 10, and 12
REM   These commands make files K_06.txt, K_08.txt, ... K_12.txt
REM

REM  Echo FAC2R.BAT is generating HK arrays using FAC2REAL...
REM  fac2real < fac2r_02.rsp
REM  fac2real < fac2r_04.rsp

echo.
Echo FAC2R running "fac2real < fac2r_06.rsp" to generate K_sd_tkd.txt
fac2real < fac2r_06.rsp

echo.
Echo FAC2R running "fac2real < fac2r_08.rsp" to generate K_sd_uka.txt
fac2real < fac2r_08.rsp

echo.
Echo FAC2R running "fac2real < fac2r_10.rsp" to generate K_sd_lka.txt
fac2real < fac2r_10.rsp

echo.
Echo FAC2R running "fac2real < fac2r_12.rsp" to generate K_sd_klf.txt
fac2real < fac2r_12.rsp

