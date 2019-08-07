@echo off

REM   Make a MODFLOW run.
REM   Run DRY and GAGEINTERP.
REM   Run J_OSOM to generate a .om file

echo.

Echo Run_tr.bat is running MODFLOW-2000...

start "MODFLOW model run" /min /low /wait mf2k-db-pcgn mod16_spheads_tsflux_ssfix.nam

Echo Run_tr.bat is running DRY.EXE...

dry mod16_spheads_tsflux_ssfix.lst

Echo Run_tr.bat is running GAGEINTERP...

gageinterp

REM   Run J_OSOM to convert ._os file produced by MODFLOW-2000
REM   to a ._om file with no omitted simulated equivalents.

Echo Run_tr.bat is running J_OSOM...
j_osom mod16trosom

echo.
echo Run_tr.bat is done.

