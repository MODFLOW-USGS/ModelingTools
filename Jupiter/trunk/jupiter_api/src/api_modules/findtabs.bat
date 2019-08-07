@echo off
REM Program findtabs.bat to find tab
REM characters in all *.f files in a directory
REM gawk "/\t/ {print FILENAME FS FNR FS $0}" *.f
gawk "/\t/ {print FILENAME FS FNR FS $0}" *.f90
REM gawk "/\t/ {print FILENAME FS FNR FS $0}" *.inc
REM gawk "/\t/ {print FILENAME FS FNR FS $0}" *.com
