@echo off
REM This is tc1-fwd-script.bat
REM ECHO.
REM ECHO tc1-fwd-script.bat invoked
del tc1._*
del tc1fwd._*
..\..\bin\mf2k tc1fwd.nam
copy tc1fwd._os tc1._os
