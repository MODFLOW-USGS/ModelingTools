@echo off
REM This is tc1-sen-script.bat
REM ECHO.
REM ECHO tc1-sen-script.bat invoked
del tc1._*
del tc1sen._*
D:\Projects\Jupiter\ForRelease\jupiter_api\bin\mf2k tc1sen.nam
copy tc1sen._os tc1._os
copy tc1sen._su tc1._su
