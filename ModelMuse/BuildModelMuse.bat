rem Set the environment options using the Community Edition of Delphi version 10.4
call "C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat"
rem Build ModelMuse with current settings.
MsBuild.exe /t:Build ModelMuse.dproj >Messages.txt
start Messages.txt
pause