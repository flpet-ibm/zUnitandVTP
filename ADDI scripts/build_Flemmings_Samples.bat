@echo off
set GITFOLDER=C:\IBM AD\Git\zUnitandVTP
set GITURL=git@github.com:flpet-ibm/zUnitandVTP.git
set ADDIPROJ=Flemmings_Samples
set ADDIROOT=C:\IBM AD\Mainframe Projects
set ADDIINST=C:\Program Files\IBM Application Discovery and Delivery Intelligence
set currdir=%~dp0
pushd "%~dp0"
IF EXIST "%GITFOLDER%" (
   cd /d "%GITFOLDER%"
   echo Pulling Git repo into %GITFOLDER%
   git pull
) ELSE (
   echo Creating folder for  repo at %GITFOLDER%
   mkdir "%GITFOLDER%"
   cd /d "%GITFOLDER%"
   cd ..
   echo Cloning Git repo from %GITURL%
   git clone %GITURL%
)
popd
echo Copying PSB Mapping file to %ADDIROOT%\%ADDIPROJ%
copy "%GITFOLDER%\ADDI Config\PSBmap.txt" "%ADDIROOT%\%ADDIPROJ%"
set BCCMD="%ADDIINST%\IBM Application Discovery Build Client\Bin\Release\IBMApplicationDiscoveryBuildClient"
echo Updateing sync.ini
powershell "& '%currdir%\updateSyncIni.ps1' Flemmings_Samples 'C:\IBM AD\Git\zUnitandVTP\' -Write"
echo Synchronzing ADDI project %ADDIPROJ%
%BCCMD%  /umm1 %ADDIPROJ%
echo Performing Make for ADDI project %ADDIPROJ%
%BCCMD%  /m1 %ADDIPROJ% /m2 y /m3 y
