echo on
set GITFOLDER=C:\IBM AD\Git\zUnitandVTP
set GITURL=git@github.com:flpet-ibm/zUnitandVTP.git
set ADDIPROJ=Flemmings_Samples
set ADDIINST="C:\Program Files\IBM Application Discovery and Delivery Intelligence"
pushd %1
IF EXIST "%GITFOLDER%" (
   cd /d "%GITFOLDER%
   git pull
) ELSE (
   mkdir "%GITFOLDER%"
   cd /d "%GITFOLDER%
   cd ..
   git clone %GITURL%
)i
popd
copy "%GITFOLDER%\ADDI Config\PSBmap.txt" "C:\IBM AD\Mainframe Projects\%ADDIPROJ%"
set BCCMD="%ADDIINST%\IBM Application Discovery Build Client\Bin\Release\IBMApplicationDiscoveryBuildClient"
%BCCMD%  /umm1 %ADDIPROJ%
%BCCMD%  /m1 %ADDIPROJ% /m2 y /m3 y