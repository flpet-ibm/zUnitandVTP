# zUnitandVTP
This sample code can be use to demo zUnit for IMS.

Also this is a useful example of IMS support in ADDI.

This also contains a sample on how to pull data from Git to ADDI, Specifically a way to automatically maintain sync.ini with the folders from Git.

The installation is a bit of a catch-22, as the scripts will clone the Git repo to your ADDI Server, but the script do the close is in the repo. So how to get the script to the server? You can download the whole repo by using "Download as Zip", and from there extract the ADDI scripts/build_Flemmings_Samples.bat and run that. 

To add to ADDI :
- Create a project in ADDI
- Copy https://github.com/flpet-ibm/zUnitandVTP/blob/main/ADDI%20scripts/build_Flemmings_Samples.bat to the server. 
- Edit the bat file :
  - Line 2: Change to the foldername where you want the Git repo cloned to on your ADDI Server. Last part of the path must be the project name in Github.
  - Line 3: The url to the Git repo in Github. If you do not intend to change anything in the project, you can just keep my repo name.
  - Line 4: The name of the ADDI projet you have created
  - Line 5: The installationroot folder for your ADDI project folders mainframe projects.
  - Line 6: The installation path to ADDI. It is already set to the default installation path.
  - Edit the line which calls updateSyncIni.ps1, if your installation is not in the default folders, then the sync.ini file might not be located in C:\IBM AD\Mainframe Sources. If not, add the path to the SyncFile to the powershell command `-SyncFile 'C:\IBM AD\Mainframe Sources\sync.ini'`.
- Now run the batch script. If you are running the Build Client as Administrator, you should open a command prompt as Administator and run the command from there.
- It will
  - Clone/pull the Git repo
  - synchronize the files intot he ADDI project
  - Copy the PSBmap.txt to the root of the project folder
  - Run a Make operation in ADDI.
- Add the folder where the Git repos are cloned to the File Service in ADDI with the  Admin web UI (https://localhost:9443/ad/admin/configure?tab=environments). In my sample I needed to add C:\IBM AD\Git.
- Rerun the script every time you have commited new changes to the Git Repo in Github.
