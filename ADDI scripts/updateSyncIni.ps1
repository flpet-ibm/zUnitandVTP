param ([Parameter(Mandatory)]$Project, [Parameter(Mandatory)]$GitFolder, $SyncFile='C:\IBM AD\Mainframe Sources\sync.ini', [switch]$Write)

$isGit = ($Gitfolder + "\.git")
if (-not (Test-Path $isGit -PathType Container)) {
	if (Test-Path $Gitfolder -PathType Container) {
     	echo ("OK:    " + $Gitfolder + " does exist as a folder")
	} else {
     	echo ("ERROR: " + $Gitfolder + " does NOT exist as a folder")
	}
	if (Test-Path $isGit -PathType Container) {
     	echo ("OK:    " + $isGit + " does exist as a folder")
	} else {
     	echo ("ERROR: " + $isGit + " does NOT exist as a folder")
	}
	echo "Abort"
	exit 8
}

if (-not (Test-Path $SyncFile -PathType leaf)) {
	  echo ($SyncFile + " is not found. It must be the sync.ini file configured in the Configureation Service.")
	  echo "add -SyncFile 'name of sync file' to the command"
	  exit 8
}

function Add-Folder {
	  param (
                $searchExt,
		$FileType,
		$ADDIFolder
                $GitSubFolder=''
                )
        $fullGit = $GitFolder + '\' + $GitSubFolder
 	$n = Get-ChildItem -Path $fullGit -Recurse -File -Include $searchExt

	$lastDir = ""
	foreach($file in $n){
		If ($file.DirectoryName -ne $Lastdir) {
		   $newP.Add([PSCustomObject]@{
				Project = $Project
				SyncType = 'LOCAL_REMOTE'
				Path = $file.DirectoryName
				FileType = $FileType
				ADDIFolder = $ADDIFolder
				Filter = 'filter(' + $searchExt + ')'}) | Out-Null
		   $lastDir = $file.DirectoryName
		}
	}	
}


$l = $GitFolder.Length + 1

$Header = 'Project', 'SyncType', 'Path', 'FileType', 'ADDIFolder', 'Filter'
$P = Import-Csv -Path $SyncFile -Delimiter ',' -Header $Header
$newP = [System.Collections.ArrayList]$P.Where({$_.Project -ne $Project})

Add-Folder -searchExt '*.bms'     -FileType 'BMS'           -ADDIFolder 'BMS'
Add-Folder -searchExt '*.dbd'     -FileType 'DBD'           -ADDIFolder 'DBD'
Add-Folder -searchExt '*.psb'     -FileType 'PSB'           -ADDIFolder 'PCB'
Add-Folder -searchExt '*.mfs'     -FileType 'IMS Map'       -ADDIFolder 'MFS'
Add-Folder -searchExt '*.jcl'     -FileType 'JCL'           -ADDIFolder 'JCL'
Add-Folder -searchExt '*.jclproc' -FileType 'JCL Procs'     -ADDIFolder 'JCL_PROCLIB'
Add-Folder -searchExt 'ims*.txt'  -FileType 'Configuration' -ADDIFolder 'IMST_PGM'
Add-Folder -searchExt '*.cpy'     -FileType 'COBOL Include' -ADDIFolder 'COPY'
Add-Folder -searchExt '*.cbl'     -FileType 'zOS Cobol'     -ADDIFolder 'COBOL_MVS'     -GitSubFolder 'cobol'
Add-Folder -searchExt '*.pli'     -FileType 'PL1'           -ADDIFolder 'PL1'
Add-Folder -searchExt '*.inc'     -FileType 'PL1 Include'   -ADDIFolder 'PL1_INCLUDE'

if ($Write) {
	Remove-Item -Path $SyncFile
	$newP | ForEach-Object -Process {
	   $out = $_.Project + "," + $_.SyncType + "," + $_.Path + "," + $_.FileType + "," + $_.ADDIFolder
	   if ($_.Filter.Length -gt 1) {
		  $out = $out + "," + $_.Filter
	   }
	   $out | Out-File -FilePath $SyncFile -Append -Encoding ascii
	}
    $newP | Format-Table
	echo ($SyncFile + " has been updated.")
	echo ""
} else {
    $newP | Format-Table
	echo ($SyncFile +" has not been updated. Add -Write to commandline to write to it")
	echo ""
}


