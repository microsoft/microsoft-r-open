Write-Host Taking ownership and adding control to files under $pwd

$output = takeown /d y /f $pwd /r
$acl = Get-Acl $pwd
$permission = ".\\Users","FullControl","Allow"
$accessRule = New-Object System.Security.AccessControl.FileSystemAccessRule $permission
$acl.SetAccessRule($accessRule)
$files = gci $pwd -Recurse

foreach ($file in $files)
{
    $acl | Set-Acl $file.PSPath
}