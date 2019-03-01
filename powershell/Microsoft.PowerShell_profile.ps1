$env:path += ";" + (Get-Item "Env:ProgramFiles(x86)").Value + "\Git\bin" + ';E:\tools\grep\bin\';
$env:RIPGREP_CONFIG_PATH = "$($env:USERPROFILE)\.ripgreprc";

if ($host.Name -eq 'ConsoleHost')
{
    # PSReadline
#    Import-Module PSReadline
    Set-PSReadlineOption -EditMode Emacs;

    # Posh-git
#    . 'C:\Users\kohzi\Documents\WindowsPowerShell\Modules\posh-git\profile.example.ps1';
#    Import-Module posh-git;

    # Out-default wrapper.
    Import-Module "$PSScriptRoot\Out-DefaultWrapper.ps1";

    # Common Functions
    Import-Module "$PSScriptRoot\Functions.ps1";

    # Filter Interactive
    Import-Module "$PSScriptRoot\fi.psm1";
}

if ($env:ConEmuPID)
{
    Import-Module 'oh-my-posh';
    if (-not (Test-Path "$($ThemeSettings.MyThemesLocation)\Paradox2.psm1"))
    {
        mkdir $ThemeSettings.MyThemesLocation -ErrorAction SilentlyContinue;
        copy "$($PSScriptRoot)\Paradox2.psm1" "$($ThemeSettings.MyThemesLocation)\Paradox2.psm1";
    }
    Set-Theme 'Paradox2';
    $DefaultUser = $env:USERNAME;
}

# Set Up alias

Set-Alias dirs Find-File;
Set-Alias grep Find-String;
Set-Alias highlight Highlight-String;
Set-Alias hi Highlight-String;
Set-Alias glr Get-LastResult;
Set-Alias glc Get-LastCommand;
Set-Alias sin Select-Interactive;

# Set-up Prompt

# function global:prompt {

#     $realLastExitCode = $LastExitCode

#     if (Get-Module Posh-Git) {

#         Write-Host $pwd.ProviderPath -NoNewLine;
#         Write-VcsStatus;
#         Write-Host;

#     } else {

#         $Host.UI.RawUI.ForegroundColor = 'Cyan';
#         Write-Host $pwd.ProviderPath;;

#     }

#     if ($global:IsCurrentUserAdministrator) {

#         $Host.UI.RawUI.ForegroundColor = 'Red';
#         Write-Host '***Administrator*** $' -NoNewLine;

#     } else {

#         $Host.UI.RawUI.ForegroundColor = 'Yellow';
#         Write-Host '>' -NoNewline;

#     }

#     $Global:LastExitCode = $realLastExitCode;

#     $out = ' ';

#     # Check for ConEmu existance and ANSI emulation enabled
#     if ($env:ConEmuANSI -eq "ON") {
#         # Let ConEmu know when the prompt ends, to select typed
#         # command properly with "Shift+Home", to change cursor
#         # position in the prompt by simple mouse click, etc.

#         $out += "$([char]27)]9;12$([char]7)";

#         # And current working directory (FileSystem)
#         # ConEmu may show full path or just current folder name
#         # in the Tab label (check Tab templates)
#         # Also this knowledge is crucial to process hyperlinks clicks
#         # on files in the output from compilers and source control
#         # systems (git, hg, ...)
#         if ($pwd.Provider.Name -eq "FileSystem") {
#             $out += "$([char]27)]9;9;`"$($pwd.Path)`"$([char]7)";
#         }
#         return $out;
#     }

#     return $out;
# }

function Test-IsAdmin {
    $currentUser = [Security.Principal.WindowsIdentity]::GetCurrent();
    $principal = new-object Security.Principal.WindowsPrincipal($currentUser);
    $principal.IsInRole("Administrators");
}

$Global:IsCurrentUserAdministrator = Test-IsAdmin

# Chocolatey profile
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
    Import-Module "$ChocolateyProfile";
}

function emacs {
    param($p1, $p2, $p3, $p4)
    if ($p1)
    {
        emacsclientw.exe -n --no-wait --alternate-editor="runemacs.exe" $p1 $p2 $p3 $p4;
        return;
    }

    $emacs = Get-Process | Where-Object {$_.Name -like "emacs"};
    if ($emacs)
    {
        [void] [System.Reflection.Assembly]::LoadWithPartialName("'Microsoft.VisualBasic");
        [Microsoft.VisualBasic.Interaction]::AppActivate($emacs.Id);
    }
    else
    {
        emacsclientw.exe -n --no-wait --alternate-editor="runemacs.exe" -e '';
    }
}

if (Test-Path "$($env:USERPROFILE)\Documents\WindowsPowerShell\custom.ps1")
{
    if ($host.Name -eq 'ConsoleHost')
    {
        . "$($env:USERPROFILE)\Documents\WindowsPowerShell\custom.ps1";
    }
}
