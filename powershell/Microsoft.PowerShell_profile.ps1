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
    Import-Module ~\Documents\WindowsPowershell\Out-DefaultWrapper.ps1;

    # Common Functions
    Import-Module ~\Documents\WindowsPowershell\Functions.ps1;

    # Posh-git
    Import-Module posh-git;
}

# Set Up alias

Set-Alias dirs Find-File;
Set-Alias grep Find-String;
Set-Alias highlight Highlight-String;
Set-Alias hi Highlight-String;
Set-Alias glr Get-LastResult;
Set-Alias glc Get-LastCommand;

# Set-up Prompt

function prompt {

    $realLastExitCode = $LastExitCode

    if (Get-Module Posh-Git) {

        #        $Host.UI.RawUI.ForegroundColor = $GitPromptSettings.DefaultForegroundColor;
        Write-Host $pwd.ProviderPath -NoNewLine;
        Write-VcsStatus;
        Write-Host;
        #        Enable-GitColors

    } else {

        $Host.UI.RawUI.ForegroundColor = 'Cyan'
        Write-Host $pwd.ProviderPath;

    }

    if ($global:IsCurrentUserAdministrator) {

        $Host.UI.RawUI.ForegroundColor = 'Red'
        Write-Host '***Administrator*** $' -NoNewLine

    } else {

        $Host.UI.RawUI.ForegroundColor = 'Yellow'
        Write-Host '>' -NoNewline

    }

    $Global:LastExitCode = $realLastExitCode

    $out = ' ';

    # Check for ConEmu existance and ANSI emulation enabled
    if ($env:ConEmuANSI -eq "ON") {
        # Let ConEmu know when the prompt ends, to select typed
        # command properly with "Shift+Home", to change cursor
        # position in the prompt by simple mouse click, etc.

        $out += "$([char]27)]9;12$([char]7)"

        # And current working directory (FileSystem)
        # ConEmu may show full path or just current folder name
        # in the Tab label (check Tab templates)
        # Also this knowledge is crucial to process hyperlinks clicks
        # on files in the output from compilers and source control
        # systems (git, hg, ...)
        if ($pwd.Provider.Name -eq "FileSystem") {
            $out += "$([char]27)]9;9;`"$($pwd.Path)`"$([char]7)"
        }
    }

    return $out;
}

function Test-IsAdmin {
    $currentUser = [Security.Principal.WindowsIdentity]::GetCurrent();
    $principal = new-object Security.Principal.WindowsPrincipal($currentUser);
    $principal.IsInRole("Administrators");
}

$Global:IsCurrentUserAdministrator = Test-IsAdmin

# Chocolatey profile
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}
