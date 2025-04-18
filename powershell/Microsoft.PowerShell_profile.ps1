$env:RIPGREP_CONFIG_PATH = "$($env:USERPROFILE)\.ripgreprc";
$Script:ProfileInitialized = $false;

function Initialize-Profile {
    Param(
        [Switch]$Force
    )
    PROCESS
    {
        if (-not $Script:ProfileInitialized -or $Force) {
            $Script:ProfileInitialized = $true;
            . Import-Scripts;
            Initialize-OhMyPosh;
            . Initialize-Aliases;
            . Import-CustomScripts;
        }
    }
}

function init {
    Initialize-Profile -Force;
}

function Import-Scripts {
    if ($host.Name -eq 'ConsoleHost')
    {
        Set-PSReadlineOption -EditMode Emacs;

        # Out-default wrapper.
        . "$PSScriptRoot/Out-DefaultWrapper.ps1";

        # Common Utilities
        . "$PSScriptRoot/utilities.ps1";

        # Common Functions (specific to personal)
        . "$PSScriptRoot/functions.ps1";

        # Filter Interactive
        Import-Module "$PSScriptRoot/fi.psm1";
    }
}

function Initialize-OhMyPosh {
    Import-Module 'oh-my-posh' -MaximumVersion '2.1';
    if (-not (Test-Path "$($ThemeSettings.MyThemesLocation)\Paradox2.psm1")) {
        if (-not (Test-Path $ThemeSettings.MyThemesLocation)) {
            if ($env:OS -eq 'Windows_NT') {
                mkdir $ThemeSettings.MyThemesLocation;
            } else {
                $path = $ThemeSettings.MyThemesLocation -replace '\\','/';
                $path = $path -replace '~',$env:HOME;
                mkdir -p $path;
            }
        }
        copy "$($PSScriptRoot)\Paradox2.psm1" "$($ThemeSettings.MyThemesLocation)\Paradox2.psm1";
    }
    Set-Theme 'Paradox2';
}

function Initialize-Aliases {
    Set-Alias dirs Find-File;
    Set-Alias grep Find-String;
    Set-Alias highlight Write-HighlightString;
    Set-Alias hi Write-HighlightString;
    Set-Alias glr Get-LastResult;
    Set-Alias glc Get-LastCommand;
    Set-Alias sin Select-Interactive;
}

function Import-CustomScripts {
    if ($host.Name -eq 'ConsoleHost')
    {
        if (Test-Path "$($env:USERPROFILE)\Documents\WindowsPowerShell\custom.ps1")
        {
            . "$($env:USERPROFILE)\Documents\WindowsPowerShell\custom.ps1";
        }
        elseif (Test-Path "$PSScriptRoot\custom.ps1")
        {
            . "$PSScriptRoot\custom.ps1";
        }
    }
}

function em {
    param($p1, $p2, $p3, $p4)
    if ($env:OS -eq 'Windows_NT') {
        emacs $p1 $p2 $p3 $p4;
    } else {
        if (Get-Process emacs) {
            emacsclientw -n --no-wait $p1 $p2 $p3 $p4;
        } else {
            emacs $p1 $p2 $p3 $p4;
        }
    }
}

function e {
    param($p1, $p2, $p3, $p4)
    if ($env:OS -eq 'Windows_NT') {
        emacs.exe -nw $p1 $p2 $p3 $p4;
    } else {
        emacs -nw $p1 $p2 $p3 $p4;
    }
}

function emacs {
    param($p1, $p2, $p3, $p4)

    if ($env:OS -eq 'Windows_NT') {
        if ($p1)
        {
            emacsclientw.exe -n --no-wait --alternate-editor="runemacs.exe" $p1 $p2 $p3 $p4;
            return;
        }

        $emacs = Get-Process | Where-Object {$_.Name -like "emacs"};
        if ($emacs)
        {
            Add-Type @"
using System;
using System.Runtime.InteropServices;
public class WinAp {
    [DllImport("user32.dll")]
    [return: MarshalAs(UnmanagedType.Bool)]
    public static extern bool SetForegroundWindow(IntPtr hWnd);

    [DllImport("user32.dll")]
    [return: MarshalAs(UnmanagedType.Bool)]
    public static extern bool ShowWindow(IntPtr hWnd, int nCmdShow);
}
"@
            $h = $emacs.MainWindowHandle;
            [void] [WinAp]::SetForegroundWindow($h);
        }
        else
        {
            emacsclientw.exe -n --no-wait --alternate-editor="runemacs.exe" -e '';
        }
    } else {
        & '/usr/bin/emacs' $p1 $p2 $p3 $p4;
    }
}

function Invoke-LazyLoad {
    $LazyLoadProfileRunspace = [RunspaceFactory]::CreateRunspace()
    $LazyLoadProfile = [PowerShell]::Create()
    $LazyLoadProfile.Runspace = $LazyLoadProfileRunspace
    $LazyLoadProfileRunspace.Open()
    [void]$LazyLoadProfile.AddScript({. Initialize-Profile -Force;}) # (1)
    [void]$LazyLoadProfile.BeginInvoke()
    $null = Register-ObjectEvent -InputObject $LazyLoadProfile -EventName InvocationStateChanged -Action {
        . Initialize-Profile -Force;
        $global:GitPromptSettings.DefaultPromptPrefix.Text = 'PS '
        $global:GitPromptSettings.DefaultPromptBeforeSuffix.Text = '`n'
        $LazyLoadProfile.Dispose()
        $LazyLoadProfileRunspace.Close()
        $LazyLoadProfileRunspace.Dispose()
    }
}

if ($env:OS -eq 'Windows_NT') {
    $DefaultUser = $env:USERNAME;
#    . Invoke-LazyLoad;
    . Initialize-Profile;
} else {
    $DefaultUser = $env:USER;
    . Initialize-Profile;
}

# Import the Chocolatey Profile that contains the necessary code to enable
# tab-completions to function for `choco`.
# Be aware that if you are missing these lines from your profile, tab completion
# for `choco` will not function.
# See https://ch0.co/tab-completion for details.
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}
