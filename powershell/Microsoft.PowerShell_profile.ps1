$env:RIPGREP_CONFIG_PATH = "$($env:HOME)/.ripgreprc";
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
    param([Parameter(ValueFromRemainingArguments)]$Arguments)
    if ($env:OS -eq 'Windows_NT') {
        emacs @Arguments;
    } else {
        if (Get-Process emacs -ErrorAction SilentlyContinue) {
            emacsclientw -n --no-wait @Arguments;
        } else {
            emacs @Arguments;
        }
    }
}

function e {
    param([Parameter(ValueFromRemainingArguments)]$Arguments)
    if ($env:OS -eq 'Windows_NT') {
        $neomacsExe = 'C:\tools\neomacs\neomacs.exe';
        if (Test-Path -LiteralPath $neomacsExe) {
            & $neomacsExe -nw @Arguments;
        } else {
            emacs.exe -nw @Arguments;
        }
    } else {
        emacs -nw @Arguments;
    }
}

function Test-EmacsServerProcess {
    param(
        [Parameter(Mandatory)]
        [string]$ServerFile,
        [Parameter(Mandatory)]
        [string]$ProcessName
    )
    if (-not (Test-Path -LiteralPath $ServerFile)) {
        return $false;
    }
    $firstLine = Get-Content -LiteralPath $ServerFile -TotalCount 1 -ErrorAction SilentlyContinue;
    if ($firstLine -match '^\S+\s+(\d+)\s*$') {
        $proc = Get-Process -Id ([int]$Matches[1]) -ErrorAction SilentlyContinue;
        return ($null -ne $proc -and $proc.Name -ieq $ProcessName);
    }
    return $false;
}

function Focus-WindowHandle {
    param(
        [Parameter(Mandatory)]
        [IntPtr]$Handle
    )
    if (-not ('WinAp' -as [type])) {
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
    }
    [void] [WinAp]::SetForegroundWindow($Handle);
}

function emacs {
    param([Parameter(ValueFromRemainingArguments)]$Arguments)

    if ($env:OS -eq 'Windows_NT') {
        $neomacsDir = 'C:\tools\neomacs';
        $neomacsExe = Join-Path $neomacsDir 'neomacs.exe';
        $neomacsClient = Join-Path $neomacsDir 'neomacsclient.exe';
        $runNeomacs = Join-Path $neomacsDir 'runneomacs.exe';
        $homeDir = if ($env:HOME) { $env:HOME } else { $env:USERPROFILE };
        $serverFile = Join-Path $homeDir '.emacs.d\server\server';

        $neomacsRunning = @(Get-Process -Name 'neomacs' -ErrorAction SilentlyContinue);
        $emacsRunning = @(Get-Process -Name 'emacs' -ErrorAction SilentlyContinue);

        if ($neomacsRunning.Count -gt 0) {
            if ($Arguments -and $Arguments.Count -gt 0) {
                if (Test-EmacsServerProcess -ServerFile $serverFile -ProcessName 'neomacs') {
                    & $neomacsClient -f $serverFile -n --alternate-editor="$runNeomacs" @Arguments;
                } else {
                    & $runNeomacs @Arguments;
                }
            } else {
                $gui = $neomacsRunning | Where-Object { $_.MainWindowHandle -ne 0 } | Select-Object -First 1;
                if ($gui) {
                    Focus-WindowHandle -Handle $gui.MainWindowHandle;
                } else {
                    & $runNeomacs;
                }
            }
            return;
        }

        if ($emacsRunning.Count -gt 0) {
            if ($Arguments -and $Arguments.Count -gt 0) {
                emacsclientw.exe -n --no-wait --alternate-editor="runemacs.exe" @Arguments;
            } else {
                $gui = $emacsRunning | Where-Object { $_.MainWindowHandle -ne 0 } | Select-Object -First 1;
                if ($gui) {
                    Focus-WindowHandle -Handle $gui.MainWindowHandle;
                } else {
                    emacsclientw.exe -c -n --no-wait --alternate-editor="runemacs.exe";
                }
            }
            return;
        }

        # Neither is running: prioritize Neomacs if installed, otherwise fall back to Emacs.
        if (Test-Path -LiteralPath $neomacsExe) {
            if ($Arguments -and $Arguments.Count -gt 0) {
                & $runNeomacs @Arguments;
            } else {
                & $runNeomacs;
            }
            return;
        }

        if ($Arguments -and $Arguments.Count -gt 0)
        {
            emacsclientw.exe -n --no-wait --alternate-editor="runemacs.exe" @Arguments;
        }
        else
        {
            emacsclientw.exe -n --no-wait --alternate-editor="runemacs.exe" -e '';
        }
    } else {
        if (Test-Path '/usr/bin/emacs') {
            . '/usr/bin/emacs' @Arguments;
        } elseif (Test-Path '/usr/local/bin/emacs') {
            if (Get-Process Emacs* -ErrorAction SilentlyContinue) {
                . '/usr/local/bin/emacsclient' @Arguments;
            } else {
                . '/usr/local/bin/emacs' @Arguments;
            }
        } else {
            Write-Warning 'Cannot find emacs';
        }
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
