Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass -Force;
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12;

$Script:RepoUrl = 'https://github.com/zichun/personal-config.git';
$Script:RepoRoot = if ($PSScriptRoot -and (Test-Path -LiteralPath (Join-Path $PSScriptRoot 'emacs'))) {
    $PSScriptRoot
} else {
    Join-Path $env:USERPROFILE 'personal-config'
};

function Test-Administrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent();
    $principal = New-Object Security.Principal.WindowsPrincipal($identity);
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator);
}

function Update-SessionPath {
    $machinePath = [Environment]::GetEnvironmentVariable('Path', [EnvironmentVariableTarget]::Machine);
    $userPath = [Environment]::GetEnvironmentVariable('Path', [EnvironmentVariableTarget]::User);
    $entries = @($machinePath, $userPath) -join ';' -split ';' |
        Where-Object { -not [string]::IsNullOrWhiteSpace($_) } |
        Select-Object -Unique;
    $env:Path = $entries -join ';';
}

function Add-MachinePath {
    param(
        [Parameter(Mandatory)]
        [string]$PathEntry
    )
    $normalized = $PathEntry.TrimEnd('\');
    $current = [Environment]::GetEnvironmentVariable('Path', [EnvironmentVariableTarget]::Machine);
    $entries = @();
    if ($current) {
        $entries = $current -split ';' | Where-Object { -not [string]::IsNullOrWhiteSpace($_) };
    }
    $alreadyPresent = $entries | Where-Object { $_.TrimEnd('\') -ieq $normalized };
    if (-not $alreadyPresent) {
        $updated = ($entries + $PathEntry) -join ';';
        [Environment]::SetEnvironmentVariable('Path', $updated, [EnvironmentVariableTarget]::Machine);
    }
    Update-SessionPath;
}

function Install-WingetPackage {
    param(
        [Parameter(Mandatory)]
        [string]$Id
    )
    & winget list --id $Id --exact *> $null;
    if ($LASTEXITCODE -eq 0) {
        Write-Host "$Id is already installed.";
        return;
    }

    & winget install --id $Id --exact --accept-package-agreements --accept-source-agreements;
    if ($LASTEXITCODE -ne 0 -and $LASTEXITCODE -ne -1978335189) {
        Write-Warning "winget install '$Id' exited with code $LASTEXITCODE";
    }
}

function Set-Link {
    param(
        [Parameter(Mandatory)]
        [string]$Path,
        [Parameter(Mandatory)]
        [string]$Target,
        [ValidateSet('SymbolicLink', 'Junction')]
        [string]$Type = 'SymbolicLink'
    )
    if (-not (Test-Path -LiteralPath $Target)) {
        Write-Warning "Link target does not exist: $Target";
        return;
    }

    $parent = Split-Path -Path $Path -Parent;
    if ($parent -and -not (Test-Path -LiteralPath $parent)) {
        New-Item -ItemType Directory -Path $parent -Force | Out-Null;
    }

    if (Test-Path -LiteralPath $Path) {
        $existing = Get-Item -LiteralPath $Path -Force;
        if ($existing.LinkType -eq $Type -and ($existing.Target -contains $Target)) {
            return;
        }
        if ($existing.LinkType) {
            # Use .Delete() so directory junctions/symlinks are unlinked without recursing into targets.
            $existing.Delete();
        } else {
            $backup = "$Path.old";
            if (Test-Path -LiteralPath $backup) {
                Remove-Item -LiteralPath $backup -Recurse -Force;
            }
            Move-Item -LiteralPath $Path -Destination $backup -Force;
        }
    }

    New-Item -ItemType $Type -Path $Path -Target $Target -Force | Out-Null;
}

function Ensure-Repo {
    Install-WingetPackage -Id 'Git.Git';
    Update-SessionPath;

    if (Test-Path -LiteralPath $Script:RepoRoot) {
        return;
    }

    $git = Get-Command git -ErrorAction SilentlyContinue;
    $gitExe = if ($git) { $git.Source } else { Join-Path $env:ProgramFiles 'Git\cmd\git.exe' };
    & $gitExe clone $Script:RepoUrl $Script:RepoRoot;
}

function Install-WinGet {
    if (Get-Command winget -ErrorAction SilentlyContinue) {
        return;
    }

    $releaseUrl = 'https://api.github.com/repos/microsoft/winget-cli/releases/latest';
    $assetUrl = (Invoke-WebRequest -Uri $releaseUrl -UseBasicParsing).Content |
        ConvertFrom-Json |
        Select-Object -ExpandProperty 'assets' |
        Where-Object { $_.browser_download_url -match '\.msixbundle$' } |
        Select-Object -First 1 -ExpandProperty 'browser_download_url';

    if (-not $assetUrl) {
        throw 'Could not locate winget .msixbundle asset from GitHub releases.';
    }

    $installerPath = Join-Path $env:TEMP 'Microsoft.DesktopAppInstaller.msixbundle';
    try {
        Invoke-WebRequest -Uri $assetUrl -OutFile $installerPath -UseBasicParsing;
        Add-AppxPackage -Path $installerPath;
    } finally {
        if (Test-Path -LiteralPath $installerPath) {
            Remove-Item -LiteralPath $installerPath -Force;
        }
    }
}

function Install-Emacs {
    Install-WingetPackage -Id 'GNU.Emacs';

    $emacsRoot = Join-Path $env:ProgramFiles 'Emacs';
    if (Test-Path -LiteralPath $emacsRoot) {
        $emacsDir = Get-ChildItem -LiteralPath $emacsRoot -Directory |
            Where-Object { $_.Name -like 'emacs*' } |
            Sort-Object Name -Descending |
            Select-Object -First 1;
        if ($emacsDir) {
            Add-MachinePath -PathEntry (Join-Path $emacsDir.FullName 'bin');
        }
    }

    $emacsConfigTarget = Join-Path $Script:RepoRoot 'emacs\.emacs.d';
    Set-Link -Path (Join-Path $env:USERPROFILE '.emacs.d') -Target $emacsConfigTarget -Type Junction;

    $env:HOME = $env:USERPROFILE;
    [Environment]::SetEnvironmentVariable('HOME', $env:USERPROFILE, [EnvironmentVariableTarget]::Machine);
}

function Install-Neomacs {
    $installDir = 'C:\tools\neomacs';
    $neomacsExe = Join-Path $installDir 'neomacs.exe';
    $releaseUrl = 'https://api.github.com/repos/eval-exec/neomacs/releases/latest';
    $headers = @{ 'User-Agent' = 'PowerShell' };

    $release = (Invoke-WebRequest -Uri $releaseUrl -Headers $headers -UseBasicParsing).Content |
        ConvertFrom-Json;

    $latestVersion = if ($release.tag_name) { $release.tag_name.TrimStart('v') } else { $null };
    if ($latestVersion -and (Test-Path -LiteralPath $neomacsExe)) {
        $currentVersionLine = (& $neomacsExe --version 2>$null | Select-Object -First 1);
        if ($currentVersionLine -match [regex]::Escape($latestVersion)) {
            Write-Host "Neomacs $latestVersion is already installed.";
            Add-MachinePath -PathEntry $installDir;
            return;
        }
    }

    $arch = if ($env:PROCESSOR_ARCHITECTURE -eq 'ARM64') { 'aarch64' } else { 'x86_64' };
    $assetUrl = $release.assets |
        Where-Object { $_.name -like "neomacs-*-$arch-pc-windows-msvc.zip" } |
        Select-Object -First 1 -ExpandProperty 'browser_download_url';

    if (-not $assetUrl) {
        Write-Warning "Could not locate Neomacs Windows ($arch) release zip.";
        return;
    }

    $zipPath = Join-Path $env:TEMP 'neomacs.zip';
    $extractPath = Join-Path $env:TEMP 'neomacs-extract';
    try {
        if (Test-Path -LiteralPath $extractPath) {
            Remove-Item -LiteralPath $extractPath -Recurse -Force;
        }
        Invoke-WebRequest -Uri $assetUrl -Headers $headers -OutFile $zipPath -UseBasicParsing;
        Expand-Archive -Path $zipPath -DestinationPath $extractPath -Force;

        $sourceDir = $extractPath;
        if (-not (Test-Path -LiteralPath (Join-Path $extractPath 'neomacs.exe'))) {
            $nestedDir = Get-ChildItem -LiteralPath $extractPath -Directory |
                Where-Object { Test-Path -LiteralPath (Join-Path $_.FullName 'neomacs.exe') } |
                Select-Object -First 1;
            if ($nestedDir) {
                $sourceDir = $nestedDir.FullName;
            }
        }

        $toolsRoot = Split-Path -Path $installDir -Parent;
        if (-not (Test-Path -LiteralPath $toolsRoot)) {
            New-Item -ItemType Directory -Path $toolsRoot -Force | Out-Null;
        }
        if (Test-Path -LiteralPath $installDir) {
            Remove-Item -LiteralPath $installDir -Recurse -Force;
        }
        Copy-Item -LiteralPath $sourceDir -Destination $installDir -Recurse -Force;
        Add-MachinePath -PathEntry $installDir;
    } finally {
        if (Test-Path -LiteralPath $zipPath) {
            Remove-Item -LiteralPath $zipPath -Force;
        }
        if (Test-Path -LiteralPath $extractPath) {
            Remove-Item -LiteralPath $extractPath -Recurse -Force;
        }
    }
}

function Install-Packages {
    $packages = @(
        'Microsoft.VisualStudioCode',
        '7zip.7zip',
        'Git.Git',
        'Mozilla.Firefox',
        'OpenJS.NodeJS',
        'Python.Python.3.12',
        # 'Microsoft.Office',
        'Microsoft.VisualStudio.2022.Community',
        'LLVM.LLVM',
        'Microsoft.VisualStudio.2022.BuildTools',
        'SyncTrayzor.SyncTrayzor',
        'BrechtSanders.WinLibs.POSIX.UCRT'
    );

    foreach ($pkg in $packages) {
        Install-WingetPackage -Id $pkg;
    }

    Update-SessionPath;

    if (Get-Command npm -ErrorAction SilentlyContinue) {
        & npm install -g '@mermaid-js/mermaid-cli';
    } else {
        Write-Warning 'npm not found on PATH; skipping @mermaid-js/mermaid-cli installation.';
    }
}

function Install-Pwsh {
    Install-WingetPackage -Id 'Microsoft.PowerShell';
    Update-SessionPath;

    if (-not (Get-PackageProvider -Name NuGet -ErrorAction SilentlyContinue)) {
        Install-PackageProvider -Name NuGet -MinimumVersion 2.8.5.201 -Scope CurrentUser -Force | Out-Null;
    }

    Install-Module -Name 'PowerShellGet' -Scope CurrentUser -Force -AllowClobber;
    Install-Module -Name 'posh-git' -Scope CurrentUser -Force;
    Install-Module -Name 'ClipboardText' -Scope CurrentUser -Force;
    Install-Module -Name 'oh-my-posh' -RequiredVersion '2.0.245' -Scope CurrentUser -Force -AllowClobber;

    $psSourceDir = Join-Path $Script:RepoRoot 'powershell';
    $docsDir = [Environment]::GetFolderPath('MyDocuments');
    $targetDirs = @(
        (Join-Path $docsDir 'WindowsPowerShell'),
        (Join-Path $docsDir 'PowerShell')
    );

    $configFiles = Get-ChildItem -Path $psSourceDir -File |
        Where-Object { $_.Extension -in '.ps1', '.psm1' };

    foreach ($targetDir in $targetDirs) {
        if (-not (Test-Path -LiteralPath $targetDir)) {
            New-Item -ItemType Directory -Path $targetDir -Force | Out-Null;
        }
        foreach ($file in $configFiles) {
            Set-Link -Path (Join-Path $targetDir $file.Name) -Target $file.FullName -Type SymbolicLink;
        }
    }
}

function Install-Rust {
    Install-WingetPackage -Id 'Rustlang.Rustup';
    Install-WingetPackage -Id 'Rustlang.rust-analyzer';

    Update-SessionPath;
    $cargoBin = Join-Path $env:USERPROFILE '.cargo\bin';
    if ((Test-Path -LiteralPath $cargoBin) -and ($env:Path -notlike "*$cargoBin*")) {
        $env:Path = "$env:Path;$cargoBin";
    }

    if (Get-Command rustup -ErrorAction SilentlyContinue) {
        & rustup update;
        & rustup component add clippy --toolchain stable-x86_64-pc-windows-msvc;
        & rustup component add rust-src;
    } else {
        Write-Warning 'rustup not found on PATH; skipping rustup/cargo setup.';
        return;
    }

    if (Get-Command cargo -ErrorAction SilentlyContinue) {
        & cargo install rust-script;
        & cargo install emacs-lsp-booster;
    }
}

function Install-Ripgrep {
    Install-WingetPackage -Id 'BurntSushi.ripgrep.GNU';

    $target = Join-Path $Script:RepoRoot 'ripgrep\.ripgreprc';
    Set-Link -Path (Join-Path $env:USERPROFILE '.ripgreprc') -Target $target -Type SymbolicLink;
}

function Install-WindowsTerminal {
    $source = Join-Path $Script:RepoRoot 'terminal\settings.json';
    $pkgDir = Get-ChildItem -Path (Join-Path $env:LOCALAPPDATA 'Packages') -Filter 'Microsoft.WindowsTerminal*' -Directory -ErrorAction SilentlyContinue |
        Select-Object -First 1;

    if (-not $pkgDir) {
        Write-Warning 'Cannot find Windows Terminal package directory in LocalAppData.';
        return;
    }

    $settingsPath = Join-Path $pkgDir.FullName 'LocalState\settings.json';
    Set-Link -Path $settingsPath -Target $source -Type SymbolicLink;
}

function Install-AutoHotkey {
    Install-WingetPackage -Id 'AutoHotkey.AutoHotkey';

    $ahkDir = Join-Path $Script:RepoRoot 'ahk';
    $startupDir = [Environment]::GetFolderPath('Startup');

    if (-not (Test-Path -LiteralPath $ahkDir)) {
        Write-Warning "AHK directory not found: $ahkDir";
        return;
    }

    Get-ChildItem -Path $ahkDir -Filter '*.ahk' -File | ForEach-Object {
        Set-Link -Path (Join-Path $startupDir $_.Name) -Target $_.FullName -Type SymbolicLink;
    }
}

function Install-FontDirectory {
    param(
        [Parameter(Mandatory)]
        [string]$Path
    )
    if (-not (Test-Path -LiteralPath $Path)) {
        return;
    }

    $shellApp = New-Object -ComObject Shell.Application;
    $fontsFolder = $shellApp.NameSpace(0x14);
    $systemFontsDir = Join-Path $env:SystemRoot 'Fonts';
    $userFontsDir = Join-Path $env:LOCALAPPDATA 'Microsoft\Windows\Fonts';

    Get-ChildItem -Path $Path -Filter '*.ttf' -File | ForEach-Object {
        $installedSystem = Test-Path -LiteralPath (Join-Path $systemFontsDir $_.Name);
        $installedUser = Test-Path -LiteralPath (Join-Path $userFontsDir $_.Name);
        if (-not ($installedSystem -or $installedUser)) {
            $fontsFolder.CopyHere($_.FullName, 0x10);
        }
    }
}

function Install-Fonts {
    $powerlineDir = Join-Path $env:TEMP 'powerline-fonts';
    try {
        if (Test-Path -LiteralPath $powerlineDir) {
            Remove-Item -LiteralPath $powerlineDir -Recurse -Force;
        }
        & git clone 'https://github.com/powerline/fonts.git' $powerlineDir;
        Push-Location $powerlineDir;
        try {
            & .\install.ps1;
        } finally {
            Pop-Location;
        }
    } finally {
        if (Test-Path -LiteralPath $powerlineDir) {
            Remove-Item -LiteralPath $powerlineDir -Recurse -Force;
        }
    }

    $fontRepos = @(
        @{ Name = 'all-the-icons.el'; Url = 'https://github.com/domtronn/all-the-icons.el.git' },
        @{ Name = 'nerd-icons.el';    Url = 'https://github.com/rainstormstudio/nerd-icons.el.git' }
    );

    foreach ($repo in $fontRepos) {
        $tempDir = Join-Path $env:TEMP $repo.Name;
        try {
            if (Test-Path -LiteralPath $tempDir) {
                Remove-Item -LiteralPath $tempDir -Recurse -Force;
            }
            & git clone $repo.Url $tempDir;
            Install-FontDirectory -Path (Join-Path $tempDir 'fonts');
        } finally {
            if (Test-Path -LiteralPath $tempDir) {
                Remove-Item -LiteralPath $tempDir -Recurse -Force;
            }
        }
    }

    # Local Nerd-Icons
    Install-FontDirectory -Path (Join-Path $Script:RepoRoot 'fonts');

    Install-CascadiaCode;
}

function Install-CascadiaCode {
    $releaseUrl = 'https://api.github.com/repos/microsoft/cascadia-code/releases/latest';
    $assetUrl = (Invoke-WebRequest -Uri $releaseUrl -UseBasicParsing).Content |
        ConvertFrom-Json |
        Select-Object -ExpandProperty 'assets' |
        Where-Object { $_.name -like 'CascadiaCode-*.zip' } |
        Select-Object -First 1 -ExpandProperty 'browser_download_url';

    if (-not $assetUrl) {
        Write-Warning 'Could not locate CascadiaCode release zip.';
        return;
    }

    $zipPath = Join-Path $env:TEMP 'cascadiacode.zip';
    $extractPath = Join-Path $env:TEMP 'cascadiacode';
    try {
        if (Test-Path -LiteralPath $extractPath) {
            Remove-Item -LiteralPath $extractPath -Recurse -Force;
        }
        Invoke-WebRequest -Uri $assetUrl -OutFile $zipPath -UseBasicParsing;
        Expand-Archive -Path $zipPath -DestinationPath $extractPath -Force;
        Install-FontDirectory -Path (Join-Path $extractPath 'ttf');
    } finally {
        if (Test-Path -LiteralPath $zipPath) {
            Remove-Item -LiteralPath $zipPath -Force;
        }
        if (Test-Path -LiteralPath $extractPath) {
            Remove-Item -LiteralPath $extractPath -Recurse -Force;
        }
    }
}

function Invoke-Install {
    if (-not (Test-Administrator)) {
        throw 'Invoke-Install must be run from an elevated (Administrator) PowerShell session.';
    }

    Install-WinGet;
    Ensure-Repo;

    # Show Networking connectivity on standby option in "Edit Power Plan"
    & reg.exe ADD 'HKLM\SYSTEM\CurrentControlSet\Control\Power\PowerSettings\F15576E8-98B7-4186-B944-EAFA664402D9' /v Attributes /t REG_DWORD /d 2 /f;

    Install-Fonts;
    Install-Packages;
    Install-Emacs;
    Install-Neomacs;
    Install-Pwsh;
    Install-Rust;
    Install-Ripgrep;
    Install-WindowsTerminal;
    Install-AutoHotkey;
    Update-SessionPath;
}

if ($MyInvocation.InvocationName -ne '.') {
    Invoke-Install;
}
