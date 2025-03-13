Set-ExecutionPolicy Bypass -Force;

function Invoke-Install {
    Install-Choco;
    Install-WinGet;

    cd $env:USERPROFILE;
    & "$($env:ProgramFiles)\Git\cmd\git" clone https://github.com/zichun/personal-config.git;

    # Show Networking connectivity on standby option in "Edit Power Plan"
    REG ADD HKLM\SYSTEM\CurrentControlSet\Control\Power\PowerSettings\F15576E8-98B7-4186-B944-EAFA664402D9 /v Attributes /t REG_DWORD /d 2 /f

    Install-Fonts;
    Install-Packages;
    Install-Emacs;
    Install-Pwsh;
    Install-Rust;
    Install-Ripgrep;
}

function Install-Choco {
    iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'));
}
function Install-WinGet {
    $URL = "https://api.github.com/repos/microsoft/winget-cli/releases/latest"
    $URL = (Invoke-WebRequest -Uri $URL).Content | ConvertFrom-Json |
      Select-Object -ExpandProperty "assets" |
      Where-Object "browser_download_url" -Match '.msixbundle' |
      Select-Object -ExpandProperty "browser_download_url"

    # download
    Invoke-WebRequest -Uri $URL -OutFile "Setup.msix" -UseBasicParsing

    # install
    Add-AppxPackage -Path "Setup.msix"

    # delete file
    Remove-Item "Setup.msix"
}

function Install-Emacs {
    winget install emacs;
    # Add emacs to Path
    $emacspath = dir "$($env:ProgramFiles)\Emacs\" | ? {$_.name.contains("emacs")} | Select-Object -ExpandProperty FullName;
    $env:Path = "$($env:Path);$emacspath\bin;";
    [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine);

    # Emacs configs
    cd "$($env:USERPROFILE)\personal-config\emacs";
    New-Item -ItemType Junction -Path "$($env:USERPROFILE)\.emacs.d" -Value "$($pwd.path)\.emacs.d";
    New-Item -Path env:\HOME -Value $env:USERPROFILE;
    [System.Environment]::SetEnvironmentVariable('HOME', $env:USERPROFILE, [System.EnvironmentVariableTarget]::Machine)
}

function Install-Packages {
    winget install Microsoft.VisualStudioCode;
    winget install 7zip.7zip;
    winget install Git.Git;
    winget install Mozilla.Firefox;
    winget install nodejs;
    winget install Python.Python.3.12;
    # winget install Microsoft.Office;
    winget install Microsoft.VisualStudio.2022.Community;
    winget install LLVM.LLVM;
    winget install --id=Microsoft.VisualStudio.2022.BuildTools -e;
    winget install SyncTrayzor;

    choco install mingw -y;

    npm install -g mermaid.cli;
}

function Install-Pwsh {
    winget install Microsoft.PowerShell;

    # posh-git
    Install-Module -Name PowerShellGet -Force;
    Install-Module posh-git -Scope CurrentUser -Force;
    Install-Module ClipboardText -Scope CurrentUser -Force;
    Install-Module -Name 'oh-my-posh' -RequiredVersion '2.0.245';

    # Powershell configs
    cd "$($env:USERPROFILE)\personal-config\powershell";
    gci *.ps1 | % { New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)\Documents\WindowsPowerShell" -Name $_.Name -Target $_.FullName };
    gci *.psm1 | % { New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)\Documents\WindowsPowerShell" -Name $_.Name -Target $_.FullName };

    # pwsh configs
    cd "$($env:USERPROFILE)\personal-config\powershell";
    mkdir "$($env:USERPROFILE)\Documents\Powershell";
    gci *.ps1 | % { New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)\Documents\Powershell" -Name $_.Name -Target $_.FullName };
    gci *.psm1 | % { New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)\Documents\Powershell" -Name $_.Name -Target $_.FullName };
}

function Install-Rust {
    winget install Rustlang.Rustup;

    rustup update
    rustup component add clippy --toolchain stable-x86_64-pc-windows-msvc
    rustup component add rls rust-analysis rust-src;
    cargo install rust-script;
    cargo install emacs-lsp-booster;
    choco install rust-analyzer -y;
}

function Install-Ripgrep {
    winget install BurntSushi.ripgrep.GNU;
    # ripgrep config
    cd "$($env:USERPROFILE)\personal-config\ripgrep";
    New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)" -Name '.ripgreprc' -Target "$($pwd.path)\.ripgreprc";
}

function Install-WindowsTerminal {
    # WindowsTerminal config
    cd "$($env:USERPROFILE)\personal-config\terminal";
    $dir = Get-ChildItem '~\AppData\Local\Packages\Microsoft.WindowsTerminal*';
    if (-not $dir)
    {
        Write-Warning 'Cannot find WindowsTerminal in AppData';
    }
    else
    {
        $settings = "$($dir[0].FullName)\LocalState\settings.json";
        if (Test-Path $settings)
        {
            move $settings "$settings.old";
        }
        New-Item -ItemType SymbolicLink -Path "$($dir[0].FullName)\LocalState\" -Name 'settings.json' -Target "$($pwd.path)\settings.json";
    }
}

function Install-Fonts {
    cd $env:USERPROFILE;
    & "$($env:ProgramFiles)\Git\cmd\git" clone https://github.com/powerline/fonts.git;
    cd fonts;
    .\install.ps1;

    cd $env:USERPROFILE;
    Remove-Item fonts -Recurse -Force;

    git clone https://github.com/domtronn/all-the-icons.el.git;
    $shellApp = New-Object -ComObject shell.application
    $fonts = $shellApp.NameSpace(0x14)
    dir all-the-icons.el/fonts/*.ttf | % {
        $fonts.CopyHere($_.FullName);
    };
    Remove-Item all-the-icons.el -Recurse -Force;

    git clone https://github.com/rainstormstudio/nerd-icons.el.git;
    $shellApp = New-Object -ComObject shell.application
    $fonts = $shellApp.NameSpace(0x14)
    dir nerd-icons.el/fonts/*.ttf | % {
        $fonts.CopyHere($_.FullName);
    };
    Remove-Item nerd-icons.el -Recurse -Force;

    # Nerd-Icons
    dir personal-config/fonts/*.ttf | % {
        $fonts.CopyHere($_.FullName);
    };

    Install-CascadiaCode;
}
function Install-CascadiaCode {
    $URL = 'https://api.github.com/repos/microsoft/cascadia-code/releases/latest';
    $URL = (Invoke-WebRequest -Uri $URL).Content | ConvertFrom-Json |
      Select-Object -ExpandProperty "assets" | Select-Object -First 1 |
	  Select-Object -ExpandProperty "browser_download_url";
    Invoke-WebRequest -Uri $URL -OutFile "cascadiacode.zip" -UseBasicParsing;
    Expand-Archive cascadiacode.zip;

    $shellApp = New-Object -ComObject shell.application
    $fonts = $shellApp.NameSpace(0x14)
    dir cascadiacode/ttf/*.ttf | % {
        $fonts.CopyHere($_.FullName);
    };
    Remove-Item cascadiacode.zip;
    Remove-Item cascadiacode -Recurse -Force;
}

Invoke-Install;
