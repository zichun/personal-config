Set-ExecutionPolicy Bypass -Force;
iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'));

choco install cmder -y;
choco install emacs -y;
choco install ripgrep -y;
choco install 7zip -y;
choco install git -y;
choco install firefox -y;
choco install nodejs -y;
choco install python -y;
choco install pwsh -y;
choco install poshgit -y;
choco install choco install visualstudio2019community -y;

Install-Module -Name PowerShellGet -Force;
Install-Module posh-git -Scope CurrentUser -Force;
Install-Module ClipboardText -Scope CurrentUser -Force;

cd $env:USERPROFILE;
& "$($env:ProgramFiles)\Git\cmd\git" clone https://github.com/zichun/personal-config.git;

# Emacs configs
cd "$($env:USERPROFILE)\personal-config\emacs";
New-Item -ItemType Junction -Path "$($env:USERPROFILE)\.emacs.d" -Value "$($pwd.path)\.emacs.d";
New-Item -Path env:\HOME -Value $env:USERPROFILE;
 [System.Environment]::SetEnvironmentVariable('HOME', $env:USERPROFILE, [System.EnvironmentVariableTarget]::Machine)

# Powershell configs
cd "$($env:USERPROFILE)\personal-config\powershell";
gci *.ps1 | % { New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)\Documents\WindowsPowerShell" -Name $_.Name -Target $_.FullName };
gci *.psm1 | % { New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)\Documents\WindowsPowerShell" -Name $_.Name -Target $_.FullName };

# pwsh configs
cd "$($env:USERPROFILE)\personal-config\powershell";
mkdir "$($env:USERPROFILE)\Documents\Powershell";
gci *.ps1 | % { New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)\Documents\Powershell" -Name $_.Name -Target $_.FullName };
gci *.psm1 | % { New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)\Documents\Powershell" -Name $_.Name -Target $_.FullName };

# ripgrep config
cd "$($env:USERPROFILE)\personal-config\ripgrep";
New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)" -Name '.ripgreprc' -Target "$($pwd.path)\.ripgreprc";

# cmder config
cd "$($env:USERPROFILE)\personal-config\cmder";
New-Item -ItemType SymbolicLink -Path \tools\Cmder\vendor\conemu-maximus5\ -Name 'ConEmu.xml' -Target "$($pwd.path)\ConEmu.xml";

#
# Setup Powerline
#
cd $env:USERPROFILE;
& "$($env:ProgramFiles)\Git\cmd\git" clone https://github.com/powerline/fonts.git;
cd fonts;
.\install.ps1;

Install-Module -Name 'oh-my-posh';

#
# rust
#
# choco install rustup;
#
# rustup update
# rustup component add clippy --toolchain stable-x86_64-pc-windows-msvc
# rustup component add rls rust-analysis rust-src;
# cargo install rust-script
#
# choco install rust-analyzer -y

