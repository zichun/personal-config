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

Install-Module -Name PowerShellGet -Force;
Install-Module posh-git -Scope CurrentUser -Force;

cd $env:USERPROFILE;
& "$($env:ProgramFiles)\Git\cmd\git" clone https://github.com/zichun/personal-config.git;

# Emacs configs
cd "$($env:USERPROFILE)\personal-config\emacs";
New-Item -ItemType Junction -Path "$($env:USERPROFILE)\.emacs.d" -Value "$($pwd.path)\.emacs.d";
New-Item -Path env:\HOME -Value $env:USERPROFILE;

# Powershell configs
cd "$($env:USERPROFILE)\personal-config\powershell";
gci *.ps1 | % { New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)\Documents\WindowsPowerShell" -Name $_.Name -Target $_.FullName };

# ripgrep config
cd "$($env:USERPROFILE)\personal-config\ripgrep";
New-Item -ItemType SymbolicLink -Path "$($env:USERPROFILE)" -Name '.ripgrep' -Target "$($pwd.path)\.ripgrep";

# cmder config
cd "$($env:USERPROFILE)\personal-config\cmder";
New-Item -ItemType SymbolicLink -Path \tools\Cmder\vendor\conemu-maximus5\ -Name 'ConEmu.xml' -Target "$($pwd.path)\ConEmu.xml";
