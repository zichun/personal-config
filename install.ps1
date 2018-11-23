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

mkdir \temp;
cd \temp;
& "$($env:ProgramFiles)\Git\cmd\git" clone https://github.com/zichun/personal-config.git;

; Emacs configs
cd \temp\personal-config\emacs;
Copy-Item -Force -Recurse * $env:USERPROFILE;
New-Item -Path env:\HOME -Value $env:USERPROFILE;

; Powershell configs
cd \temp\personal-config\powershell;
copy * "$($env:USERPROFILE)\Documents\WindowsPowerShell\";

; ripgrep config
cd \temp\personal-config\ripgrep;
copy * $env:USERPROFILE;

; cmder config
cd \temp\personal-config\cmder;
copy * \tools\Cmder\vendor\conemu-maximus5\;
