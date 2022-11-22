function car($arr) {
    $first, $null = $arr;
    return $first;
}

function cdr($arr) {
    $null, $rest = $arr;
    return $rest;
}

function Show-Notification {
    param($title,
          $text,
          $type = 'Info',
          $notificationTimeout = 10)

    # load Windows Forms and drawing assemblies
    [reflection.assembly]::loadwithpartialname("System.Windows.Forms") | Out-Null
    [reflection.assembly]::loadwithpartialname("System.Drawing") | Out-Null

    # define an icon image pulled from PowerShell.exe
    $icon=[system.drawing.icon]::ExtractAssociatedIcon((join-path $pshome powershell.exe))
    $notify = new-object system.windows.forms.notifyicon
    $notify.icon = $icon
    $notify.visible = $True

    # define the tool tip icon based on the message type
    switch ($type) {
        "Error" { $messageIcon=[system.windows.forms.tooltipicon]::Error}
        "Info" {$messageIcon=[system.windows.forms.tooltipicon]::Info}
        "Warning" {$messageIcon=[system.windows.forms.tooltipicon]::Warning}
        Default {$messageIcon=[system.windows.forms.tooltipicon]::None}
    }

    #display the balloon tipe
    $notify.showballoontip($notificationTimeout,$title,$text,$type)
}

function Set-RemoteClipboard {
    [CmdletBinding()]
    Param(
 		[Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$data
    )
    $data | out-file E:\logs\clip.txt;
}

function Get-LastCommand
{
    $lastHistory = (Get-History -count 1);
    $lastCommand = $lastHistory.CommandLine;
    $Global:LastCommand = $lastCommand;
    Set-Clipboard $Global:LastCommand;
    return $Global:LastCommand;
}

function Get-LastResult {
    $Global:lr = $Global:LastResult
    Write-Output $Global:lr
}
function lr {
    $Global:lr
}

function Find-File {
    param(
        $Pattern,
        [switch]$Table
    )
    if ($Table)
    {
        $items = (Get-ChildItem -r -i $Pattern)
        Write-Output $items | Format-Table @{Name="FullName";Expression={"$($_.FullName):1:"}},Mode,LastWriteTime,Length;
    }
    else
    {
        $Script:FindFileResult = @();

        Get-ChildItem -r -i $Pattern | % {
            $file = "$($_.FullName):1:";
            $Script:FindFileResult += ,$file;
            Write-Output $file;
        };
    }
}

function op($ind) {
    $res = $Script:FindFileResult;

    try {
        if ($res[$ind].GetType().ToString() -eq 'System.IO.FileInfo') {
            Write-Host "emacs $($res[$ind].FullName)"
            emacs $res[$ind].FullName;
        }
        elseif ($res[$ind] -match '(.*):([0-9]+):')
        {
            write-host $res[$ind];
            Write-Host "Opening $($matches[1]):$($matches[2])";
            & "$($env:PROGRAMDATA)\chocolatey\lib\Emacs\tools\emacs\bin\emacsclientw.exe" '-n' '--no-wait' '--alternate-editor=`"C:\ProgramData\chocolatey\lib\Emacs\tools\emacs\bin\runemacs.exe`"' "+$($matches[2]):0" $matches[1];
        }
    } catch {
    }
}

function Get-OrgImageLink {
    add-type -an system.windows.forms;
    $Script:OrgDirectory = "$($env:USERPROFILE)\OneDrive\org";

    $imageDir = "$($Script:OrgDirectory)\images";
    $image = [System.Windows.Forms.Clipboard]::GetImage();

    if (-not $image)
    {
        Write-Warning 'No image found in Clipboard';
        return;
    }

    $t = [DateTime]::Now;
    $fileName = "$($t.ToString('yyyy-MM-ddThh-mm-ss-ms')).png";
    $filePath = "$imageDir\$fileName";
    $image.Save($filePath);

    $orgLink = "[[./images/$fileName]]";
    Write-Host "Image saved to $filePath. Clipboard replaced: $orgLink";
    Set-Clipboard $orgLink;
}

$Script:CachedDirectories = @();
function Get-CachedDirectories
{
    $cnt = 0;
    $Script:CachedDirectories | % {
        Write-Host "[$cnt]: $_";
        $cnt++;
    };
}

function New-CachedDirectory
{
    param($NewPath)

    if ($NewPath)
    {
        $path = $NewPath;
    }
    else
    {
        $path = ((pwd).Path);
    }

    if (-not ($Script:CachedDirectories -contains $path))
    {
        $Script:CachedDirectories += ,$path;
    }
    Get-CachedDirectories;
}

function Set-CachedDirectory
{
    param($Index)

    if (-not $Index -and ($Index -ne '0'))
    {
        $Index = Invoke-MenuSelection -Options $Script:CachedDirectories;
    }

    $Index = [int]::parse($Index);
    if ($Index -lt 0 -or $Index -ge $Script:CachedDirectories.Count)
    {
        Write-Error "Index out of bounds";
        return;
    }
    cd $Script:CachedDirectories[$Index];
}

function Remove-CachedDirectory
{
    param([int]$Index)

    if ($Index -lt 0 -or $Index -ge @($Script:CachedDirectories).Count)
    {
        Write-Error "Index out of bounds. Index: $Index, Count:$(@($Script:CachedDirectories).Count)";
        return;
    }

    $len = @($Script:CachedDirectories).Count;
    if ($len -eq 1)
    {
        $Script:CachedDirectories = @();
    }
    elseif ($Index -eq 0)
    {
        $Script:CachedDirectories = $Script:CachedDirectories[1..($len - 1)];
    }
    elseif ($Index -eq $len - 1)
    {
        $Script:CachedDirectories = $Script:CachedDirectories[0..($len - 2)];
    }
    else
    {
        $Script:CachedDirectories = $Script:CachedDirectories[0..($Index - 1)] + $Script:CachedDirectories[($Index + 1) .. ($len - 1)];
    }
    Get-CachedDirectories;
}

Set-Alias ld Get-CachedDirectories;
Set-Alias ad New-CachedDirectory;
Set-Alias dd Remove-CachedDirectory;
Set-Alias gd Set-CachedDirectory;

$Script:ScratchPath = 'Z:\scratch.txt';
function New-Scratch {
    [CmdletBinding()]
    Param(
 		[Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$data
    )
    if (Test-Path $Script:ScratchPath)
    {
        $scratch = (gc $Script:ScratchPath) | ConvertFrom-JSON;
        $scratch = @($scratch) + ,$data;
        $scratch | ConvertTo-JSON | Out-File $Script:ScratchPath;
    }
    else
    {
        '[' + ($data | ConvertTo-JSON) + ']' | Out-File $Script:ScratchPath;
    }
}
function Get-Scratch {
    if (Test-Path $Script:ScratchPath)
    {
        $scratch = (gc $Script:ScratchPath) | ConvertFrom-JSON;
    }
    else
    {
        return;
    }
    $sel = Invoke-MenuSelection -Options $scratch;
    if ($sel -ne $null)
    {
        Set-Clipboard $scratch[$sel];
    }
}
function Clear-Scratch {
    if (Test-Path $Script:ScratchPath)
    {
        Remove-Item $Script:ScratchPath;
    }
}

Set-Alias ns New-Scratch;
Set-Alias gs Get-Scratch;
Set-Alias cs Clear-Scratch;

function Group-ObjectTree {
    param(
        [Parameter(Position=2,
                   Mandatory=$true,
                   ValueFromPipeline=$true)]
        [Object]
        $Object,
        [Parameter(Position=0,
                   Mandatory=$true)]
        [String]
        $Identifier,

        [Parameter(Position=1,
                   Mandatory=$true)]
        [String]
        $ParentKey
    )
    begin {
        $objects = @();
        $tr = @();
    }
    process {
        $objects += $Object;
    }
    end {
        $objHash = @{};
        foreach ($o in $objects)
        {
            $objHash[$o.$Identifier.ToString()] = @{
                'Identifier' = $o.$Identifier.ToString();
                'Object' = $o;
                'Children' = @();
            };
        }

        foreach ($o in $objects)
        {
            $key = $o.$ParentKey.ToString();
            if ($key -and $objHash.ContainsKey($key))
            {
                $parent = $objHash[$key];
                $parent.Children += ,($objHash[$o.$Identifier.ToString()]);
            }
            else
            {
                $tr += ,($objHash[$o.$Identifier.ToString()]);
            }
        }
        return $tr;
    }
}

function Out-Tree {
    param(
 		[Parameter(Position=4,Mandatory=$true, ValueFromPipeline=$true)]
        $Tree,
 		[Parameter(Position=0)]
        [Object]$Identifier = '',
 		[Parameter(Position=1)]
        $Spacing = 4,
 		[Parameter(Position=2)]
        $Depth = 0,
 		[Parameter(Position=3)]
        $LastArr = @()
    )
    begin {
        $objects = @();
    }
    process {
        $objects += $Tree
    }
    end {
        $objects | Out-TreeScriptBlock -Script {
            param($Object, $Prefix)
            Write-HostOutput $Prefix -NoNewLine;
            if ($Identifier.GetType().Name -eq 'ScriptBlock')
            {
                $toPrint = $Object | % $Identifier;
                Write-HostOutput $toPrint;
            }
            elseif ($Identifier.GetType().Name -eq 'String' -and $Identifier)
            {
                Write-HostOutput $Object.$Identifier;
            }
            else
            {
                Write-HostOutput $Object.Identifier;
            }
        };
    }
}

function Invoke-Environment
{
    param
    (
        [Parameter(Mandatory=$true)] [string]
        $Command
    )

    cmd /c "$Command > nul 2>&1 && set" | .{process{
                                                if ($_ -match '^([^=]+)=(.*)') {
                                                    [System.Environment]::SetEnvironmentVariable($matches[1], $matches[2])
                                                }
                                            }}

    if ($LASTEXITCODE) {
        throw "Command '$Command': exit code: $LASTEXITCODE"
    }
}

$rename = Get-Command RenameTab -erroraction 'ignore';
if (-not $rename)
{
    function RenameTab($name) {
        $host.ui.rawui.WindowTitle = $name;
    }
}
