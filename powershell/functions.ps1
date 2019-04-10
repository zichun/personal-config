function Remove-FCShellCrap {
    $regex = '\b[A-F0-9]{8}(?:-[A-F0-9]{4}){3}-[A-F0-9]{12}\b';
    get-item * | ? {$_ -match $regex } | remove-item -recurse;

    write-host "Remember to delete the zip files";
}

function car($arr) {
    $first, $null = $arr;
    return $first;
}

function cdr($arr) {
    $null, $rest = $arr;
    return $rest;
}

function Curry($Name) {
    if ($Name -eq $null) {
        throw "Function name not specified"
    }

    $func = Get-Item function:$Name

    $closedFunc = [ScriptBlock]::Create($func.Definition);
    $calledArgs = $args

    return { & $closedFunc @calledArgs @args }.GetNewClosure()
}

function Write-HostAndHighlightPattern(
    [string]$inputText,
    [System.Text.RegularExpressions.Regex]$regex
)
{
	$index = 0
	while($index -lt $inputText.Length)
	{
		$match = $regex.Match($inputText, $index)
		if($match.Success -and $match.Length -gt 0)
		{
			Write-Host $inputText.SubString($index, $match.Index - $index) -nonewline
			Write-Host $match.Value.ToString() -ForegroundColor Blue -nonewline
			$index = $match.Index + $match.Length
		}
		else
		{
			Write-Host $inputText.SubString($index) -nonewline
			$index = $inputText.Length
		}
	}
}

function Highlight-String {
    [CmdletBinding()]
    param(
        [Parameter(Position=1,
                   Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true)]
        [Object]
        $Hay,
        [Parameter(Position=0,
                   Mandatory=$true)]
        [String]
        $Needle
    )
    begin {
        $regexPattern = "(?i)$Needle";
        $objects = @();
        $regex = New-Object System.Text.RegularExpressions.Regex $regexPattern;
    }
    process { $objects += $Hay; }
    end {
        $str = $objects | Out-String;
        Write-HostAndHighlightPattern $str $regex;
    }

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

function Get-ColorPalette([System.ConsoleColor]$BackgroundColor) {
    switch ($BackgroundColor.ToString()) {
        'Black' {
            @('Cyan','Green','Blue','Gray','Red','White','Yellow','Magenta');
        }
        'DarkMagenta' {
            @('White','Green','Yellow','Red','Gray','Cyan','Blue');
        }
        'DarkBlue' {
            @('White','Green','Yellow','Red','Gray','Cyan','Magenta');
        }
    }
}

function New-LexicalClosure {
    Param(
        [ScriptBlock]$Script,
        [HashTable]$ClosedVariableMap
    )
    Process {
        $closure = $script.GetNewClosure();

        #
        # Initialize variables.
        #

        foreach ($key in $ClosedVariableMap.Keys) {
            $closure.Module.sessionstate.PSVariable.Set($key, $ClosedVariableMap[$key]);
        }

        return {
            $tr = & $closure @args;

            foreach ($key in $ClosedVariableMap.Keys) {
                if ($tr -is 'HashTable' -and $tr.ContainsKey($key)) {
                    $closure.Module.sessionstate.PSVariable.Set($key, $tr[$key]);
                }
            }

            if ($tr -is 'HashTable' -and $tr.ContainsKey('_return')) {
                return $tr['_return'];
            }

        }.GetNewClosure();
    }
}

${Function:Write-HostOutput} = & {
    Param(
        $textBuffer,
        $coloredTextBuffer,
        $prevLine
    )

    return {
        Param(
            $Text,
            $ForegroundColor = $null,
            $BackgroundColor = $null,
            [switch]$NoNewline,
            [switch]$NoColor
        )

        function Write-TextBlock($text, $cursor, $foreground, $background) {
            $maxWidth = $Host.UI.RawUI.BufferSize.Width;
            $index = 0;

            while ($index -lt $text.length) {
                $len = [Math]::Min($text.length - $index, $maxWidth - $cursor.X);
                $row = $Host.UI.RawUI.NewBufferCellArray($text.Substring($index, $len), $foreground, $background);
                $Host.UI.RawUI.SetBufferContents($cursor, $row);

                $cursor.X += $len;
                $index += $len;

                if ($cursor.X -ge $maxWidth) {
                    $cursor.X = 0;
                    $cursor.Y++;
                }
            }

            return $cursor;
        }

        if ($ForegroundColor -eq $null) {
            $ForegroundColor = $Host.UI.RawUI.ForegroundColor;
        }

        if ($BackgroundColor -eq $null) {
            $BackgroundColor = $Host.UI.RawUI.BackgroundColor;
        }

        $textBuffer += $Text;
        $coloredTextBuffer += @{
            'Text' = $text;
            'ForegroundColor' = $ForegroundColor;
            'BackgroundColor' = $BackgroundColor
        };

        if ($nonewline -eq $false)
        {
            $cursor = $host.ui.RawUI.CursorPosition;
            Write-Output $textBuffer;

            if (-not $NoColor) {
                $cursor.X = 0;

                if ($prevLine -ne -1 -and $prevLine -eq $cursor.Y) {
                    #
                    # Clear out previously written line
                    #
                    $row = $Host.UI.RawUI.NewBufferCellArray(
                        ((0..$Host.UI.RawUI.WindowSize.Width) -Join ' ' -Replace '[0-9]', ''),
                        $Host.UI.RawUI.ForegroundColor,
                        $Host.UI.RawUI.BackgroundColor);
                    $Host.UI.RawUI.SetBufferContents($cursor, $row);

                }
                else
                {
                    $prevLine = $cursor.Y;

                    foreach ($c in $coloredTextBuffer)
                    {
                        $cursor = Write-TextBlock $c.Text $cursor $c.ForegroundColor $c.BackgroundColor;
                    }
                }
            }

            $textBuffer = '';
            $coloredTextBuffer = @();
        }

        Set-Variable -Name 'textBuffer' -Value $textBuffer -Scope 1;
        Set-Variable -Name 'coloredTextBuffer' -Value $coloredTextBuffer -Scope 1;
        Set-Variable -Name 'prevLine' -Value $prevLine -Scope 1;

    }.GetNewClosure();
} '' @() -1;

function Out-XML {
    Param(
 		[Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [XML]$Xml,

        [Switch]$Expand
    )
    $XAML=@'
<Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        WindowStartupLocation="CenterScreen"
        Title="XML Viewer" Height="650" Width="850">

<TreeView x:Name="XmlTree"></TreeView>

</Window>
'@;

    Add-Type -AssemblyName presentationframework;

    $Window = [Windows.Markup.XamlReader]::Load((New-Object System.Xml.XmlNodeReader ([xml]$XAML)));
    $xmlTree = $Window.FindName("XmlTree");

    function Enumerate-XML($node) {
        $treeNode = New-Object System.Windows.Controls.TreeViewItem;

        if ($node.NodeType -ne 'Text') {
            if ($node.NodeType -eq 'Document') {
                $treeNode.Header = $node.localName;
            } elseif ($node.outerXML -match '<[^>]*>') {
                $treeNode.Header = $matches[0];
            } else {
                $treeNode.Header = $node.localName;
            }

            foreach ($cn in $node.ChildNodes) {
                $treeNode.AddChild((Enumerate-Xml $cn));
            }
        }
        else {
            $treeNode.Header = $node.Value;
        }

        if ($Expand) {
            $treeNode.ExpandSubTree();
        }

        return $treeNode;
    }

    $xmlTree.AddChild((Enumerate-Xml $Xml));
    [void]$Window.ShowDialog();
}

function Set-RemoteClipboard {
    [CmdletBinding()]
    Param(
 		[Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$data
    )
    $data | out-file E:\logs\clip.txt;
}

function Format-XML {
    [CmdletBinding()]
    Param(
 		[Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [Xml]$Xml,
        [Switch]$Colorized,
        [Switch]$NoColor
    )
    Process {

        $x = [Xml]$Xml;
        $color = Get-ColorPalette $Host.UI.RawUI.BackgroundColor;

        function EnumerateAndPrint($x, $offset = 0) {
            function Write-OffsetSpace($offset) {
                if ($offset -eq 0) {
                    return;
                }
                0..$offset | % {Write-HostOutput ' ' -nonewline -ForegroundColor Gray}
            }

            function Write-XMLDeclaration($x, $offset) {
                Write-OffsetSpace $offset;
                Write-HostOutput $x.OuterXML -ForegroundColor $color[0] -nocolor:$nocolor;
            }

            function Write-Comment($x, $offset) {
                Write-OffsetSpace $offset;
                Write-HostOutput $x.OuterXML -ForegroundColor $color[1] -nocolor:$nocolor;
            }

            function Write-ElementOpening($x, $offset) {
                Write-OffsetSpace $offset;
                Write-HostOutput ('<{0}' -f $x.Name) -ForegroundColor $color[2] -nonewline -nocolor:$nocolor;
                foreach ($a in $x.Attributes) {
                    Write-HostOutput ' ' -nonewline -ForegroundColor $color[3] -nocolor:$nocolor;
                    Write-HostOutput $a.name -ForegroundColor $color[4] -nonewline -nocolor:$nocolor;
                    Write-HostOutput '=' -ForegroundColor $color[2] -nonewline -nocolor:$nocolor;
                    Write-HostOutput ('"{0}"' -f $a.Value) -ForegroundColor DarkGreen -nonewline -nocolor:$nocolor;
                }
                Write-HostOutput '>' -ForegroundColor $color[2] -nonewline -nocolor:$nocolor;
                Write-HostOutput '' -ForegroundColor $color[3] -nocolor:$nocolor;
            }

            function Write-ElementClosing($x, $offset) {
                Write-OffsetSpace $offset;
                Write-HostOutput ('</{0}>' -f $x.Name) -ForegroundColor $color[2] -nocolor:$nocolor;
            }
            function Write-ElementText($x, $offset) {
                Write-OffsetSpace $offset;
                Write-HostOutput $x.Value -ForegroundColor $color[3] -nocolor:$nocolor;
            }

            if ($x.NodeType -eq 'Document') {
                #
                # Root, do nothing. Enumerate children.
                #

                foreach($cn in $x.ChildNodes) {
                    EnumerateAndPrint $cn $offset;
                }
            }
            elseif ($x.NodeType -eq 'XmlDeclaration') {
                Write-XMLDeclaration $x $offset;
            }
            elseif ($x.NodeType -eq 'Comment') {
                Write-Comment $x $offset;
            }
            elseif ($x.NodeType -eq 'Element') {
                Write-ElementOpening $x $offset;

                foreach($cn in $x.ChildNodes) {
                    EnumerateAndPrint $cn ($offset + 4);
                }

                Write-ElementClosing $x $offset;
            }
            elseif ($x.NodeType -eq 'Text') {
                Write-ElementText $x $offset;
            }
        }

        EnumerateAndPrint $x;

    }
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

function Find-String
{
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$True,
                   Position=0,
                   ParameterSetName = "FilePattern")]
        [Parameter(ParameterSetName="TextBlob")]
        [String]$needlePattern,
        [Parameter(Mandatory=$True,
                   Position=1,
                   ParameterSetName = "FilePattern")]
        [String]$filePattern = "*",

        [Switch]$CaseSensitive = $false,
        [Int32[]]$Context = @(0, 0),

        [Parameter(ParameterSetName = "FilePattern")]
        [Switch]$Summarize = $false,
        [Switch]$NoHighlight = $false,
        [Switch]$AsObject = $false,

        [Parameter(Mandatory=$True,
                   ValueFromPipeline=$True,
                   ParameterSetName = "TextBlob")]
        [Alias('host')]
        [string]$Text
    )
    BEGIN
    {
        $TextBlob = @();
        $Script:FindFileResult = @();
    }
    PROCESS
    {
        $TextBlob += $Text;
    }
    END
    {
        function Get-Hay
        {
            if ($TextBlob)
            {
                return $TextBlob;
            }

            return Get-ChildItem -r -i $filePattern;
        }

        if ($Summarize)
        {
            (Get-ChildItem -r -i $filePattern |
              Select-String -CaseSensitive:$CaseSensitive -Context $Context $needlePattern) |
              Group-Object Path |
              Select-Object Name, Count, @{Expression={ $_.Group | foreach { $_.LineNumber} }; Name="Line Numbers"};
        }
        else
        {
            $regexPattern = $needlePattern
            if ($CaseSensitive -eq $false) { $regexPattern = "(?i)$regexPattern" }
            $regex = New-Object System.Text.RegularExpressions.Regex $regexPattern;

            function Print($path, $linenumber, $string, $regex)
            {
                $Script:FindFileResult += ,"${path}:${linenumber}:";

                if ($AsObject -eq $true)
                {
                    Write-Output (New-Object PSObject -Property ([ordered]@{
                                      'File' = (Get-Item $path);
                                      'Line' = $linenumber
                                  }));
                }
                elseif ($NoHighlight -eq $true)
                {
                    if ($path -ne 'InputStream')
                    {
                        Write-Output "$($path):$($linenumber): $string";
                    }
                    else
                    {
                        Write-Output $string;
                    }
                }
                else
                {
                    if ($path -ne 'InputStream')
                    {
                        Write-Host "$($path):$($linenumber): " -nonewline;
                    }

                    Write-HostAndHighlightPattern $string $regex;
                    Write-Host;
                }
            }

            Get-Hay |
              Select-String -CaseSensitive:$CaseSensitive -Context $Context $needlePattern |
              ForEach-Object -Begin { Write-Host  } `
              -Process {
                  if ($_.Context -eq $null)
                  {
                      Print $_.Path $_.LineNumber $_.Line $regex
                  }
                  else
                  {
                      $path = $_.Path
                      $linenumber = $_.LineNumber
                      $diff = $_.Context.DisplayPreContext.Length
                      $_.Context.DisplayPreContext | % {
                          Print $path ($linenumber - $diff) $_ $regex
                          $diff--
                      }
                      Print $_.Path $_.LineNumber $_.Line $regex
                      $_.Context.DisplayPostContext | % {
                          $diff++
                          Print $path ($linenumber + $diff) $_ $regex
                      }
                  }
              } `
                -End { Write-Host }
        }
    }
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
            & "C:\users\kohzi\tools\emacs\bin\emacsclientw.exe" '-n' '--no-wait' '--alternate-editor=`"C:\users\kohzi\tools\emacs\bin\runemacs.exe`"' "+$($matches[2]):0" $matches[1];
        }
    } catch {
    }
}

function Get-OrgImageLink {
    add-type -an system.windows.forms;
    $Script:OrgDirectory = 'C:\Users\kohzi\OneDrive\org';

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

function Invoke-MenuSelection
{
    [CmdletBinding()]
    param(
        [string[]]$Options
    )

    try
    {
        return Invoke-MenuSelectionCurses -Options $Options;
    }
    catch
    {
        return Invoke-MenuSelectionOption -Options $Options;
    }
}

function Invoke-MenuSelectionOption
{
    [CmdletBinding()]
    param(
        [string[]]$Options
    )

    for ($i = 0; $i -lt @($Options).Count; ++$i)
    {
        Write-Host "$($i): $($Options[$i])";
    }

    $good = $false;
    $sel = -1;
    while(-not $good)
    {
        $inp = Read-Host -Prompt 'Option';
        try
        {
            $sel = [int]::Parse($inp);
        }
        catch
        {
            $good = $false;
        }

        if ($sel -ge 0 -and $sel -lt @($Options).Count)
        {
            $good = $true;
        }
    }
    return $sel;
}

function Invoke-MenuSelectionCurses
{
    [CmdletBinding()]
    param(
        [string[]]$Options
    )

    $fgColor = [System.Console]::ForegroundColor;
    $bgColor = [System.Console]::BackgroundColor;
    $fgColorSel = $bgColor;
    $bgColorSel = $fgColor;

    $startLine = [System.Console]::CursorTop;
    $curSel = 0;
    $selText = '';

    $start = 1;
    $end = @($Options).Count;

    $tomin = [Math]::min([System.Console]::WindowHeight - 4, @($Options).Count);

    $newStartLine = $startLine;
    if ($startLine + $tomin + 2 -ge [System.Console]::BufferHeight)
    {
        $newStartLine = [System.Console]::BufferHeight - $tomin - 3;
    }

    do {
		[System.Console]::CursorTop = ($startLine);
        $totalHeight = [System.Console]::WindowHeight;
        $totalAvail = $totalHeight - 4;

        if ($curSel -ge $end)
        {
            $end = $curSel + 1;
            $start = [Math]::max($start, $end - $totalAvail);
        }
        elseif ($curSel -lt $start)
        {
            $start = $curSel;
            $end = [Math]::min($end, $start + $totalAvail);
        }

        for ($i = $start; $i -lt $end; ++$i)
        {
			[System.Console]::Write("`r");
            $line = "   $i`: $($Options[$i])";
            $line = $line.PadRight([System.Console]::WindowWidth - 1).SubString(0, [System.Console]::WindowWidth - 1);

            if ($curSel -eq $i)
            {
                [System.Console]::ForegroundColor = $fgColorSel;
                [System.Console]::BackgroundColor = $bgColorSel;
            }
            else
            {
                [System.Console]::ForegroundColor = $fgColor;
                [System.Console]::BackgroundColor = $bgColor;
            }
            [System.Console]::WriteLine($line);
        }

        [System.Console]::ForegroundColor = $fgColor;
        [System.Console]::BackgroundColor = $bgColor;

        [System.Console]::WriteLine("");
        $selTextTmp = $selText.PadRight([System.Console]::WindowWidth);
        $tmp = [System.Console]::CursorTop;
        [System.Console]::Write(" Option > $selTextTmp");

        if ($tmp + 1 -ge [System.Console]::BufferHeight)
        {
            [System.Console]::CursorTop = $tmp - 1;
        }
        else
        {
            [System.Console]::CursorTop = $tmp;
        }

        # Read Key
		$inputKey = [System.Console]::ReadKey($false);

        if ($inputKey.Key -eq [System.ConsoleKey]::DownArrow)
        {
            if ($curSel + 1 -lt @($Options).Count)
            {
                ++$curSel;
                $selText = $curSel.ToString();
            }
        }
        elseif ($inputKey.Key -eq [System.ConsoleKey]::UpArrow)
        {
            if ($curSel -gt 0)
            {
                --$curSel;
                $selText = $curSel.ToString();
            }
        }
        elseif ($inputKey.Key -eq [System.ConsoleKey]::PageDown)
        {
            $curSel += $totalAvail - 3;
            if ($curSel -ge @($Options).Count)
            {
                $curSel = @($Options).Count - 1;
            }
        }
        elseif ($inputKey.Key -eq [System.ConsoleKey]::PageUp)
        {
            $curSel -= $totalAvail - 3;
            if ($curSel -lt 0)
            {
                $curSel = 0;
            }
        }
        elseif ([System.Char]::IsDigit($inputKey.KeyChar))
        {
            $newText = $selText + [string]$inputKey.KeyChar;
            $newSel = [Int]::Parse($newText);
            if (($newSel -ge 0) -and ($newSel -lt @($Options).Count))
            {
                $curSel = $newSel;
                $selText = $curSel.ToString();
            }
            elseif ($newSel -ge @($Options).Count -and $selText -ne '')
            {
                # Attempt replacing last digit with input
                $newText = $selText.SubString(0, $selText.Length - 1) + [string]$inputKey.KeyChar;
                $newSel = [Int]::Parse($newText);

                if ($newSel -lt @($Options).Count)
                {
                    $curSel = $newSel;
                    $selText = $curSel.ToString();
                }
            }
        }
        elseif ($inputKey.Key -eq [System.ConsoleKey]::Backspace)
        {
            if ($selText.Length -gt 0)
            {
                $selText = $selText.SubString(0, $selText.Length - 1);
                if ($selText -eq '')
                {
                    $curSel = 0;
                }
                else
                {
                    $curSel = [Int]::Parse($selText);
                }
            }
        }

        $startLine = $newStartLine;
    } while ($inputKey.Key -ne [System.ConsoleKey]::Enter -and $inputKey.Key -ne [System.ConsoleKey]::Escape);

    [System.Console]::WriteLine("");

    if ($inputKey.Key -eq [System.ConsoleKey]::Escape)
    {
        return $null;
    }

    return $curSel;
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
    $path = ((pwd).Path);
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
        $Index = Invoke-MenuSelection $Script:CachedDirectories;
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

$Script:ScratchPath = 'E:\logs\scratch.txt';
function New-Scratch {
    [CmdletBinding()]
    Param(
 		[Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$data
    )
    if (Test-Path $Script:ScratchPath)
    {
        $scratch = (gc $Script:ScratchPath) | ConvertFrom-JSON;
        $scratch += ,$data;
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
    $sel = Invoke-MenuSelection $scratch;
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
            $objHash[$o.$Identifier] = @{
                'Identifier' = $o.$Identifier;
                'Object' = $o;
                'Children' = @();
            };
        }

        foreach ($o in $objects)
        {
            $key = $o.$ParentKey;
            if ($key -and $objHash.ContainsKey($key))
            {
                $parent = $objHash[$key];
                $parent.Children += ,($objHash[$o.$Identifier]);
            }
            else
            {
                $tr += ,($objHash[$o.$Identifier]);
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
        for ($j = 0; $j -lt $objects.Count; ++$j)
        {
            $t = $objects[$j];
            $isLast = $j -eq ($objects.Count - 1);

            for ($i = 0; $i -lt $Depth - 1; ++$i)
            {
                if ($LastArr[$i + 1])
                {
                    Write-HostOutput (' ' * $Spacing) -NoNewLine;
                }
                else
                {
                    Write-HostOutput '|' -NoNewLine;
                    Write-HostOutput (' ' * ($Spacing - 1)) -NoNewLine;
                }
            }
            if ($Depth -gt 0)
            {
                Write-HostOutput '|' -NoNewLine;
                Write-HostOutput ('-' * ($Spacing - 1)) -NoNewLine;
            }

            if ($Identifier.GetType().Name -eq 'ScriptBlock')
            {
                $toPrint = $t.Object | % $Identifier;
                Write-HostOutput $toPrint;
            }
            elseif ($Identifier.GetType().Name -eq 'String' -and $Identifier)
            {
                Write-HostOutput $t.Object.$Identifier;
            }
            else
            {
                Write-HostOutput $t.Identifier;
            }

            $t.Children | Out-Tree -Identifier $Identifier -Depth ($Depth + 1) -Spacing $Spacing -LastArr ($LastArr + $isLast);
        }
    }
}

function ConvertFrom-TableString {
    param(
        [Parameter(Mandatory=$false, ValueFromPipeline=$true)]
        [String]$String
    )
    begin
    {
        $Table = '';
    }
    process
    {
        if ($Table)
        {
            $Table = "$Table`n$String";
        }
        else
        {
            $Table = $String;
        }
    }
    end
    {
        #
        # Table headers will be on the first row.
        #

        $lines = $Table -split '\n'
        $headerLine = $lines[0];
        $headers = ($headerLine -split '\s+') | ? {$_};

        if (@($headers | Select -Unique).Count -ne $headers.Count)
        {
            Write-Error 'Headers [$($headers -join ',')] are not unique';
            return;
        }

        $headerIndex = @();
        for ($j = 0; $j -lt $headers.Count; ++$j)
        {
            $h = $headers[$j];
            $index = $headerLine.IndexOf($h + ' ');
            if ($index -eq -1)
            {
                $index = $headerLine.IndexOf(' ' + $h) + 1;
            }
            $headerIndex += ,$index;
        }

        #
        # Extract objects from indices based off headers.
        #

        for ($i = 1; $i -lt $lines.Count; ++$i)
        {
            $obj = [ordered]@{};
            $line = $lines[$i];
            if (-not $line.trim())
            {
                continue;
            }

            for ($j = 0; $j -lt $headers.Count; ++$j)
            {
                $startIndex = $headerIndex[$j];
                if ($j -eq $headers.Count - 1)
                {
                    $endIndex = $line.Length;
                }
                else
                {
                    $endIndex = $headerIndex[$j + 1];
                }
                $obj[$headers[$j]] = $line.SubString($startIndex, $endIndex - $startIndex ).trim();
            }

            Write-Output (New-Object PSObject -Property $obj);
        }
    };
}

function Then {
    param(
        [Parameter(Position=1, Mandatory=$true, ValueFromPipeline=$true)]
        [ScriptBlock]$Trigger,

        [Parameter(Position=0, Mandatory=$false)]
        [ScriptBlock]$Action
    )

    while ($true)
    {
        $res = & $Trigger;
        if ($res)
        {
            break;
        }
        Start-Sleep 5;
    }

    if (-not $Action)
    {
        Show-Notification -Type 'Info' -Text $Trigger.ToString() -Title 'Task Completed';
    }
    else
    {
        $res | % $Action;
    }
}
