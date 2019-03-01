function Select-Interactive {
    [CmdletBinding()]
    param(
        [Parameter(Position=0,
                   Mandatory=$false,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true)]
        [String]
        $String
    )
    begin
    {
        $Strings = @();
    }
    process
    {
        $strSplit = $String -split '\n';
        $Strings += @($strSplit);
    }
    end
    {
        try {
            Start-FilterInteractive $Strings;
        } catch
        {
            write-host $_.ScriptStackTrace;
        }
    }
}

function Start-FilterInteractive {
    param(
        [String[]]$Strings
    )

    $rui = (Get-Host).UI.RawUI;

    $query = '';
    $startIndex = 0;

    $matched = @(0..@($Strings).Count);
    $prevCurIndex = 0;
    $curIndex = 0;
    $matchCurIndex = 0;

    $fgColor = [System.Console]::ForegroundColor;
    $bgColor = [System.Console]::BackgroundColor;
    $fgColorSel = $bgColor;
    $bgColorSel = $fgColor;

    do
    {
        $width = $rui.WindowSize.Width;
        $height = $rui.WindowSize.Height;

        #
        # Render Interactive UI.
        #
        [System.Console]::ForegroundColor = $fgColor;
        [System.Console]::BackgroundColor = $bgColor;
        Clear-Host;
        $tempQuery = Get-FiSuffix -String $query -Length ($width - 13);
        Write-Host "> $tempQuery";

		[System.Console]::CursorTop = ($startLine);
		[System.Console]::CursorLeft = $width - 10;
        Write-Host "[$($matchCurIndex + 1)/$(@($matched).Count)]";

        $endIndex = $startIndex + $height - 4;
        if ($curIndex -ge 0 -and $curIndex -lt $startIndex)
        {
            $startIndex = $curIndex;
            $endIndex = $startIndex + $height - 4;
        }
        elseif ($curIndex -gt $endIndex)
        {
            $endIndex = $curIndex;
            $startIndex = $endIndex - $height + 4;
        }
        if ($endIndex -ge @($Strings).Count)
        {
            $endIndex = @($Strings).Count - 1;
        }

        for ($i = $startIndex; $i -le $endIndex; ++$i)
        {
            if ($curIndex -eq $i)
            {
                [System.Console]::ForegroundColor = $fgColorSel;
                [System.Console]::BackgroundColor = $bgColorSel;
            }
            else
            {
                [System.Console]::ForegroundColor = $fgColor;
                [System.Console]::BackgroundColor = $bgColor;
            }
            Write-FiHostAndHighlightPattern -String $Strings[$i] -Regex $query -Length $width;
            Write-Host;
        }
        [System.Console]::ForegroundColor = $fgColor;
        [System.Console]::BackgroundColor = $bgColor;

        $matchedChange = $false;
        do
        {
            $modified = $true;

            [System.Console]::TreatControlCAsInput = $true
		    $inputKey = [System.Console]::ReadKey($true);
            $KeyString = $inputKey.Key.ToString()
            if ($inputKey.Modifiers -ne 0)
            {
                $m = $inputKey.Modifiers.ToString() -replace ', ', '+';
                $KeyString = "${m}+${KeyString}";
            }

            if ($inputKey.Key -eq [System.ConsoleKey]::DownArrow)
            {
                if ($matchCurIndex -ge 0 -and $matchCurIndex -lt @($matched).Count - 1)
                {
                    ++$matchCurIndex;
                    $curIndex = $matched[$matchCurIndex];
                }
            }
            elseif ($inputKey.Key -eq [System.ConsoleKey]::UpArrow)
            {
                if ($matchCurIndex -gt 0)
                {
                    --$matchCurIndex;
                    $curIndex = $matched[$matchCurIndex];
                }
            }
            elseif ($KeyString -eq 'Alt+C')
            {
                if ($curIndex -gt 0)
                {
                    $Strings[$curIndex] | clip;
                }
            }
            elseif ($KeyString -eq 'Control+C' -or $KeyString -eq 'Enter')
            {
                return;
            }
            elseif ($KeyString -eq 'Escape')
            {
                if ($query -eq '')
                {
                    return;
                }
                else
                {
                    $query = '';
                    $matched = Invoke-FiQuery -Hay $Strings -RegEx $query;
                    $matchedChange = $true;
                }
            }
            elseif ($KeyString -eq 'Backspace')
            {
                if ($query.Length)
                {
                    $query = $query.SubString(0, $query.Length - 1);
                    $matched = Invoke-FiQuery -Hay $Strings -RegEx $query;
                    $matchedChange = $true;
                }
            }
            elseif ($inputKey.KeyChar)
            {
                $query = $query + $inputKey.KeyChar;
                $matched = Invoke-FiQuery -Hay $Strings -RegEx $query -PreviousMatches $matched;
                $matchedChange = $true;
            }
            else
            {
                $modified = $false;
            }

        } while(-not $modified);

        if ($matchedChange)
        {
            if (@($matched).Count -eq 0)
            {
                if ($curIndex -ne -1)
                {
                    $prevCurIndex = $curIndex;
                }
                $curIndex = -1;
                $matchCurIndex = -1;
            }
            else
            {
                #
                # Find closest curIndex.
                #
                if ($curIndex -eq -1)
                {
                    $curIndex = $prevCurIndex;
                }
                $found = $false;
                for ($i = 0; $i -lt @($matched).Count; ++$i)
                {
                    if ($matched[$i] -ge $curIndex)
                    {
                        $matchCurIndex = $i;
                        $curIndex = $matched[$i];
                        $found = $true;
                        break;
                    }
                }

                if (-not $found)
                {
                    $matchCurIndex = @($matched).Count - 1;
                    $curIndex = $matched[$matchCurIndex];
                }
            }
        }

    } while($true);
}

function Invoke-FiQuery
{
    param(
        [String[]]$Hay,
        [String]$RegEx,
        [int[]]$PreviousMatches
    )

    if (-not $PreviousMatches)
    {
        $PreviousMatches = @(0..(@($Hay).Count - 1));
    }

    trap {
        return @();
    }

    $rx = [RegEx]::new($RegEx, [System.Text.RegularExpressions.RegexOptions]::IgnoreCase);

    $tr = @();
    for ($i = 0; $i -lt $PreviousMatches.Count; ++$i)
    {
        if ($Hay[$PreviousMatches[$i]] -match $rx)
        {
            $tr += $PreviousMatches[$i];
        }
    }

    return $tr;
}

function Get-FiSuffix
{
    param(
        $String,
        $Length
    )

    if ($String.Length -le $Length)
    {
        return $String;
    }

    return $String.Substring($String.Length - $Length, $Length);
}

function Write-FiHostAndHighlightPattern(
    [string]$String,
    [String]$Regex,
    $Length
)
{
    try
    {
        $rx = [RegEx]::new($RegEx, [System.Text.RegularExpressions.RegexOptions]::IgnoreCase);
    }
    catch
    {
        $rx = [RegEx]::new('');
    }

	$index = 0;
	while($index -lt $String.Length -and $Length -gt 0)
	{
		$match = $rx.Match($String, $index);
		if ($match.Success -and $match.Length -gt 0)
		{
            $tp = $String.SubString($index, $match.Index - $index);
            if ($tp.Length -le $Length)
            {
			    Write-Host $tp -nonewline;
            }
            else
            {
                Write-Host $tp.SubString(0, $Length) -nonewline;
            }
            $Length -= $tp.Length;

            if ($Length -gt 0)
            {
                $tp = $match.Value.ToString();
                if ($tp.Length -le $Length)
                {
			        Write-Host $tp -nonewline -ForegroundColor Blue;
                }
                else
                {
                    Write-Host $tp.SubString(0, $Length) -nonewline -ForegroundColor Blue;
                }
                $Length -= $tp.Length;
            }

			$index = $match.Index + $match.Length
		}
		else
		{
            $tp = $String.SubString($index);
            if ($tp.Length -le $Length)
            {
			    Write-Host $tp -nonewline;
            }
            else
            {
                Write-Host $tp.SubString(0, $Length) -nonewline;
            }
            $Length -= $tp.Length;

			$index = $String.Length;
		}
	}
}
