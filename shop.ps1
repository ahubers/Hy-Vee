<#
.SYNOPSIS
    Opens Firefox for base URL combined with unique ingredients read from the "# Ingredients" 
    section of command-line specified text files.
.EXAMPLE
    .\script.ps1 groceries.recipe
.EXAMPLE
    .\script.ps1 recipe1.recipe recipe2.recipe
#>

param(
    [Parameter(Position=0, ValueFromRemainingArguments=$true)]
    [string[]]$FilePaths = @("recipes/staples.recipe")
)

# Define the base URL
$baseUrl = "https://www.hy-vee.com/aisles-online/search?search="

# Filter for files that actually exist
$existingFiles = $FilePaths | Where-Object { Test-Path $_ }

if ($existingFiles) {
    $allProcessedItems = [System.Collections.Generic.List[string]]::new()

    foreach ($file in $existingFiles) {
        $isCollecting = $false
        
        # Read file line by line to track sections
        foreach ($rawLine in Get-Content -Path $file) {
            $trimmed = $rawLine.Trim()

            # Check if we hit a Markdown header (e.g., # Ingredients, # Comments)
            if ($trimmed.StartsWith('#')) {
                # Extract header text, lowercase it, and remove leading '#' symbols and spaces
                $headerText = $trimmed.TrimStart('#').Trim().ToLower()
                
                # Turn collection ON if it's the Ingredients section, OFF for any other header
                $isCollecting = ($headerText -eq 'ingredients')
                continue
            }

            # If we are inside the Ingredients section, process valid lines
            if ($isCollecting) {
                # Ignore empty lines or strikethroughs (~~)
                if ([string]::IsNullOrWhiteSpace($trimmed) -or $trimmed.StartsWith('~~')) {
                    continue
                }

                # 1. Strip annotations starting with '|' through the end of the line
                $cleanItem = ($rawLine -replace '\|.*$', '').Trim()

                # 2. Remove bracketed text [...] e.g. [1 oz]
                $cleanItem = ($cleanItem -replace '\[.*?\]', '').Trim()
                
                # 3. Convert remaining spaces to +
                if (-not [string]::IsNullOrWhiteSpace($cleanItem)) {
                    $suffix = $cleanItem.Replace(' ', '+')
                    $allProcessedItems.Add($suffix)
                }
            }
        }
    }

    # Deduplicate across all processed files
    $uniqueSuffixes = $allProcessedItems | Select-Object -Unique

    if ($uniqueSuffixes) {
        foreach ($suffix in $uniqueSuffixes) {
            $fullUrl = "$baseUrl$suffix"
            Write-Host "Opening: $fullUrl"
            
            # Launch Firefox with the concatenated URL
            Start-Process "firefox.exe" -ArgumentList $fullUrl
            
            # Short delay to prevent opening all tabs at once
            Start-Sleep -Milliseconds 500
        }
    } else {
        Write-Host "No ingredients found under any '# Ingredients' headers in the specified files."
    }

} else {
    Write-Error "None of the specified files were found: $($FilePaths -join ', ')"
}