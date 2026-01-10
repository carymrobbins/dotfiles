# POSIX-style `touch`
function touch {
    param ([string]$Path)
    if (Test-Path $Path) {
        (Get-Item $Path).LastWriteTime = Get-Date
    } else {
        New-Item -ItemType File -Path $Path | Out-Null
    }
}

# POSIX-style `ln -s`
function ln {
    param (
        [Parameter(Mandatory = $true, Position = 0)]
        [string]$Source,

        [Parameter(Mandatory = $true, Position = 1)]
        [string]$Target,

        [switch]$s  # Support for -s flag (symbolic link)
    )

    if (-not $s) {
        Write-Host "Usage: ln -s TARGET LINK_NAME" -ForegroundColor Yellow
        return
    }

    try {
        # Create a symbolic link using New-Item
        New-Item -ItemType SymbolicLink -Path $Target -Target $Source
        Write-Host "Symlink created: $Target -> $Source" -ForegroundColor Green
    } catch {
        Write-Host "Failed to create symlink: $($_.Exception.Message)" -ForegroundColor Red
        return
    }
}


# POSIX-style `which`
function which($name) {
      Get-Command $name -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Source
}

