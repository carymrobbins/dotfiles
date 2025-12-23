# Add the following to your $PROFILE to load this script at startup:
# . $HOME/.pwshrc.ps1

Invoke-Expression (&starship init powershell)

Set-Alias -Name g -Value git
Set-Alias -Name v -Value vim

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

# Function to auto-add SSH keys from ~/.ssh based on associated public keys
function Add-SshKeys {
    # Get all public key files (~/.ssh/id_*.pub)
    $publicKeys = Get-ChildItem -Path "$HOME\.ssh\id_*.pub" -File

    # Get a list of fingerprints of all currently loaded keys
    $loadedKeys = ssh-add -l 2>&1 | ForEach-Object { ($_ -split ' ')[1] } # Extract only the fingerprint

    foreach ($pubKey in $publicKeys) {
        # Derive the corresponding private key path by removing .pub
        $privateKeyPath = $pubKey.FullName -replace '\.pub$'

        # Check if the private key exists
        if (-Not (Test-Path $privateKeyPath)) {
            Write-Host "Private key for $($pubKey.Name) not found. Skipping..." -ForegroundColor Yellow
            continue
        }

        # Compute the fingerprint of the corresponding private key
        $keyFingerprint = ssh-keygen -lf $pubKey.FullName | ForEach-Object { ($_ -split ' ')[1] } # Extract the fingerprint

        # Check if the fingerprint is already in the list of loaded keys
        if (-Not ($loadedKeys -contains $keyFingerprint)) {
            ssh-add $privateKeyPath
        }
    }
}

# Automatically call the function to add keys on terminal start
Add-SshKeys

# May require you to install PSReadLine first:
# > Install-Module -Name PSReadLine -Force -Scope CurrentUser
Set-PSReadLineOption -EditMode Emacs
