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
