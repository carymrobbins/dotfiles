$LastPathFile = "$env:USERPROFILE\.lastpath"

# Restore last directory on startup
if (Test-Path $LastPathFile) {
    $LastPath = Get-Content $LastPathFile -ErrorAction SilentlyContinue
    if ($LastPath -and (Test-Path $LastPath)) {
        Set-Location $LastPath
    }
}

# Save whenever location changes (works with cd, pushd, popd, Set-Location, etc.)
$ExecutionContext.InvokeCommand.LocationChangedAction = {
    $PWD.Path | Out-File "$env:USERPROFILE\.lastpath" -ErrorAction SilentlyContinue
}

# Save on exit as backup
Register-EngineEvent PowerShell.Exiting -Action {
    $PWD.Path | Out-File "$env:USERPROFILE\.lastpath"
} | Out-Null
