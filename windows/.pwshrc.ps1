# Add the following to your $PROFILE to load this script at startup:
# . $HOME/.pwshrc.ps1
#
# If you run into issues with modules not loading, be sure to run:
# . $HOME/dotfiles/windows/pwsh/setup.ps1

Invoke-Expression (&starship init powershell)

. "$HOME/dotfiles/windows/pwsh/aliases.ps1"
. "$HOME/dotfiles/windows/pwsh/cd.ps1"
. "$HOME/dotfiles/windows/pwsh/posix.ps1"
. "$HOME/dotfiles/windows/pwsh/ssh.ps1"

Import-Module posh-git
Set-PSReadLineOption -EditMode Emacs

# Automatically add ssh keys on powershell startup; from ssh.ps1
Add-SshKeys
