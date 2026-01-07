eval "$(starship init zsh)"

# History configuration
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS

# Enable interactive comments
setopt interactivecomments
# Disable zsh-specific globbing
unsetopt nomatch

# Keybindings - use emacs mode
bindkey -e

alias g=git
alias v='$EDITOR'
alias sv='sudoedit'
alias rm=trash

ssh-add-all() {
    # Get a list of fingerprints of all currently loaded keys
    local -a loaded_keys
    loaded_keys=(${(f)"$(ssh-add -l 2>&1 | awk '{print $2}')"})

    # Loop through all public key files (~/.ssh/id_*.pub)
    for pub_key in ~/.ssh/id_*.pub(N); do
        # Derive the corresponding private key path by removing .pub
        local private_key="${pub_key%.pub}"

        # Check if the private key exists
        if [[ ! -f "$private_key" ]]; then
            echo "Private key for $(basename "$pub_key") not found. Skipping..." >&2
            continue
        fi

        # Compute the fingerprint of the public key
        local key_fingerprint=$(ssh-keygen -lf "$pub_key" | awk '{print $2}')

        # Check if the fingerprint is already in the list of loaded keys
        if (( ! ${loaded_keys[(I)$key_fingerprint]} )); then
            ssh-add "$private_key"
        fi
    done
}

ssh-add-all
