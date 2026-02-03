eval "$(starship init zsh)"

source "$HOME/.env"

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

: ${EDITOR:=vim}
alias g=git
alias v='$EDITOR'
alias sv='sudoedit'
alias rm=trash

# Ensure ssh-agent is running and keys are loaded only if missing
function maybe_start_ssh_agent() {
  if ! pgrep -u "$USER" ssh-agent >/dev/null 2>&1; then
    # Start ssh-agent if not running; output to ~/.ssh/agent.env
    eval "$(ssh-agent -s)" >| ~/.ssh/agent.env
  fi
  # Source the agent env (works even if agent already running)
  [[ -f ~/.ssh/agent.env ]] && source ~/.ssh/agent.env > /dev/null
}

function ssh-add-if-needed() {
  # Add all id_* keys if none are loaded or any are missing
  local pub_keys
  pub_keys=(~/.ssh/id_*.pub(N))
  if [[ ${#pub_keys} -eq 0 ]]; then
    return
  fi

  local loaded
  loaded="$(ssh-add -l 2>/dev/null | grep -v "The agent has no identities")"
  for pub_key in $pub_keys; do
    local priv_key="${pub_key%.pub}"
    # Only attempt if there's a matching private key
    [[ -f $priv_key ]] || continue

    # If the public key's fingerprint isn't in ssh-add -l, add it
    local fp; fp="$(ssh-keygen -lf "$pub_key" | awk '{print $2}')"
    if ! ssh-add -l 2>/dev/null | grep -q "$fp"; then
      ssh-add "$priv_key" >/dev/null 2>&1
    fi
  done
}

maybe_start_ssh_agent
ssh-add-if-needed

reload-path() {
    if [ "$CMR_CUSTOM_PATH" ]; then
        # Determine what the original path was before we modified it be removing
        # our CMR_CUSTOM_PATH.  This way we don't double-up entries on the path when
        # manually calling reload_path.
        export CMR_ORIGINAL_PATH=$(echo $PATH | sed "s|$CMR_CUSTOM_PATH||")
    else
        export CMR_ORIGINAL_PATH=$PATH
    fi
    # Evaluate parts of the path line by line from ~/.path
    # expanding $variables and globs (*)
    export CMR_CUSTOM_PATH=$(
      while read x; do
        eval echo -n "$x"
        echo -n ':'
      done < "$HOME/.path"
    )
    # CMR_CUSTOM_PATH ends with a trailing colon (:) so no need to provide it here.
    export PATH=${CMR_CUSTOM_PATH}${CMR_ORIGINAL_PATH}
}

reload-path

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
