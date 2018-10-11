# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Load ~/.env, if it exists.
[ ! -f "$HOME/.env" ] || source "$HOME/.env"

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="carymrobbins"

# Enable 256 colors
export TERM=xterm-256color

# Disable virtualenv prompt
export VIRTUAL_ENV_DISABLE_PROMPT=1

# ZSH disabled features
# The 'r' command isn't really used
disable r

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias g=git
alias c='curl -sS'
alias v='$EDITOR'
alias sv='sudoedit'
alias ssh-add-all="ssh-add ~/.ssh/*_rsa"
alias zsv='v ~/.zshrc'
alias sca='bash -c '"'"'(cd ~/dump/scaling ; sbt "$@" consoleQuick)'"'"' sca'
alias rm=trash

# Load a virtualenv
vnv() {
  if [ $# -ne 1 ]; then
    >&2 echo "$0: Expected exactly 1 argument, got: $#"
    return 1
  fi
  vpath=$HOME/.pyenv/$1
  if [ ! -f "$vpath/bin/python" ]; then
    >&2 echo "$0: virtualenv does not exist: $vpath"
  fi
  echo $1 > "$HOME/.pyenv/current-env"
  source "$vpath/bin/activate"
}

unvn() {
  rm "$HOME/.pyenv/current-env"
  deactivate
}

ipy() {
  if [ -z "$VIRTUAL_ENV" ]; then
    ipython
  else
    "$VIRTUAL_ENV/bin/ipython"
  fi
}

# Auto-load virtualenv, if current one exists
# if [ -f "$HOME/.pyenv/current-env" ]; then
#   vnv "$(cat "$HOME/.pyenv/current-env")"
# fi

# Conditionally use ./gradlew if it exists
# Setting TERM to workaround gradle 4.5.1 bug
# See https://github.com/gradle/gradle/issues/4426
gr() {
  TERM=xterm-color $(if [ -f ./gradlew ]; then echo ./gradlew; else echo gradle; fi) "$@"
}

wo() {
  if [ -z "$1" ]; then
    >&2 echo "Usage: wo <project>"
    return
  fi

  local found=$(find-project-dir "$1")
  if [ "$?" -eq 0 ]; then
    cd "$found"
  fi
}

_vbinfile() {
  local filepath=$1/$2
  shift
  shift
  if [ ! -f "$filepath" ]; then
    echo "Initializing $filepath as a bash script..."
    echo -e "#!/bin/bash\n\n" > "$filepath"
    chmod +x "$filepath"
  fi
  "$EDITOR" "$filepath"
}
alias vsb='_vbinfile $HOME/dotfiles/shared/bin'
alias vlb='_vbinfile $HOME/dotfiles/linux/bin'
alias vmb='_vbinfile $HOME/dotfiles/mac/bin'

log_implicits='set scalacOptions in Global += "-Xlog-implicits"'

# DISABLED AS THIS USES TOO MUCH MEMORY
# SBT fails for permgen, this should work for Java >= 8
# See https://github.com/sbt/sbt/issues/1395
# export SBT_OPTS="-XX:+CMSClassUnloadingEnabled -XX:MaxMetaspaceSize=512M -XX:MetaspaceSize=256M -Xms2G -Xmx2G"

# Helper functions

# Run --help | less
hl() { "$@" --help 2>&1 | less;  }

# Run bash's help function
help() { bash -c "help $(printf '%q ' "$@")"; }

if [ "$(uname -s)" = "Linux" ]; then
  # Linux-specific aliases
  alias xmv='v ~/.xmonad/xmonad.hs'
  alias xmc='xmonad --recompile && cat ~/.xmonad/xmonad.errors'
  alias sc='systemctl'
  alias ssc='sudo systemctl'
  alias scu='systemctl --user'
elif [ "$(uname -s)" = "Darwin" ]; then
  # Mac-specific aliases
  for c in java javac; do alias "${c}7"='$(/usr/libexec/java_home -v 1.7)/bin/'$c; done
  for c in java javac; do alias "${c}8"='$(/usr/libexec/java_home -v 1.8)/bin/'$c; done
fi

ref_exec() {
  local c=$1
  shift
  if ! command -v "$@" >/dev/null; then
    >&2 echo "Not found"
    return 1
  fi
  $c $(command -v "$@")
}

cx() {
  if command -v vimcat >/dev/null; then
    ref_exec vimcat "$@";
  else
    ref_exec cat "$@";
  fi
}

vx() { ref_exec "$EDITOR" "$1"; }


# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git last-working-dir path ssh-agent titles)

# Mac-specific plugins
if [ "$(uname -s)" = "Darwin" ]; then
  plugins+=(brew)
fi

source $ZSH/oh-my-zsh.sh

# User configuration

# Use ~/.path to manage the PATH variable.  Each path should be on a new line.

# export MANPATH="/usr/local/man:$MANPATH"

# Prevent `git status` on every prompt (fixes slow prompt).
git_prompt_info() {
  git rev-parse >/dev/null 2>&1 || return
  ref=$(git symbolic-ref -q --short HEAD || (printf "detached @ " && git name-rev --name-only HEAD))
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}${ZSH_THEME_GIT_PROMPT_CLEAN}${ZSH_THEME_GIT_PROMPT_SUFFIX}"
}

venv_prompt_info() {
  [ -n "$VIRTUAL_ENV" ] || return
  local name=$(basename "$VIRTUAL_ENV")
  echo "${ZSH_THEME_VENV_PROMPT_PREFIX}${name}${ZSH_THEME_VENV_PROMPT_SUFFIX} "
}

# Allow end-of-line comments (i.e. `echo foo # bar` should echo "foo", not "foo # bar")
setopt interactivecomments

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

if command -v nvim >/dev/null; then
  export EDITOR='nvim'
else
  export EDITOR='vim'
fi

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# The aws plugin gives strange python errors, so manually adding the completion
if command -v aws_zsh_completer.sh >/dev/null; then
  compctl -K aws_profiles asp
  source aws_zsh_completer.sh
fi

# Add all of our ssh keys to the agent.
for key in $(find ~/.ssh -name '*_rsa'); do
  if ! ssh-add -l | fgrep "$key" >/dev/null; then
    ssh-add "$key"
  fi
done

# NOTE: Let's not auto-enable tmux, it ends up being weird when you open
# multiple terminals.
#
# Attempt to start a tmux session if we're not in a TTY and not already in tmux.
# If we can't, don't error, just warn.
# if [ -n "$DISPLAY" -a -z "$TMUX" ] && command -v tmux >/dev/null; then
#   tmux a 2>/dev/null || tmux
# fi

# Custom completions
autoload -Uz compinit
compinit
# _path_commands autocompletes executables on the $PATH
compdef _path_commands hl
compdef _path_commands cx
compdef _path_commands vx

_find_project_dir_completions() {
  find-project-dir "$1" --complete | while read line
  do
    compadd "$line"
  done
}
compdef _find_project_dir_completions find-project-dir
compdef _find_project_dir_completions wo

_tau_completions() {
  # Not included 'track' since it's more of an internal command which
  # shouldn't be invoked manually by a user.
  compadd 'active'
  compadd 'view'
  compadd 'timecard'
}
compdef _tau_completions tau

_vnv_completions() {
  for x in $(find $HOME/.pyenv/* -maxdepth 0 -type d -exec basename {} \;); do
    compadd "$x"
  done
}
compdef _vnv_completions vnv
