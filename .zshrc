# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="carymrobbins"

# Enable 256 colors
export TERM=xterm-256color

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias wo="source ~/bin/workon"
alias g=git
alias v='$EDITOR'
alias lock="gnome-screensaver-command -l"
alias ssh-add-all="ssh-add ~/.ssh/*_rsa"
alias xmv='v ~/.xmonad/xmonad.hs'
alias xmc='xmonad --recompile && cat ~/.xmonad/xmonad.errors'

hl() {
  "$@" --help | less
}

# Mac-specific aliases
if [ "$(uname -s)" = "Darwin" ]; then
  # kwm helpers
  alias vk="vim ~/.kwm/kwmrc"
  alias kr="kwmc config reload"

  alias bs="brew services"

  # Java helpers
  for c in java javac; do alias "${c}7"='$(/usr/libexec/java_home -v 1.7)/bin/'$c; done
  for c in java javac; do alias "${c}8"='$(/usr/libexec/java_home -v 1.8)/bin/'$c; done
fi

ref_exec() {
  local c=$1
  shift
  local loc=$(command -v "$@")
  if [ $? -ne 0 ]; then
    >&2 echo "Not found"
    return 1
  fi
  $c $loc
}

cx() { ref_exec cat "$@"; }

vx() { ref_exec "$EDITOR" "$1"; }

vcx() { ref_exec vimcat "$1"; }

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
plugins=(git last-working-dir path ssh-agent)

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

# Attempt to start a tmux session. If we can't, don't error, just warn.
# [ -n "$TMUX" ] || tmux a || tmux || >&2 echo "Warning, could not start a tmux session"
