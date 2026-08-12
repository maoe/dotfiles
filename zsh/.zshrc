# users generic .zshrc file for zsh(1)

## Default shell configuration
#
# set prompt
#
autoload colors
colors
setopt prompt_subst
unsetopt promptcr

# auto change directory
#
setopt auto_cd

# auto directory pushd that you can get dirs list by cd -[tab]
#
setopt auto_pushd

# command correct edition before each completion attempt
#
setopt correct

# compacked complete list display
#
setopt list_packed

# no remove postfix slash of command line
#
setopt noautoremoveslash

# no beep sound when complete list displayed
#
setopt nolistbeep

# comments begin with a #
#
setopt interactivecomments

## Keybind configuration
#
# emacs like keybind (e.x. Ctrl-a goes to head of a line and Ctrl-e goes
# to end of it)
#
bindkey -e

# historical backward/forward search with linehead string binded to ^P/^N
#
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end

## Command history configuration
#
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups # ignore duplication command history list
setopt share_history # share command history data
setopt extended_history
setopt append_history

## Completion configuration
#
if [ -d "$HOME/.zfunc" ]; then
  fpath+=~/.zfunc
fi

if command -v rustc > /dev/null 2>&1 && [ -d "$(rustc --print sysroot)/share/zsh/site-functions" ]; then
  fpath+="$(rustc --print sysroot)/share/zsh/site-functions"
fi

# Homebrew-installed completions (_gh, _ghq, _pass, _rg, _delta, ...).
# Must come before compinit, and the prefix differs per arch (/usr/local vs /opt/homebrew).
if command -v brew > /dev/null 2>&1; then
  brew_prefix=$(brew --prefix)
  [ -d "$brew_prefix/share/zsh/site-functions" ] && fpath=("$brew_prefix/share/zsh/site-functions" $fpath)
  [ -d "$brew_prefix/share/zsh-completions" ] && fpath=("$brew_prefix/share/zsh-completions" $fpath)
  unset brew_prefix
fi

autoload -U compinit
compinit

## Alias configuration
#
# expand aliases before completing
#
setopt complete_aliases # aliased ls needs if file/dir completions work

alias where="command -v"

case "${OSTYPE}" in
freebsd*|darwin*)
  alias ls="ls -G -w"
  ;;
linux*)
  alias ls="ls --color"
  ;;
esac

alias la="ls -a"
alias lf="ls -F"
alias ll="ls -l"

alias du="du -h"
alias df="df -h"

alias su="su -l"

## terminal configuration
#
unset LSCOLORS
case "${TERM}" in
xterm)
  export TERM=xterm-color
  ;;
esac

# set terminal title including current directory
#
case "${TERM}" in
xterm*|screen*)
  precmd() {
    echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
  }
  export LSCOLORS=exfxcxdxbxegedabagacad
  export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
  zstyle ':completion:*' list-colors \
    'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
  zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' \
      'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
  ;;
esac

# Reset terminal modes left enabled by a remote TUI.
#
# When an ssh/mosh session dies abruptly (broken pipe, "closed by remote host"),
# the remote full-screen app never gets to send its cleanup sequences, so the
# local terminal stays in mouse-reporting mode and floods the shell with
# \e[<35;...M reports on every mouse move. Disable the modes ourselves on exit:
# mouse tracking (1000/1002/1003), SGR coordinates (1006), bracketed paste
# (2004), and force the cursor visible again (25h).
_reset_terminal_modes() {
  [[ -t 1 ]] && printf '\e[?1000l\e[?1002l\e[?1003l\e[?1006l\e[?2004l\e[?25h'
}

ssh() {
  command ssh "$@"
  local ret=$?
  _reset_terminal_modes
  return $ret
}

mosh() {
  command mosh "$@"
  local ret=$?
  _reset_terminal_modes
  return $ret
}

export PATH="/usr/local/opt/icu4c/bin:$PATH"
export PATH="/usr/local/opt/icu4c/sbin:$PATH"
export PATH="/usr/local/opt/gpg-agent/bin:$PATH"

# OPAM configuration
[ -f "$HOME/.opam/opam-init/init.zsh" ] && . "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null
# export PATH="/usr/local/opt/llvm/bin:$PATH"

## load user .zshrc configuration file
#
[ -f ~/.zshrc.mine ] && source ~/.zshrc.mine
[ -f ~/.zshrc.private ] && source ~/.zshrc.private || true

autoload -Uz is-at-least
if command -v starship > /dev/null && is-at-least 5.3; then
  eval "$(starship init zsh)"
fi

# added by Nix installer
if [ -e /home/maoe/.nix-profile/etc/profile.d/nix.sh ]; then
  . /home/maoe/.nix-profile/etc/profile.d/nix.sh;
fi


[ -f "/home/maoe/.ghcup/env" ] && . "/home/maoe/.ghcup/env" # ghcup-env