################################################################
################################################################
##                                                            ##
##              _               _                             ##
##             | |__   __ _ ___| |__  _ __ ___                ##
##             | '_ \ / _` / __| '_ \| '__/ __|               ##
##             | |_) | (_| \__ \ | | | | | (__                ##
##             |_.__/ \__,_|___/_| |_|_|  \___|               ##
##                                                            ##
##                                                            ##
################################################################
################################################################

PATH="$HOME/.cargo/bin${PATH:+:${PATH}}"
PATH="/snap/bin${PATH:+:${PATH}}"

export XDG_CONFIG_HOME=$HOME/.config

[ -f "~/.ghcup/env" ] && source "~/.ghcup/env"


# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

alias ls='exa -al --color=always --group-directories-first'

export EDITOR=emacsclient
export ALTERNATE_EDITOR=""
export TERM=xterm-256color

NC='\033[0m'
RED='\033[01;31m'
GRE='\033[01;32m'
YEL='\033[01;33m'
BLU='\033[01;34m'
MAG='\033[01;35m'
CYA='\033[01;36m'

~/.bin/ascii.sh

force_color_prompt=yes
title='\e]0; \w\a'
export PS1="${YEL}[${RED}\u${NC}${YEL}@${BLU}\h ${GRE}\w${NC}${YEL}]${RED}$ $(echo -e "$title")${NC}"
