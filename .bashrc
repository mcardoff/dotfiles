# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
PATH="$HOME/.cargo/bin${PATH:+:${PATH}}"

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


# some more ls aliases
alias ls='exa -al --color=always --group-directories-first'
alias octave='octave -q --no-gui'
alias ..='cd ..'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

export EDITOR=emacs
export TERM=xterm-256color

# neofetch --source $FILE --disable cpu gpu --color_blocks off
NC='\033[0m'
YELLOW='\033[01;33m'
RED='\033[01;31m'

~/.bin/ascii.sh

force_color_prompt=yes
title='\e]0; \w\a'
export PS1="[\[${RED}\]\u\[${NC}\]@\h \[${YELLOW}\]\w \[${NC}\]]\$ \[$(echo -e "$title")\]"
