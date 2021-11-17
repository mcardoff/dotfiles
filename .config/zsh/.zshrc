# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/mcard/.config/zsh/.oh-my-zsh/"
plugins=(git)

DISABLE_AUTO_TITLE="false"

# Export path
PATH="$HOME/.cargo/bin${PATH:+:${PATH}}"
PATH="/snap/bin${PATH:+:${PATH}}"
PATH="$HOME/.local/bin${PATH:+:${PATH}}"

# Set up the prompt
PROMPT='%B%F{3}[%F{1}%n%F{3}@%F{4}%m %F{2}%~%F{3}]%F{1}$%f%b '

alias ls='exa -al --color=always --group-directories-first'
alias projector='xrandr --output HDMI-1 --right-of eDP-1 --primary --mode 800x600'
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'

# title
print -Pn "\e]0;%m:%~\a"
# memescript
~/.bin/ascii.sh
