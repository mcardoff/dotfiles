# start x if it isnt running
[[ -z "$(pgrep X)" ]] && startx

export PATH=$HOME/bin:/usr/local/bin:$PATH

autoload -U colors && colors

# history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# autocomplete
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

DISABLE_AUTO_TITLE="false"

# Set up the prompt
PROMPT='%B%F{3}[%F{1}%n%F{3}@%F{4}%m %F{2}%~%F{3}]%F{1}$%f%b '

alias ls='exa -al --color=always --group-directories-first'
alias projector='xrandr --output HDMI-1 --right-of eDP-1 --primary --mode 800x600'
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'
alias tlmgr='tllocalmgr'

# title
print -Pn "\e]0;%m:%~\a"
# memescript
~/.bin/ascii.sh

# zsh-syntax-highlighting
source ~/repos/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2 > /dev/null
