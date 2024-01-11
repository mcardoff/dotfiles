# start x if it isnt running
[[ -z "$(pgrep X)" ]] && (xinit -- :1)

export PATH=$HOME/bin:/usr/local/bin:$PATH

autoload -U colors && colors

# history
HISTFILE=~/.cache/zsh/history
HISTSIZE=10000
SAVEHIST=10000
setopt SHARE_HISTORY

# autocomplete
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

DISABLE_AUTO_TITLE="false"

# Set up the prompt
PROMPT='%B%F{3}[%F{1}%n%F{3}@%F{4}%m %F{2}%~%F{3}]%F{1}$%f%b '

# aliases
alias ls='eza -al --color=always --group-directories-first'
alias kssh='kitty +kitten ssh'
alias projector='xrandr --output DP-2 --right-of eDP1 --mode 800x600'
alias screen='xrandr --output HDMI-1 --right-of eDP-1 --mode 1920x1080'
alias office='xrandr --output DP-1-3 --right-of eDP-1 --mode 1920x1080 && source ~/.local/scripts/cisbg.sh'
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'
alias fixmymouse='xinput set-prop "PS/2 Generic Mouse" "Device Enabled" 0'
alias noscreenoff='xset s noblank; xset s off; xset -dpms'

# binds
bindkey "^[[3~" delete-char
bindkey "^[[3;5~" delete-word
bindkey "^[[3;3~" delete-word

# title
print -Pn "\e]0;%m:%~\a"
# memescript
~/.local/scripts/ascii.sh

# zsh-syntax-highlighting
source $XDG_CONFIG_HOME/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2 > /dev/null

[ -f "/home/mcard/.ghcup/env" ] && source "/home/mcard/.ghcup/env" # ghcup-env