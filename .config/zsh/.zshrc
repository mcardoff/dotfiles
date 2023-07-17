# start x if it isnt running
[[ -z "$(pgrep X)" ]] && startx

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
alias ls='exa -al --color=always --group-directories-first'
alias kssh='kitty +kitten ssh'
alias projector='xrandr --output DP-2 --right-of eDP1 --mode 800x600'
alias screen='xrandr --output HDMI-1 --right-of eDP-1 '
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'
alias tlmgr='tllocalmgr'
alias fixmymouse='xinput set-prop "PS/2 Generic Mouse" 186 0'
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
source ~/repos/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2 > /dev/null
