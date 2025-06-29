export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.local/cache"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_RUNTIME_DIR="/run/user/$UID"
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
export XRESOURCES="$XDG_CONFIG_HOME"/X11/xresources
export LESSHISTFILE="-"
export ZDOTDIR="$HOME/.config/zsh"
export PATH="$HOME/.local/share/cargo/bin${PATH:+:${PATH}}"
export PATH="$HOME/.local/bin${PATH:+:${PATH}}"
export PATH="/snap/bin${PATH:+:${PATH}}"
export MANPATH="/usr/local/texlive/2021/texmf-dist/doc/man${MANPATH:+:${MANPATH}}"
export INFOPATH="/usr/local/texlive/2021/texmf-dist/doc/info${INFOPATH:+:${INFOPATH}}"
export STACK_ROOT="$XDG_DATA_HOME"/stack 
export GTK_RC_FILES="$XDG_CONFIG_HOME"/gtk-1.0/gtkrc
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export STACK_ROOT="$XDG_DATA_HOME"/stack
export LESS="--RAW-CONTROL-CHARS"
export LESS_TERMCAP_mb=$(tput bold; tput setaf 2) # green
export LESS_TERMCAP_md=$(tput bold; tput setaf 1) # cyan
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 3) # yellow on blue
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7) # white
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)
export GROFF_NO_SGR=1                  # for konsole and gnome-terminal
export MANPAGER='bat --style=grid'
export XMONAD_CACHE_DIR="$XDG_CACHE_HOME"/xmonad
export XMONAD_CONFIG_DIR="$XDG_CONFIG_HOME"/xmonad
export XMONAD_DATA_DIR="$XDG_DATA_HOME"/xmonad
export GVIMINIT='let $MYGVIMRC="$XDG_CONFIG_HOME/vim/gvimrc" | source $MYGVIMRC'
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'
export MU_HOME="$HOME/.local/cache/mu"
export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
export XSERVERRC="$XDG_CONFIG_HOME"/X11/xserverrc
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass
export TEXMFHOME="$XDG_DATA_HOME"/texmf
export TEXMFVAR="$XDG_CACHE_HOME"/texlive/texmf-var
export TEXMFCONFIG="$XDG_CONFIG_HOME"/texlive/texmf-config
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export TERMINFO="$XDG_DATA_HOME"/terminfo
export TERMINFO_DIRS="$XDG_DATA_HOME"/terminfo:/usr/share/terminfo
export DOOMDIR="~/.config/emacsenv/.doom.d/"
export MATHEMATICA_BASE="$XDG_CONFIG_HOME"/mathematica
export MATHEMATICA_USERBASE="$XDG_CONFIG_HOME"/mathematica
export EDITOR=kak
export VISUAL=kak
export PATH="/home/mcard/.config/anaconda3/bin:$PATH"
export CONDARC="$XDG_CONFIG_HOME/conda/condarc"
export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc
export GOPATH="$XDG_DATA_HOME"/go
export GOMODCACHE="$XDG_CACHE_HOME"/go/mod
export W3M_DIR="$XDG_STATE_HOME/w3m"
export PYTHONHISTFILE="$XDG_CACHE_HOME"/python/python_history
export LD_PRELOAD=/usr/lib/libstdc++.so.6
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java
export VSCODE_PORTABLE="$XDG_DATA_HOME"/vscode
export JUPYTER_CONFIG_PATH="$XDG_CONFIG_HOME"/jupyter
export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME"/jupyter
export GHCUP_USE_XDG_DIRS="YES"
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc
export PYTHON_HISTORY=$XDG_STATE_HOME/python_history
export PYTHONPYCACHEPREFIX=$XDG_CACHE_HOME/python
export PYTHONUSERBASE=$XDG_DATA_HOME/python
export PYTHONPATH="${PYTHONPATH}:${HOME}/RootUtils/"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME"/npm/npmrc

. "/home/mcard/.local/share/cargo/env"

setxkbmap -option ctrl:nocaps
[[ -f ~/.config/X11/xresources ]] && xrdb -merge ~/.config/X11/xresources
