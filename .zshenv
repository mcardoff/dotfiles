export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_RUNTIME_DIR="/run/user/$UID"
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
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
export MU_HOME="$HOME"/.config/mu
export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
export XSERVERRC="$XDG_CONFIG_HOME"/X11/xserverrc
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass
export TEXMFHOME="$XDG_DATA_HOME"/texmf
export TEXMFVAR="$XDG_CACHE_HOME"/texlive/texmf-var
export TEXMFCONFIG="$XDG_CONFIG_HOME"/texlive/texmf-config
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup

#if [ "$(tty)" = '/dev/tty1' ]; then
#    [ -n "$CDM_SPAWN" ] && return
#    # Avoid executing cdm(1) when X11 has already been started.
#    [ -z "$DISPLAY$SSH_TTY$(pgrep xinit)" ] && exec cdm
#fi
. "/home/mcard/.local/share/cargo/env"
