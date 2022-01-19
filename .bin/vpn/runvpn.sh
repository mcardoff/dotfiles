#!/bin/bash

OLDDIR=$(pwd)

cd ~/.bin/vpn/ || return

./.vpn_connect <<< "$(gpg --decrypt --quiet ~/.local/share/pass/mbsync/gmail.gpg)"

cd "$OLDDIR" || return
return
