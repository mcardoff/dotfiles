(
 ;; default profile, the best one
 ("default" . ((user-emacs-directory . "~/.config/eprofiles/default/")))
 ;; profile that is stripped down from default so that tramp works better
 ("ido"     . ((user-emacs-directory . "~/.config/eprofiles/ido/")))
 ;; supposed to be the same as default except with evil mode but I gave up on configuring it
 ("evil"    . ((user-emacs-directory . "~/.config/eprofiles/evil/")))
 ("doom"    . ((user-emacs-directory . "~/.config/eprofiles/doom/")
	       env . (("DOOMDIR" . "~/.config/emacsenv/.doom.d/"))))
)
