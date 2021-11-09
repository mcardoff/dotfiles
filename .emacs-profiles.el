(
 ;; default profile, the best one
 ("default" . ((user-emacs-directory . "~/eprofiles/default/")))
 ;; profile that is stripped down from default so that tramp works better
 ("ido"     . ((user-emacs-directory . "~/eprofiles/ido/")))
 ;; supposed to be the same as default except with evil mode but I gave up on configuring it
 ("evil"    . ((user-emacs-directory . "~/eprofiles/evil/")))
 ;; doom emacs
 ("doom"    . ((user-emacs-directory . "~/eprofiles/doom/")
               (env . (("DOOMDIR" . "~/.emacsenv/.doom.d")))))
 ;; spacemacs
 ("space"   . ((user-emacs-directory . "~/eprofiles/space/")
	       (env . (("SPACEMACSDIR" . "~/.emacsenv/.spacemacs.d")))))
)
