(setq gc-cons-threshold most-positive-fixnum)
(org-babel-load-file (expand-file-name "EmacsInit.org" user-emacs-directory))

;;; shit set by emacs, one day when the world is good I will get rid of this

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-display-line-numbers-mode t)
 '(ivy-rich-mode t)
 '(org-structure-template-alist
   '(("s" . "src")
     ("e" . "example")
     ("q" . "quote")
     ("v" . "verse")
     ("V" . "verbatim")
     ("c" . "center")
     ("C" . "comment")
     ("l" . "latex")
     ("a" . "ascii")
     ("i" . "index")))
 '(package-selected-packages
   '(which-key org-roam cuda-mode counsel-projectile projectile all-the-icons-ivy-rich all-the-icons-ivy ivy-rich magit elfeed-goodies elfeed cl-lib cl-libify auctex ivy all-the-icons doom-modeline yasnippet use-package tramp smex rainbow-mode org-journal org-bullets multiple-cursors move-text haskell-mode gruber-darker-theme ein diminish counsel command-log-mode auto-complete airline-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:extend t :background "#ffdd33" :foreground "black"))))
 '(ivy-highlight-face ((t (:inherit \#cc8c3c))))
 '(ivy-minibuffer-match-face-1 ((t (:background "#cc8c3c"))))
 '(ivy-minibuffer-match-highlight ((t (:inherit compilation-warning)))))
