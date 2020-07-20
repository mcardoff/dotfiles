(package-initialize)

;; Appearance
(set-default-font "Hasklig-13")

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode)
(show-paren-mode)
(window-divider-mode)
(powerline-default-theme)
(global-display-line-numbers-mode)
(spotify-enable-song-notifications)

(setq inhibit-startup-screen 1)
(setq package-check-signature nil)
(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;; Created keybindings
(define-prefix-command 'spot)
(global-set-key (kbd "C-z") 'spot)

;;; ido
(ido-mode 1)
(ido-everywhere 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; whitespace
(defun ws-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'haskell-mode-hook 'ws-handling)

;;; auto-fill
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill) 

;;; Org
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; Haskell
(require 'haskell-mode)

(setq haskell-process-show-debug-tips nil)
(setq haskell-interactive-popup-errors nil)

(define-key haskell-mode-map (kbd "C-.") 'flymake-hlint-load)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)
(add-hook 'haskell-mode-hook 'hasklig-mode)

;;; Multiple Cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;;; Move Text
(require 'move-text)

(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;;; Spotify Commands
(global-set-key (kbd "C-z p") 'spotify-playpause)
(global-set-key (kbd "C-z n") 'spotify-next)
(global-set-key (kbd "C-z m") 'spotify-previous)

;;; neotree
(global-set-key (kbd "C-`") 'neotree)

;;; Shit set by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode 1)
 '(custom-enabled-themes (quote (gruber-darker)))
 '(custom-safe-themes
   (quote
    ("5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" default)))
 '(fringe-mode 0 nil (fringe))
 '(package-selected-packages
   (quote
    (magit spotify editorconfig wolfram-mode auto-complete move-text org-bullets multiple-cursors hindent flymake-haskell-multi flymake-hlint flymake-easy powerline hasklig-mode haskell-mode gruber-darker-theme smex)))
 '(tool-bar-mode nil)
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(window-divider ((t (:foreground "gray9" :weight thin :width condensed)))))
(put 'scroll-left 'disabled nil)
