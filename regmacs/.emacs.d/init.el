;;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Appearance
(set-default-font "Hasklig-13")

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode)
(show-paren-mode)
(window-divider-mode)
(rich-minority-mode)
(global-display-line-numbers-mode)
(spotify-enable-song-notifications)

(setq inhibit-startup-screen 1)
(setq package-check-signature nil)
(setq backup-directory-alist '(("." . "~/.emacs_saves")))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.m$"   . octave-mode))

;;; Created keybindings
(define-prefix-command 'spot)
(global-set-key (kbd "C-z") 'spot)
(global-set-key (kbd "M-o") 'other-window)

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
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  )

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

;;; powerline stuff
(powerline-default-theme)

;;; Diminish
(require 'diminish)
(diminish 'whitespace-mode)
(diminish 'hasklig-mode)
(diminish 'eldoc-mode)
(diminish 'interactive-haskell-mode)
(diminish 'hindent-mode)
(diminish 'undo-tree-mode)

;;; Reddit Stuff
(defvar subreddit-list '(("emacs+linux+gentoo+me_irl" "megawanxl1")))

(defun reddit-browser ()
  (interactive)
  (ivy-read "Reddit: "
	    (mapcar 'car subreddit-list)
	    :sort nil
            :action '(1
		      ("o" (lambda (x)
			     (browse-subreddit x "new"))
		       "new")
		      ("t" (lambda (x)
			     (browse-subreddit x "top"))
		       "top")
		      ("r" (lambda (x)
			     (browse-subreddit x "rising"))
		       "rising")
		      ("c" (lambda (x)
			     (browse-subreddit x "controversial"))
		       "controversial"))))

(defun browse-subreddit (&optional subreddit sort)
  (interactive)
  (let ((subreddit (or subreddit (read-string
				  (format "Open Subreddit(s) [Default: %s]: " (car subreddits))
				  nil 'subreddits (car subreddits))))
	(sort (or sort (ivy-read "sort: " '("top" "new" "rising" "controversial") :sort nil :re-builder 'regexp-quote)))
	(duration))
    (if (or (equal sort "top") (equal sort "controversial"))
	(setq duration (ivy-read "Duration: " '("day" "week" "month" "year") :sort nil))
      (setq duration "day"))
    (switch-to-buffer (generate-new-buffer "*reddit*"))
    (eww-mode)
    (eww (format "https://old.reddit.com/r/%s/%s/.mobile?t=%s" subreddit sort duration))
    (my-resize-margins)))

(defun my-resize-margins ()
  (interactive)
  (if (or (> left-margin-width 0) (> right-margin-width 0))
      (progn
        (setq left-margin-width 0
              right-margin-width 0)
        (visual-line-mode -1)
        (set-window-buffer nil (current-buffer)))
    (progn
      (let ((margin-size (/ (- (frame-width) 75) 2)))
	(setq left-margin-width margin-size
              right-margin-width  margin-size)
        (visual-line-mode 1)
	(set-window-buffer nil (current-buffer))))))

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
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" default)))
 '(fringe-mode 0 nil (fringe))
 '(package-selected-packages
   (quote
    (company airline-themes ivy micgoline diminish evil-visual-mark-mode smart-mode-line-powerline-theme smart-mode-line magit spotify editorconfig wolfram-mode auto-complete move-text org-bullets multiple-cursors hindent flymake-haskell-multi flymake-hlint flymake-easy powerline hasklig-mode haskell-mode gruber-darker-theme smex)))
 '(tool-bar-mode nil)
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active1 ((t (:inherit mode-line :background "gray20" :foreground "white"))))
 '(window-divider ((t (:foreground "gray9" :weight thin :width condensed)))))
(put 'scroll-left 'disabled nil)

