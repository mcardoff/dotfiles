;; -*- lexical-binding: t; -*-
;;; Startup stuff
(defvar mpc--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(use-package compile-angel
  :ensure t
  :demand t
  :custom
  (compile-angel-verbose nil)
  :config
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; requires for emacs 28
;; (package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package) (package-install 'use-package))

(cond ((not (package-installed-p 'gruber-darker-theme))
       (use-package gruber-darker-theme))
      (t (load-theme 'gruber-darker t)))

(use-package rainbow-mode :defer t)

(use-package saveplace :defer t)

(use-package saveplace-pdf-view :defer t)

(use-package server :ensure nil :defer t)

(use-package tab-bar :ensure nil :defer t)

(use-package pass :ensure nil :defer t)

(use-package recentf :ensure nil :defer t
  :config
  (setq recentf-max-saved-items 300) ; default is 20
  (setq recentf-auto-cleanup 'mode))

(use-package autorevert :ensure nil :defer t
  :config
  (setq revert-without-query (list ".")  ; Do not prompt
	auto-revert-stop-on-user-input nil
	auto-revert-verbose t)
  (setq global-auto-revert-non-file-buffers t))

(use-package auth-source
  :ensure nil
  :defer 1
  :custom (auth-sources '("~/.config/emacs/authinfo.gpg")))

;; baseline visuals
(use-package doom-modeline
  :defer 1
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 40)
  (doom-modeline-icon t))

(use-package dashboard
  :hook ((after-init . dashboard-setup-startup-hook)
         (after-init . dashboard-insert-startupify-lists))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents   . 10)
                     (bookmarks . 10)
                     (agenda    . 10)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t))

;; completion framework(s)
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :diminish)

(use-package ivy
  :ensure t
  :diminish
  :hook (after-init . ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-count-format "(%d/%d) ")
  :custom-face
  (ivy-current-match ((t (:extend t :background "#ffdd33" :foreground "black"))))
  (ivy-minibuffer-match-highlight ((t (:inherit compilation-warning))))
  (ivy-minibuffer-match-face-1 ((t (:background "#cc8c3c"))))
  :config
  (general-define-key
   "C-s"    'swiper
   "C-x b"  'ivy-switch-buffer)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "TAB"  'ivy-alt-done
   "C-j"  'ivy-next-line
   "C-k"  'ivy-previous-line)
  (general-define-key
   :keymaps 'ivy-switch-buffer-map
   "C-k"  'ivy-previous-line
   "C-d"  'ivy-switch-buffer-kill)
  (general-define-key
   :keymaps 'ivy-reverse-i-search-map
   "C-k"  'ivy-previous-line
   "C-d"  'ivy-reverse-i-search-kill)
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist)
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist))

(use-package counsel
  :bind
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  (ivy-initial-inputs-alist nil)
  :config
  (general-define-key
   "M-x"  'counsel-M-x
   "C-x C-f"  'counsel-find-file
   "C-x b"  'counsel-switch-buffer
   "C-M-l"  'counsel-imenu)
  (general-define-key
   :keymaps 'minibuffer-local-map
   "C-r" 'counsel-minibuffer-history)
  )

(load-file (format "%s/org-defuns.el" user-emacs-directory))

(load-file (format "%s/latex-defuns.el" user-emacs-directory))

(load-file (format "%s/convenient-defuns.el" user-emacs-directory))

(use-package general
  :ensure t
  :config (global-unset-key (kbd "C-z"))

  (general-define-key
   :prefix "C-z"
   "" '(nil :which-key "General Prefix")
   "C-c" '(org-capture :which-key "Capture!")
   "a" '(org-agenda-list :which-key "Open Agenda List")
   "A" '(org-agenda :which-key "Open Agenda")
   "d" '(org-roam-dailies-capture-today :which-key "Note of the Day")
   "e" '(elfeed :which-key "Check RSS Feeds")
   "g" '(agendafile :which-key "Open Latest Org Agenda")
   "i" '(dotemacs :which-key "Open init.el")
   "j" '(projectorg :which-key "Open Current Projects Org")
   ;; "m" '(counsel-imenu :which-key "counsel-imenu")
   "o" '(initorg :which-key "Open Literate Config")
   "u" '(mu4e :which-key "Check Mail!"))

  (general-define-key
   "<escape>" 'keyboard-escape-quit
   "M-1" 'shell-command
   "M-2" 'split-window-below
   "M-3" 'split-window-right
   "M-o" 'other-window
   "M-r" 'enlarge-window
   "M-R" 'shrink-window
   "M-." 'enlarge-window-horizontally
   "M-," 'shrink-window-horizontally
   "M-<left>" 'windmove-left
   "M-<up>" 'windmove-up
   "M-<down>" 'windmove-down
   "M-<right>" 'windmove-right
   "C-<SPC>" 'set-mark-command
   "C-x <SPC>" 'rectangle-mark-mode)

  (general-define-key
   :keymaps 'prog-mode-map "C-z C-z" 'compile))

(use-package which-key
  :ensure t
  :config (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.3))

(use-package auctex
  :ensure t
  :defer t
  :config
  (require 'latex)
  (require 'auctex)
  :bind (:map LaTeX-mode-map
              ("C-z TAB" . 'mpc/toggle-latex-preamble))
  :hook
  (TeX-mode       . mpc/LaTeX-setup)
  (plain-TeX-mode . mpc/LaTeX-setup)
  (TeXinfo-mode   . mpc/LaTeX-setup)
  (LaTeX-mode     . mpc/LaTeX-setup)
  (docTeX-mode    . mpc/LaTeX-setup)
  (LaTeX-mode     . flymake-mode)
  :custom
  (TeX-view-program-selection
    '(((output-dvi has-no-display-manager) "dvi2tty") 
      ((output-dvi style-pstricks)  "dvips and gv")
       (output-dvi "xdvi")
       (output-pdf "open")
       (output-html "xdg-open")))
  (TeX-engine 'luatex)
  (TeX-parse-self t)
  (LaTeX-beamer-item-overlay-flag nil)
  (LaTeX-section-hook
   '(LaTeX-section-heading LaTeX-section-title LaTeX-section-section))

  (LaTeX-indent-environment-list
   '(("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("filecontents" current-indentation)
     ("filecontents*" current-indentation)
     ("tabular" LaTeX-indent-tabular)
     ("tabular*" LaTeX-indent-tabular)
     ("array" LaTeX-indent-tabular)
     ("picture")
     ("tabbing")))

  (LaTeX-electric-left-right-brace t)
  (LaTeX-float "H")
  (TeX-output-dir "./build"))

(use-package reftex
  :ensure nil
  :custom (reftex-plug-into-AUCTeX t)
  :hook ((LaTeX-mode . turn-on-reftex)
	 (latex-mode . turn-on-reftex)))

(use-package doc-view
  :ensure nil
  :defer
  :hook (doc-view-mode . mpc/no-lines-setup))

(use-package move-text
  :defer 2
  :diminish 
  :config
  (general-define-key
   "M-p" 'move-text-up
   "M-n" 'move-text-down))

(use-package avy
  :defer 1
  :config
  (general-define-key
   :prefix "C-z"
   "c" '(avy-goto-char :which-key "Go to char")
   "l" '(avy-goto-line :which-key "Go to line")))

(use-package multiple-cursors
  :defer 2
  :diminish
  :config
  (general-define-key
   "C-S-c C-S-c" 'mc/edit-lines
   "C->"         'mc/mark-next-like-this
   "C-<"         'mc/mark-previous-like-this
   "C-c C-<"     'mc/mark-all-like-this))

(use-package yasnippet
  :defer 5
  :config (yas-global-mode)
  :custom (yas-snippet-dirs '("~/.emacs.d/mysnippets")))

(use-package highlight-indent-guides
  :defer 1
  :hook ((prog-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-auto-enabled nil)
  :custom-face
  (highlight-indent-guides-odd-face ((t :background "#303030")))
  (highlight-indent-guides-even-face ((t :background "#252525"))))

(use-package rainbow-delimiters
  :defer 1
  :hook ((prog-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode))
  :custom
  (rainbow-delimiters-max-face-count 6)
  :custom-face
  (rainbow-delimiters-depth-1-face ((t :foreground "#f43841")))
  (rainbow-delimiters-depth-2-face ((t :foreground "#cc8c3c")))
  (rainbow-delimiters-depth-3-face ((t :foreground "#ffdd33")))
  (rainbow-delimiters-depth-4-face ((t :foreground "#73c936")))
  (rainbow-delimiters-depth-5-face ((t :foreground "#96a6c8")))
  (rainbow-delimiters-depth-6-face ((t :foreground "#565f73"))))

(use-package smartparens
  :ensure nil
  :hook (prog-mode LaTeX-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package org
  :defer 1
  :hook ((org-mode . mpc/org-mode-setup)
         (org-mode . rainbow-delimiters-mode)
         (org-agenda-mode . mpc/no-lines-setup))

  :bind 
  :custom
  (org-tags-column 0)
  (org-ellipsis " [+]")
  (org-directory "~/Org/Agenda/")
  (org-agenda-files (directory-files "~/Org/Agenda/" t "\\.org$"))
  (org-agenda-tags-column -80)
  (org-hide-block-startup t)
  :custom-face
  (org-block    ((t :foreground "#e4e4ef")))
  (org-ellipsis ((t :foreground "#FFFFFF" :underline nil)))
  (org-level-1  ((t :inherit 'outline-1 :height 1.15)))
  (org-verbatim ((t :foreground "#888888")))
  :config
  (require 'org-tempo)
  (setq org-tempo-keywords-alist nil)
  (setq org-refile-targets '((mpc/org-agenda-list :maxlevel . 3)))
  (general-define-key
   :keymaps 'org-mode-map
   "<C-M-return>" 'org-insert-todo-subheading
   "<C-return>"   'org-insert-subheading)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

(use-package org-roam
  :defer 1
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Org/Roam")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'ivy)
  :config
  (general-define-key
   :prefix "C-z"
   "n" '(nil :which-key "Roam Prefix")
   "n l" '(org-roam-buffer-toggle :which-key "Toggle Roam Buffer")
   "n f" '(org-roam-node-find :which-key "Find Node")
   "n i" '(org-roam-node-insert :which-key "Insert Node")
   "n d" '(org-roam-dailies-capture-today :which-key "Capture Daily")
   "n t" '(org-roam-dailies-goto-today :which-key "Goto Daily"))
  (org-roam-setup))

(use-package org-bullets
  :defer
  :hook (org-mode . org-bullets-mode))

;; load capture template skeletons
(load-file (format "%s/org-capture.el" user-emacs-directory))

(use-package elfeed
  :ensure t
  :defer t
  :custom
  (elfeed-db-directory (concat user-emacs-directory "elfeed"))
  (elfeed-feeds '(("https://atlas.cern/updates/briefing/feed.xml" physics cern)
		  ("http://feeds.aps.org/rss/recent/physics.xml" physics)
		  ("https://export.arxiv.org/rss/hep-ex" physics article)
		  ("https://export.arxiv.org/rss/hep-ph" physics article)))
  :commands (elfeed))

(use-package projectile
  :defer 1
  :bind 
  :custom
  (projectile-indexing-method 'native)
  (projectile-completion-system 'ivy)
  (projectile-project-search-path
   '(("~/Projects" . 1) ("~/Org/Agenda" . 1) ("~/Org/Roam" . 1)))
  (projectile-git-submodule-command "true")
  :config
  (projectile-mode +1)
  (general-define-key
   :keymaps 'projectile-mode-map
   "C-z p"  'projectile-command-map))

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (lsp-mode . mpc/lsp-mode-setup))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-log-io nil)
  (lsp-enable-which-key-integration t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t))))

(use-package magit
  :defer 1
  :custom (magit-refresh-status-buffer nil))

(use-package smerge-mode
  :ensure nil
  :custom (smerge-command-prefix "\C-cm"))

(use-package tramp
  :defer 1
  :custom (shell-prompt-pattern '"^[^#$%>\n]*~?[#$%>] *")
  :config
  (setq tramp-backup-directory-alist backup-directory-alist)
  (setq tramp-auto-save-directory
	(expand-file-name "tramp-autosave/" user-emacs-directory)))

(use-package hideshow
  :hook ((prog-mode . hs-minor-mode))
  :config
  (general-define-key
   :prefix "C-z"
   "C-<tab>" '(hs-toggle-hiding :which-key "Hide/Show Block")))

(use-package cuda-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode)))

(use-package nxml-mode
  :defer t
  :ensure nil
  :config (add-to-list 'auto-mode-alist '("\\.gmx$" . nxml-mode)))

(use-package octave
  :defer t
  :ensure nil
  :config (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))

(use-package haskell-mode
  :defer t
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indent-mode))
  :custom
  (haskell-process-type 'stack-ghci) ; use stack ghci instead of global ghc
  (haskell-stylish-on-save t))

(use-package python
  :defer t)

(use-package pyvenv-auto
  :defer t
  :hook ((python-mode . pyvenv-auto-run)))

(use-package vterm
  :defer t
  :hook (vterm-mode . mpc/no-lines-setup)
  :ensure t)

(add-hook 'emacs-startup-hook
  (lambda () (setq file-name-handler-alist mpc--file-name-handler-alist)))

(setq safe-local-variables
      '((eval add-hook 'after-save-hook
              (lambda nil
                (if (y-or-n-p "Tangle?")
                    (org-babel-tangle))) nil t)
        (eval add-hook 'after-save-hook
              (lambda nil
                (if (y-or-n-p "Reload?")
                    (load-file user-init-file))) nil t)))
(put 'list-timers 'disabled nil)
