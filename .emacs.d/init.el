;; -*- lexical-binding: t; -*-
;;; Startup stuff
(defvar mpc--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;; BEGIN EMACSINIT.EL

;; package stuff

(setq package-archives '(
          ("melpa" . "https://melpa.org/packages/")
          ;; ("org" . "https://orgmode.org/elpa/")
          ("elpa" . "https://elpa.gnu.org/packages/")))

;; requires for emacs 28
(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package) (package-install 'use-package))

(cond ((not (package-installed-p 'gruber-darker-theme))
       (use-package gruber-darker-theme))
      (t (load-theme 'gruber-darker t)))

;; Speed
(use-package rainbow-mode :defer t)

(use-package recentf :defer t)

(use-package saveplace :defer t)

(use-package saveplace-pdf-view :defer t)

(use-package server :defer t)

(use-package autorevert :defer t)

;; Doom modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  ;; :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 40)
  (doom-modeline-icon t))

;; dashboard
(use-package dashboard
  :hook (after-init . dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner "~/repos/mcardoff/Profile.png")
  (dashboard-items '((recents   . 10)
		     (bookmarks . 10)
                     (agenda    . 10)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  :config (dashboard-setup-startup-hook))

;; Completion frameworks

;; company
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  ;; :init (global-company-mode)
  :diminish
  :defer f)

;; ivy
(use-package ivy
  :ensure t
  :diminish
  :hook (after-init . ivy-mode)
  ;; :init (ivy-mode 1)
  :bind (("C-s" . swiper)
         ("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-count-format "(%d/%d) ")
  :custom-face
  (ivy-current-match ((t (:extend t :background "#ffdd33" :foreground "black"))))
  (ivy-minibuffer-match-highlight ((t (:inherit compilation-warning))))
  (ivy-minibuffer-match-face-1 ((t (:background "#cc8c3c"))))
  :config
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist)
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist))

(use-package ivy-rich
  ;; :hook (after-init . ivy-rich-mode)
  ;; :init (ivy-rich-mode 1)
  :after ivy
  :custom
  (ivy-format-function #'ivy-format-function-line))

(use-package all-the-icons-ivy-rich
  ;; :hook (after-init . all-the-icons-ivy-rich-mode)
  :init (all-the-icons-ivy-rich-mode)
  :after ivy-rich)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  (ivy-initial-inputs-alist nil))

;; defuns
(defun mpc/LaTeX-setup ()
  ;; (hl-line-mode 1)
  (visual-line-mode 1))

(defun mpc/org-mode-setup ()
  (org-indent-mode)
  ;; (hl-line-mode 1)
  (visual-line-mode 1))

(defun mpc/no-lines-setup ()
  (display-line-numbers-mode 0))

(defun mpc/TeX-view-once (doc)
  "View TeX output and clean up after `my/TeX-compile-and-view'.
  Call `TeX-view' to display TeX output, and remove this function
  from `TeX-after-TeX-LaTeX-command-finished-hook', where it may
  have been placed by `my/TeX-compile-and-view'."
  (TeX-view)
  (remove-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'mpc/TeX-view-once))

(defun mpc/TeX-compile-and-view ()
  "Compile current master file using LaTeX then view output. Run the \"LaTeX\" command on the master file for active buffer. When compilation is complete, view output with default viewer (using `TeX-view')."
  (interactive)
  (TeX-command "LaTeX" 'TeX-master-file)
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'mpc/TeX-view-once))

(defun mpc/org-agenda-list ()
  (delq nil
	(mapcar (lambda (buffer)
		  (buffer-file-name buffer))
		(org-buffer-list 'files t))))

(defun mpc/prompt-num ()
  "Prompt user to enter a number, with input history support."
  (interactive)
  (let (n)
    (setq n (read-number "Type a number: "))
    (message "Number is %s" n)))

(defun dotemacs ()
  "Opens init.el"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun initorg ()
  "Opens EmacsInit.org"
  (interactive)
  (find-file (concat user-emacs-directory "EmacsInit.org")))

(defun org-gimme-date ()
  (format-time-string (car org-time-stamp-formats) (org-read-date nil t)))

(defvar script-path "~/.bin/find_next_hw.sh")
(defun mpc/next-hw-num (class sem schoolpath)
  (shell-command-to-string (format "/home/mcard/.bin/next_hw_num.sh %s %s" sem class)))

(defun mpc/make-latest-hw-file (class sem school-path)
  "class: Subject indicator and number, sem: [FA/SP]YY, school path: no slash at end"
  (format "%s/%s/%s/%s" school-path sem class (shell-command-to-string (format "%s %s %s" script-path sem class))))

;; Maybe using general?
(use-package general
  :ensure t
  :config
  (global-unset-key (kbd "C-z"))
  (general-define-key
   :prefix "C-z"
   "" '(nil :which-key "General Prefix")
   "C-c" '(org-capture :which-key "Capture!")
   "a" '(org-agenda :which-key "Open Agenda")
   "d" '(org-roam-dailies-capture-today :which-key "Note of the Day")
   "i" '(dotemacs :which-key "Open init.el")
   "l" '(org-agenda-list :which-key "Open Agenda List")
   "m" '(counsel-imenu :which-key "counsel-imenu")
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
   "C-<SPC>" 'set-mark-command
   "C-x <SPC>" 'rectangle-mark-mode))

;; which-key because there are so many bindings
(use-package which-key
  :ensure t
  :defer 2
  :config (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.3))

(use-package auctex
  :defer
  :hook
  (TeX-mode       . mpc/LaTeX-setup)
  (plain-TeX-mode . mpc/LaTeX-setup)
  (TeXinfo-mode   . mpc/LaTeX-setup)
  (LaTeX-mode     . mpc/LaTeX-setup)
  (docTeX-mode    . mpc/LaTeX-setup)
  :custom
  (TeX-view-program-selection
    '(((output-dvi has-no-display-manager) "dvi2tty") 
      ((output-dvi style-pstricks)  "dvips and gv")
       (output-dvi "xdvi")
       (output-pdf "Zathura")
       (output-html "xdg-open")))
  (TeX-engine 'luatex)
  
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
     ("tabbing"))))

(use-package move-text
  :defer 2
  :diminish 
  :bind (("M-p" . 'move-text-up)
         ("M-n" . 'move-text-down)))

(use-package multiple-cursors
  :defer 2
  :diminish
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
	 ("C->"         . 'mc/mark-next-like-this)
	 ("C-<"         . 'mc/mark-previous-like-this)
	 ("C-c C-<"     . 'mc/mark-all-like-this)))

(use-package yasnippet
  :defer 5
  :config (yas-global-mode)
  :custom (yas-snippet-dirs '("~/.config/emacs/mysnippets")))

(require 'org-tempo)
(use-package org-bullets
  :defer
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :defer
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-graph-executable "dot")
  (org-roam-directory "~/Org/Roam")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'ivy)
  :bind (("C-z n l" . org-roam-buffer-toggle)
         ("C-z n f" . org-roam-node-find)
         ("C-z n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package org
  :defer
  :hook (org-mode . mpc/org-mode-setup)
  :bind (:map org-mode-map
	      ("<C-M-return>" . org-insert-todo-subheading)
	      ("<C-return>"   . org-insert-subheading))
  :custom
  (org-tags-column 0)
  (org-ellipsis " [+]")
  (org-directory "~/Org/Agenda/")
  (org-agenda-files (directory-files "~/Org/Agenda/" t "\\.org$"))
  (org-agenda-tags-column -80)
  (org-clock-sound "~/Downloads/bell.wav")
  :custom-face
  (org-block    ((t :foreground "#e4e4ef")))
  (org-ellipsis ((t :foreground "#FFFFFF" :underline nil)))
  (org-level-1  ((t :inherit 'outline-1 :height 1.15)))
  :config
  (setq org-tempo-keywords-alist nil)
  (setq org-refile-targets '((mpc/org-agenda-list :maxlevel . 2)))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

(setq org-capture-templates
      '(("t"  "TODO Item" entry (file "FA21.org") "** TODO %?\n\n")
	;; Homeworks
	("h"  "Add Homework")
	("hz" "PHYS 161a" entry (file+olp "FA22.org" "PHYS 161a" "Homework")
	 "* TODO 161a HW %(mpc/next-hw-num \"PHYS161a\" \"FA22\" \"~/school\")%?\n[[%(mpc/make-latest-hw-file \"PHYS161a\" \"FA22\" \"~/school\")][Associated File]]")
	("hx" "PHYS 162a" entry (file+olp "FA22.org" "PHYS 162a" "Homework")
	 "* TODO 162a HW %(mpc/next-hw-num \"PHYS162a\" \"FA22\" \"~/school\")%?\n[[%(mpc/make-latest-hw-file \"PHYS162a\" \"FA22\" \"~/school\")][Associated File]]")
	("hc" "PHYS 163a" entry (file+olp "FA22.org" "PHYS 163a" "Homework")
	 "* TODO 163a HW %(mpc/next-hw-num \"PHYS163a\" \"FA22\" \"~/school\")%?\n[[%(mpc/make-latest-hw-file \"PHYS163a\" \"FA22\" \"~/school\")][Associated File]]")
	("hv" "PHYS 164a" entry (file+olp "FA22.org" "PHYS 164a" "Homework")
	 "* TODO 164a HW %(mpc/next-hw-num \"PHYS164a\" \"FA22\" \"~/school\")%?\n[[%(mpc/make-latest-hw-file \"PHYS164a\" \"FA22\" \"~/school\")][Associated File]]")
	("e"  "Add Exam")
	("ez" "PHYS 161a" entry (file+olp "FA22.org" "PHYS 161a" "Exams")
	 "* TODO 161a Exam %?")
	("ex" "PHYS 162a" entry (file+olp "FA22.org" "PHYS 162a" "Exams")
	 "* TODO 162a Exam %?")
	("ec" "PHYS 163a" entry (file+olp "FA22.org" "PHYS 163a" "Exams")
	 "* TODO 163a Exam %?")
	("ev" "PHYS 164a" entry (file+olp "FA22.org" "PHYS 164a" "Exams")
	 "* TODO 164a Exam %?")
	("p"  "Add Preclass Prep")
	("pz" "PHYS 162a" entry (file+olp "FA22.org" "PHYS 162a" "Preclass Prep")
	 "* TODO 162a Reading Question %?")
	("px" "PHYS 163a" entry (file+olp "FA22.org" "PHYS 163a" "Preclass Prep")
	 "* TODO 163a PREP %?")
      	("r" "Random Workflow")
	("rd" "Daily Item" entry (file+olp "~/Org/Agenda/MISC.org" "Daily")
	 "* TODO %?\nSCHEDULED: %t")))

(setq org-structure-template-alist
      '(("s" . "src"     ) ("e" . "example") ("q" . "quote"  ) ("v" . "verse" )
	("V" . "verbatim") ("c" . "center" ) ("C" . "comment") ("l"  . "latex")
	("a" . "ascii"   ) ("i" . "index"  ) ("el" . "src emacs-lisp")))

(use-package doc-view
  :ensure nil
  :defer
  :hook (doc-view-mode . mpc/no-lines-setup))

;; IDE 

(use-package magit
  :defer 5)

(use-package cuda-mode
  :defer
  :config
  (add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode)))

(use-package octave
  :defer
  :ensure nil
  :config (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))

(use-package haskell-mode
  :defer
  :bind (("C-c C-c" . compile))
  :hook ((haskell-mode . interactive-haskell-mode)
	 (haskell-mode . haskell-indent-mode))
  :custom
  (haskell-stylish-on-save t))

; lsp
(defun mpc/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp)
	 (lsp-mode . mpc/lsp-mode-setup))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
)

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :defer t
  :after lsp)

(use-package lsp-ivy
  :defer t)

(use-package vterm
  :defer  t
  :ensure t)

;; password stuff
(use-package auth-source-pass
  :hook (after-init . auth-source-pass-enable)
  :init (auth-source-pass-enable)
  :ensure nil)

(use-package pass)

;;;; END OF EMACSINIT.EL

(add-hook 'emacs-startup-hook
  (lambda () (setq file-name-handler-alist mpc--file-name-handler-alist)))
