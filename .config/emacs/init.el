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
;; (package-initialize)
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

(use-package saveplace-pdf-view :defer t)

(use-package auth-source
  :defer 1
  :custom (auth-sources ("~/.config/emacs/authinfo.gpg")))

;; Doom modeline
(use-package doom-modeline
  :defer 1
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 40)
  (doom-modeline-icon t))

;; Doesnt work with emacs 29
;; dashboard
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
  (dashboard-center-content t)
  ;; :config (dashboard-setup-startup-hook)
  )

;; Completion frameworks

;; company
(use-package company
  :ensure t
  :defer 1
  :hook (after-init . global-company-mode)
  ;; :init (global-company-mode)
  :diminish)

;; ivy
(use-package ivy
  :ensure t
  :diminish
  :hook (after-init . ivy-mode)
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
  (display-line-numbers-mode 0)
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

; temporary solution, hopefully can be replaced by something more dynamic
(defvar mpc/latest-org-file "~/Org/Agenda/SU23.org")

(defun mpc/prompt-num ()
  "Prompt user to enter a number, with input history support."
  (interactive)
  (let (n)
    (setq n (read-number "Type a number: "))
    (message "Number is %s" n)))

(defvar script-path "~/.local/scripts/find_next_hw.sh")
(defun mpc/next-hw-num (class sem schoolpath)
  (shell-command-to-string (format "/home/mcard/.local/scripts/next_hw_num.sh %s %s" sem class)))

(defun mpc/make-latest-hw-file (class sem school-path)
  "class: Subject indicator and number, sem: [FA/SP]YY, school path: no slash at end"
  (format "%s/%s/%s/%s" school-path sem class (shell-command-to-string (format "%s %s %s" script-path sem class))))

(defun mpc/create-todo-entry (num subj semester)
  (format
   "* TODO %s HW %%(mpc/next-hw-num \"%s%s\" \"%s\" \"~/school\") [[%%(mpc/make-latest-hw-file \"%s%s\" \"%s\" \"~/school\")][LaTeX File]]"
   num subj num semester subj num semester))

(defun mpc/create-next-lecture-todo (classnum subj semester)
  (format
   "* TODO 19a Lab %%? Lecture [[%s][Slides]]"
   (shell-command-to-string
    (format
     "/home/mcard/.local/scripts/find_next_lec.sh %s %s%s"
     semester subj classnum))))

(defun dotemacs ()
  "Opens init.el"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun initorg ()
  "Opens EmacsInit.org"
  (interactive)
  (find-file (concat user-emacs-directory "EmacsInit.org")))

(defun agendafile ()
  "open the latest modified org-agenda file"
  (interactive)
  (find-file (shell-command-to-string "/home/mcard/.local/scripts/latestorg.sh")))

;; Keybinds
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
   "e" '(elfeed :which-key "Check RSS Feeds")
   "g" '(agendafile :which-key "Open Latest Org Agenda")
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
   "M-<left>" 'windmove-left
   "M-<up>" 'windmove-up
   "M-<down>" 'windmove-down
   "M-<right>" 'windmove-right
   "C-<SPC>" 'set-mark-command
   "C-x <SPC>" 'rectangle-mark-mode)

  (general-define-key
   :keymaps 'c++-mode-map
   "C-z C-z" 'compile))

;; which-key because there are so many bindings
(use-package which-key
  :ensure t
  :defer 2
  :config (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.3))

;; Document writing/editing
(use-package auctex
  :defer t
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
       (output-pdf "Zathura")
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

(use-package doc-view
  :ensure nil
  :defer t
  :hook (doc-view-mode . mpc/no-lines-setup))

;; Org-mode
(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :defer 1
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-graph-executable "dot")
  (org-roam-graph-viewer "chromium")
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

(use-package org
  :defer 1
  :hook ((org-mode . mpc/org-mode-setup)
	 (org-agenda-mode . mpc/no-lines-setup))

  :bind (:map org-mode-map
	      ("<C-M-return>" . org-insert-todo-subheading)
	      ("<C-return>"   . org-insert-subheading))
  :custom
  (org-tags-column 0)
  (org-ellipsis " [+]")
  (org-directory "~/Org/Agenda/")
  (org-agenda-files (directory-files "~/Org/Agenda/" t "\\.org$"))
  (org-agenda-tags-column -80)
  (org-hide-block-startup t)
  (org-clock-sound "~/Downloads/bell.wav")
  :custom-face
  (org-block    ((t :foreground "#e4e4ef")))
  (org-ellipsis ((t :foreground "#FFFFFF" :underline nil)))
  (org-level-1  ((t :inherit 'outline-1 :height 1.15)))
  (org-verbatim ((t :foreground "#888888")))
  :config
  (require 'org-tempo)
  (setq org-tempo-keywords-alist nil)
  (setq org-refile-targets '((mpc/org-agenda-list :maxlevel . 3)))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

(setq org-capture-templates
      '(;; Classes, SP24
	("p" "PHYS 167b")
	("pr" "167b Reading" entry (file+olp "SP24.org" "PHYS 167b" "Readings")
	 "* TODO 167b Chapter %?")
	("pe" "167b Exam" entry (file+olp "SP24.org" "PHYS 167b" "Exams")
	 "* TODO 167b Exam %?")
	("ph" "167b HW" entry (file+olp "SP24.org" "PHYS 167b" "Homework")
	 (function (lambda () (mpc/create-todo-entry "167b" "PHYS" "SP24"))))
	;; Homeworks
	;; ("h"  "Add Homework")
	;; ("hz" "Self Study" entry (file+olp "FA23.org" "Particles" "Homework")
	;;  (function (lambda () (mpc/create-todo-entry "280a" "PHYS" "FA23"))))
	;; ;; exams 
	;; ("e"  "Add Exam")
	;; ("ez" "Self Study" entry (file+olp "FA23.org" "Particles" "Exams")
	;;  "* TODO Self Study Exam %?")
	;; Research
	("r"  "Research")
	("ra" "ATLAS TODO" entry (file+olp "SP24.org" "Research" "ATLAS QT")
	 "* TODO %?")
	("rm" "ML TODO" entry (file+olp "SP24.org" "Research" "ML Tracking")
	 "* TODO %?")
	("rn" "Analysis TODO" entry (file+olp "SP24.org" "Research" "VBS VVH")
	 "* TODO %?")
	("ro" "Other" entry (file+olp "SP24.org" "Research" "Other Meetings")
	 "* TODO %?")
	("w" "Weekly Meetings")
	;; ATLAS/QT Related
	("wa" "QT Meeting" entry (file+olp "SP24.org" "Research" "ATLAS QT")
	 "* TODO QT related meeting @")
	;; ML Tracking
	("wm" "ML Meeting" entry (file+olp "SP24.org" "Research" "ML Tracking")
	 "* TODO ML Tracking Meeting @ %?")
	;; Analysis meeting
	("wn" "Analysis Meeting" entry (file+olp "SP24.org" "Research" "VBS VVH")
	 "* TODO Weekly VBS VVH Meeting @ 12:00\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+Thu\"))"
	 :immediate-finish t)
	;; Aram Group meeting
	("ww" "Aram Group Meeting" entry (file+olp "SP24.org" "Research" "Other Meetings")
	 "* TODO Aram Group Meeting @ 09:00\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+Mon\"))"
	 :immediate-finish t)
	;; Brandeis-ATLAS meeting
	("wb" "Brandeis-ATLAS Meeting" entry (file+olp "SP24.org" "Research" "Other Meetings")
	 "* TODO Brandeis-ATLAS Meeting @ 08:00\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+Wed\"))"
	 :immediate-finish t)
	;; Mail Workflow
	("m" "Mail Workflow")
	;; Follow up on Email
	("mf" "Follow Up" entry (file+olp "Mail.org" "Follow Up")
         "* TODO Follow up with %:fromname on %a\nSCHEDULED: %t DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i")
	;; Read Email later
	("mr" "Read Later" entry (file+olp "SU23.org" "MAIL" "Read Later")
         "* TODO Read %:subject\nSCHEDULED: %t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i"
	 :immediate-finish t)
	;; Attend Event in Email
	("mm" "Attend Included Event" entry (file+olp "Mail.org" "Meetings")
         "* TODO Attend %:subject %a\nSCHEDULED: %t\n%i")
	;; Send email to someone
	("ms" "Send Email" entry (file+olp "Mail.org" "Send Email")
	 "* TODO Send Email to %? about \nSCHEDULED: %t DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")))

(setq org-agenda-custom-commands
      '(("z" "View Current Semester"
	 ((agenda)
	  (tags-todo "SP24")))))

(setq org-structure-template-alist
      '(("s" . "src"     ) ("e" . "example") ("q" . "quote"  ) ("v" . "verse" )
	("V" . "verbatim") ("c" . "center" ) ("C" . "comment") ("l"  . "latex")
	("a" . "ascii"   ) ("i" . "index"  ) ("el" . "src emacs-lisp")))

;; Socials
(use-package elfeed
  :ensure t
  :defer t
  :custom
  (elfeed-db-directory (concat user-emacs-directory "elfeed"))
  (elfeed-feeds '(("https://atlas.cern/updates/briefing/feed.xml" physics)
		  ("http://feeds.aps.org/rss/recent/physics.xml" physics)
		  ("https://export.arxiv.org/rss/hep-ex" phyics article)
		  ("https://export.arxiv.org/rss/hep-ph" physics article)))
  :commands (elfeed))

;; TODO: Setup slack for emacs
;; (use-package slack)

;; Mail
(use-package mu4e
  :ensure nil
  :hook ((mu4e-headers-mode . (lambda () (display-line-numbers-mode 0)))
	 (mu4e-main-mode . (lambda () (display-line-numbers-mode 0))))
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :config
  ;; Load org-mode integration
  (require 'mu4e-org)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a -c ~/.config/isync/mbsyncrc")
  (setq mu4e-maildir "~/repos/Mail")

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ivy-completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)
  
  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash")

  (setq mu4e-maildir-shortcuts
    '((:maildir "/INBOX"             :key ?i)
      (:maildir "/BRANDEIS"          :key ?b)
      (:maildir "/[Gmail]/Sent Mail" :key ?s)
      (:maildir "/[Gmail]/Trash"     :key ?t)
      (:maildir "/[Gmail]/Drafts"    :key ?d)
      (:maildir "/[Gmail]/All Mail"  :key ?a))))

;; Development Improvement
;;; Tools
(use-package projectile
  :defer 1
  :bind (:map projectile-mode-map ("C-z p" . projectile-command-map))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-project-search-path
   '(("~/repos/" . 1) ("~/repos/Programming" . 1)))
  :config
  (projectile-mode +1))

(use-package magit :defer 1)

(use-package highlight-indent-guides
  :ensure nil
  :defer 1
  :hook ((prog-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-auto-enabled nil)
  :custom-face
  (highlight-indent-guides-odd-face ((t :background "#303030")))
  (highlight-indent-guides-even-face ((t :background "#252525")))
  )

(use-package hideshow
  :ensure nil
  :hook ((prog-mode . hs-minor-mode))
  :config
  (general-define-key
   :prefix "C-z"
   "C-<tab>" '(hs-toggle-hiding :which-key "Hide/Show Block"))
  )

(use-package treemacs
  :defer 1
  :ensure nil
  :hook (treemacs-mode . mpc/no-lines-setup))

;;; Modes
(use-package tramp
  :defer 1
  :custom (shell-prompt-pattern '"^[^#$%>\n]*~?[#$%>] *"))

(use-package cuda-mode
  :defer t
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
  (haskell-process-type 'stack-ghci) ; use stack ghci instead of global ghc
  (haskell-stylish-on-save t))

; automatically detect virtual environment to use with default python-mode repl
(use-package pyvenv-auto
  :defer 1
  :hook ((python-mode . pyvenv-auto-run)))

;; (use-package ein
;;   :ensure nil
;;   :defer t)

; lsp
(defun mpc/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

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
  (lsp-enable-which-key-integration t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t))))

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
  :defer t
  :after lsp)

(use-package vterm
  :defer t
  :hook (vterm-mode . mpc/no-lines-setup)
  :ensure t)

;;;; END OF EMACSINIT.EL

(set-face-attribute 'window-divider nil
 :foreground "#282828")
(set-face-attribute 'window-divider-first-pixel nil
 :foreground "#282828")
(set-face-attribute 'window-divider-last-pixel nil
 :foreground "#282828")
(set-face-attribute 'fringe nil
 :foreground "#181818"
 :background "#181818")

(add-hook 'emacs-startup-hook
  (lambda () (setq file-name-handler-alist mpc--file-name-handler-alist)))
