;; -*- lexical-binding: t; -*-
;;; Startup stuff
(defvar mpc--file-name-handler-alist file-name-handler-alist)
(setq native-comp-deferred-compilation t)
(setq file-name-handler-alist nil)
(setq backup-directory-alist '(("." . "~/.emacsenv/cache/")))

;;;; BEGIN EMACSINIT.EL

;; package stuff

(setq package-archives '(
          ("melpa" . "https://melpa.org/packages/")
          ("org" . "https://orgmode.org/elpa/")
          ("elpa" . "https://elpa.gnu.org/packages/")))

;; requires for emacs 28
;; (package-initialize)
;; (require 'use-package)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package) (package-install 'use-package))

;; Visual stuff

(set-face-attribute 'default nil
 :font "Source Code Pro"
 :foundry 'regular
 :height 140)

(cond ((not (package-installed-p 'gruber-darker-theme))
       (use-package gruber-darker-theme))
      (t (load-theme 'gruber-darker t)))

;; Speed
(use-package rainbow-mode
  :defer t)

(use-package recentf
  :defer t)

(use-package saveplace
  :defer t)

(use-package saveplace-pdf-view
  :defer t)

;; (use-package server
;;   :defer t)

(use-package autorevert
  :defer t)

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 40)
  (doom-modeline-icon t))

;; dashboard
(use-package dashboard
  :hook (after-init . dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner "~/repos/mcardoff/Profile.png")
  (dashboard-items '((recents  . 10)
                     (agenda   . 10)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  :config (dashboard-setup-startup-hook))

;; Completion frameworks

;; company
(use-package company
  :defer t
  :diminish
  :init (global-company-mode))

;; ivy
(use-package ivy
  :defer t
  :after counsel
  :diminish
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
  :init (ivy-mode 1)
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
  :init (ivy-rich-mode 1)
  :after counsel
  :custom
  (ivy-format-function #'ivy-format-function-line))

(use-package all-the-icons-ivy-rich
  :defer t
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package counsel
  :defer t
  :bind (("M-x" . counsel-M-x)
         ;; ("C-x b" . counsel-ibuffer)
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

(defun dotemacs ()
  "Opens init.el"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun initorg ()
  "Opens EmacsInit.org"
  (interactive)
  (find-file (concat user-emacs-directory "EmacsInit.org")))


;; Maybe using general?
(use-package general
  ;; :defer 1
  :config
  ;; (load-file (concat user-emacs-directory "configfuns.el"))
  (global-unset-key (kbd "C-z"))
  (general-define-key
   :prefix "C-z"
   "C-c" 'org-capture
   "a" 'org-agenda
   "d" 'org-roam-dailies-capture-today
   "i" 'dotemacs
   "l" 'org-agenda-list
   "m" 'counsel-imenu
   "o" 'initorg
   "u" 'mu4e)
  
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
  :config (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.3))

(use-package auctex
  :defer
  :hook
  (TeX-mode . mpc/LaTeX-setup)
  (plain-TeX-mode . mpc/LaTeX-setup)
  (TeXinfo-mode . mpc/LaTeX-setup)
  (LaTeX-mode . mpc/LaTeX-setup)
  (docTeX-mode . mpc/LaTeX-setup)
  :custom
  (TeX-view-program-selection
    '(((output-dvi has-no-display-manager) "dvi2tty") 
      ((output-dvi style-pstricks)  "dvips and gv")
       (output-dvi "xdvi")
       (output-pdf "Zathura")
       (output-html "xdg-open")))
  
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
  :custom (yas-snippet-dirs '("~/eprofiles/default/mysnippets")))

;; (use-package yasnippet-snippets
  ;; :after yasnippet)

(require 'org-tempo)
(use-package org-bullets
  :defer
  ;; :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :after general
  :init
  (setq org-roam-v2-ack t)
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
  :bind (("<C-M-return>" . org-insert-todo-subheading))
  :custom
  (org-ellipsis " [+]")
  (org-directory "~/Org/Agenda/Agenda Files/")
  (org-agenda-files (list org-directory))
  :custom-face
  (org-block    ((t :foreground "#e4e4ef")))
  (org-ellipsis ((t :foreground "#FFFFFF" :underline nil)))
  (org-level-1  ((t :inherit 'outline-1 :height 1.15)))
  :config
  (setq org-tempo-keywords-alist nil)
  (setq org-refile-targets '((mpc/org-agenda-list :maxlevel . 2)))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

(setq org-format-latex-header
      "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage{physics} 
\\usepackage{siunitx} 
\DeclareSIUnit\angstrom{\text {Å}}
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

(setq org-capture-templates
      '(("t"  "TODO Item" entry (file "FA21.org") "** TODO %?\n\n")
	("h"  "Homework flow")
	("m"  "Mail Workflow")
	("mf" "Follow Up" entry
	 (file+olp "~/Org/Agenda/Agenda Files/Mail.org" "Follow Up")
	 "* TODO Follow Up with %:fromname on %:subject, Received %:date\n%a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\")) \n%i":immediate-finish t)
	("mr" "Read Later" entry
	 (file+olp "~/Org/Agenda/Agenda Files/Mail.org" "Read Later")
	  "* TODO Read %:subject\nSCHEDULED:%t\nDEADLINE: %(org-read-date)\n\n%a\n\n%i" :immediate-finish t)))

(setq org-structure-template-alist
      '(("s" . "src"     ) ("e" . "example") ("q" . "quote"  ) ("v" . "verse" )
	("V" . "verbatim") ("c" . "center" ) ("C" . "comment") ("l"  . "latex")
	("a" . "ascii"   ) ("i" . "index"  ) ("el" . "src emacs-lisp")))

(use-package doc-view
  :ensure nil
  :defer 2
  :hook (doc-view-mode . mpc/no-lines-setup))

(use-package magit
  :defer 5)

;; (use-package tramp
;;   :defer 5)

(use-package projectile
  :after general
  :defer 10
  :diminish projectile-mode
  :config (projectile-mode)
  ;; :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-z p" . projectile-command-map)
  ;; :init
  ;; (when (file-directory-p "~/Projects/Code")
    ;; (setq projectile-project-search-path '("~/Coding/Projects/Code")))
  ;; (setq projectile-switch-project-action #'projectile-dired)
  )

(use-package counsel-projectile
  :defer 10
  :after projectile
  :config (counsel-projectile-mode))

(use-package cuda-mode
  :defer 10
  :config
  (add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode)))

(use-package octave
  :defer 10
  :ensure nil
  :config (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))

(use-package haskell-mode
  :defer 10
  :bind (("C-c C-c" . compile))
  :hook ((haskell-mode . interactive-haskell-mode)
	 (haskell-mode . haskell-indent-mode))
  :custom
  (haskell-stylish-on-save t))

;; linting in haskell
(use-package hlint-refactor
  :defer 10
  :after haskell-mode
  :hook (haskell-mode . hlint-refactor-mode))

(use-package clojure-mode :defer 10)

(use-package yaml-mode :defer 10)

(use-package elfeed
  :defer 10
  :after dashboard
  :custom
  (elfeed-db-directory "~/.config/elfeed")
  (elfeed-feeds '("http://www.reddit.com/r/emacs/.rss"
                  "http://www.reddit.com/r/Physics/.rss")))

(setq smtpmail-default-smtp-server "smtp.gmail.com")
(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer 1 ; Wait until 1 seconds after startup
  :hook
  (mu4e-view-mode . mpc/no-lines-setup)
  (mu4e-headers-mode . mpc/no-lines-setup)
  (mu4e-main-mode . mpc/no-lines-setup)
  (mu4e-compose-mode . mpc/no-lines-setup)
  :custom
  ;; Mail signature
  (mu4e-compose-signature-auto-include t)
  (mu4e-compose-signature "Michael Cardiff\nSenior\nIIT PHYS '22")

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (mu4e-change-filenames-when-moving t)

  (mu4e-mu-home "~/.config/mu/")
  
  ;; Refresh mail using isync every 10 minutes
  (mu4e-update-interval (* 10 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/Mail")
  (mu4e-drafts-folder "/[Gmail]/Drafts")
  (mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (mu4e-refile-folder "/[Gmail]/All Mail")
  (mu4e-trash-folder  "/[Gmail]/Trash")

  (mu4e-maildir-shortcuts
   '(("/Inbox"             . ?i)
     ("/[Gmail]/Sent Mail" . ?s)
     ("/[Gmail]/Trash"     . ?t)
     ("/[Gmail]/Drafts"    . ?d)
     ("/[Gmail]/All Mail"  . ?a)
     ("/Teacher Emails/Sullivan"   . ?z)
     ("/Teacher Emails/Hood"       . ?x)
     ("/Teacher Emails/Rosenberg"  . ?c)
     ("/Teacher Emails/IPRO"       . ?v)
     ("/Teacher Emails/Littlejohn" . ?b)
     ("/Teacher Emails/Dr. Z"       . ?n)))

  ;; smtp settings
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-stream-type 'starttls)
  (user-full-name "Michael Cardiff")
  (user-mail-address "mcardiff@hawk.iit.edu")
  (smtpmail-smtp-user "mcardiff@hawk.iit.edu")
  (smtpmail-local-domain "gmail.com")
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)

  ;; password
  (auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))
  (auth-source-pass-filename "~/.password-store/mbsync/")
  
  :config (require 'org-mu4e))

;; password stuff
(use-package auth-source-pass
  :ensure nil
  :init (auth-source-pass-enable))

(use-package pass)

;; now smtp stuff
;; (defvar my-mu4e-account-alist
;;   '(("Gmail"
;;      (mu4e-sent-folder "/[Gmail]/Sent Mail")
;;      (user-mail-address "mcardiff@hawk.iit.edu")
;;      (smtpmail-smtp-user "mcardiff@hawk.iit.edu")
;;      (smtpmail-local-domain "gmail.com")
;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-service 587))))


;; (defun my-mu4e-set-account ()
;;   "Set the account for composing a message.
;;    This function is taken from: 
;;     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
;;   (let* ((account
;;     (if mu4e-compose-parent-message
;;     (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;;     (string-match "/\\(.*?\\)/" maildir)
;;     (match-string 1 maildir))
;;     (completing-read (format "Compose with account: (%s) "
;;     (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
;;     (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
;;     nil t nil nil (caar my-mu4e-account-alist))))
;;     (account-vars (cdr (assoc account my-mu4e-account-alist))))
;;     (if account-vars
;;     (mapc #'(lambda (var) (set (car var) (cadr var))) account-vars)
;;     (error "No email account found"))))



;; -------------------- ;;
(defvar schoolpath "~/school/")
(defvar templatepath "~/school/template.tex")
  
(defun gencopy (subj code)
  (let ((fname
         (read-file-name
         (concat subj ": ")
         (concat schoolpath (concat code "/HW/")))))
    (copy-file templatepath fname)
    (find-file fname)))

(defun starthw ()
  (interactive)
  (let ((x (upcase (read-string "Class Shorthand: "))))
    (cond ((string= x "CM") (gencopy "CM" "PHYS309"))
          ((string= x "QM") (gencopy "QM" "PHYS406"))
          ((string= x "EM") (gencopy "EM" "PHYS414"))
          ((string= x "MM") (gencopy "MM" "PHYS502"))
          ((string= x "GQ") (gencopy "GQ" "PHYS510"))
          (t "failed"))))

(defun continuehw ()
  (interactive)
  (let ((x (upcase (read-string "Class Shorthand: "))))
    (cond ((string= x "CM") (find-file (concat schoolpath "/PHYS309/HW/")))
          ((string= x "QM") (find-file (concat schoolpath "/PHYS406/HW/")))
          ((string= x "EM") (find-file (concat schoolpath "/PHYS414/HW/")))
          ((string= x "MM") (find-file (concat schoolpath "/PHYS502/HW/")))
          ((string= x "GQ") (find-file (concat schoolpath "/PHYS510/HW/")))
          (t "failed"))))

;;;; END OF EMACSINIT.EL

(add-hook 'emacs-startup-hook
  (lambda () (setq file-name-handler-alist mpc--file-name-handler-alist)))

(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
