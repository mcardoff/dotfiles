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

(defun org-gimme-date ()
  (format-time-string (car org-time-stamp-formats) (org-read-date nil t)))


(defvar cur-school-path "~/school/SP22/")
(defun make-phys-hw-file (class num)
  (let ((hwnum (shell-command-to-string
		(format "~/.bin/find_next_hw.sh %s%s" class num))))
  (format "%s%s%s/Cardiff_%s_HW_%s.tex" cur-school-path class num num hwnum)))

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
  :bind (("<C-M-return>" . org-insert-todo-subheading))
  :custom
  (org-tags-column 0)
  (org-ellipsis " [+]")
  (org-directory "~/Org/Agenda/")
  (org-agenda-files (directory-files-recursively "~/Org/Agenda/" "\\.org$"))
  (org-agenda-tags-column -80)
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
	("h"  "Homework flow")
	("hz" "PHYS 437" entry (id "8a056dbf-1082-47be-8c64-c3249ac5a9ae")
	 "* TODO 437 HW %?\nDEADLINE: %(org-gimme-date)\n[[%(make-phys-hw-file \"PHYS\" \"437\")][Associated File]]")
	("hx" "PHYS 440" entry (id "e6e3eb9b-91f9-4047-8ca5-e049775341b8")
	 "* TODO 440 HW %?\nDEADLINE: %(org-gimme-date)")
	("hc" "PHYS 518" entry (id "abc1d28d-c5a6-4f0e-bda4-44adbacb3179")
	 "* TODO 518 HW %?\nDEADLINE: %(org-gimme-date)")
	("hv" "PHYS 546" entry (id "fdd24cd5-2a9e-484a-bba9-be02996265a1")
	 "* TODO 546 HW %?\nDEADLINE: %(org-gimme-date)\n[[%(make-phys-hw-file \"PHYS\" \"546\")][Associated File]]")
	("hb" "PHYS 553" entry (id "76bb4c80-d5e5-4917-adc7-407be5eec2d4")
	 "* TODO 553 HW %?\nDEADLINE: %(org-gimme-date)\n[[%(make-phys-hw-file \"PHYS\" \"553\")][Associated File]]")
	("hn" "IPRO 497" entry (id "adb180e0-64a3-47d3-996b-91fdd416c6bf")
	 "* TODO IPRO Week %? HW\nDEADLINE: %(org-gimme-date)")
	("hm" "IPRO Meeting" entry (id "850a0ac3-317a-4ccc-bdbe-5b07ca95475f")
	 "* TODO IPRO Week %? Meeting\nDEADLINE: %(org-gimme-date)")
	("r" "Random Workflow")
	("rd" "Daily Item" entry (file+olp "~/Org/Agenda/MISC.org" "Daily")
	 "* TODO %?\nSCHEDULED: %t")
	;; Mail
	("m"  "Mail Workflow")
	("mf" "Follow Up" entry
	 (file+olp "~/Org/Agenda/Mail.org" "Follow Up")
	 "* TODO Follow Up with %:fromname on %:subject, Received %:date\n%a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\")) \n%i":immediate-finish t)
	("mr" "Read Later" entry
	 (file+olp "~/Org/Agenda/Mail.org" "Read Later")
	  "* TODO Read %:subject\nSCHEDULED:%t\nDEADLINE: %(org-read-date)\n\n%a\n\n%i" :immediate-finish t)))

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

(setq smtpmail-default-smtp-server "smtp.gmail.com")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer 1
  :hook
  (mu4e-view-mode . mpc/no-lines-setup)
  (mu4e-headers-mode . mpc/no-lines-setup)
  (mu4e-main-mode . mpc/no-lines-setup)
  (mu4e-compose-mode . mpc/no-lines-setup)
  :custom
  ;; Mail signature
  (mu4e-compose-signature-auto-include t)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (mu4e-change-filenames-when-moving t)
  (mu4e-mu-home "~/.local/cache/mu/")
  
  ;; Refresh mail using isync every 10 minutes
  (mu4e-update-interval (* 10 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/Mail")
  (mu4e-drafts-folder "/Gmail/[Gmail]/Drafts")
  (mu4e-sent-folder   "/Gmail/[Gmail]/Sent Mail")
  (mu4e-refile-folder "/Gmail/[Gmail]/All Mail")
  (mu4e-trash-folder  "/Gmail/[Gmail]/Trash")

  (mu4e-maildir-shortcuts
   '(("/Gmail/Inbox"                . ?q)
     ("/Brandeis/Inbox"             . ?a)
     ("/Gmail/[Gmail]/Sent Mail"    . ?w)
     ("/Brandeis/[Gmail]/Sent Mail" . ?s)
     ("/Gmail/[Gmail]/Trash"        . ?e)
     ("/Brandeis/[Gmail]/Trash"     . ?d)
     ("/Gmail/[Gmail]/Drafts"       . ?r)
     ("/Brandeis/[Gmail]/Drafts"    . ?f)
     ("/Gmail/[Gmail]/All Mail"     . ?t)
     ("/Brandeis/[Gmail]/All Mail"  . ?g)
     ))

  ;; smtp stuff
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-stream-type 'starttls)
  (smtpmail-local-domain "gmail.com")
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  ;; password
  (auth-sources '("~/.config/.authinfo" password-store))
  ;; (auth-sources '(password-store))
  (auth-source-debug t)
  (auth-source-do-cache nil)
  (auth-source-pass-filename "~/.local/share/pass/mbsync/")
  
  :config
  (setq mu4e-contexts
        (list
         ;; IIT Account
         (make-mu4e-context
          :name "IIT"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address  . "mcardiff@hawk.iit.edu")
		    (smtpmail-smtp-user . "mcardiff@hawk.iit.edu")
                    (user-full-name     . "Michael Cardiff")
                    (mu4e-drafts-folder . "/Gmail/[Gmail]/Drafts")
                    (mu4e-sent-folder   . "/Gmail/[Gmail]/Sent Mail")
                    (mu4e-refile-folder . "/Gmail/[Gmail]/All Mail")
                    (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")
		    (mu4e-compose-signature . "Michael Cardiff\nSenior\nIIT PHYS '22")))
	 ;; Brandeis Account
         (make-mu4e-context
          :name "Brandeis"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Brandeis" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address  . "mcardiff@brandeis.edu")
		    (smtpmail-smtp-user . "mcardiff@brandeis.edu")
                    (user-full-name     . "Michael Cardiff")
                    (mu4e-drafts-folder . "/Brandeis/[Gmail]/Drafts")
                    (mu4e-sent-folder   . "/Brandeis/[Gmail]/Sent Mail")
                    (mu4e-refile-folder . "/Brandeis/[Gmail]/All Mail")
                    (mu4e-trash-folder  . "/Brandeis/[Gmail]/Trash")
		    (mu4e-compose-signature . "Michael Cardiff\nGraduate Student\nBrandeis University")))
	 ))
  (auth-source-pass-enable)
  (require 'org-mu4e))

;; password stuff
(use-package auth-source-pass
  :hook (after-init . auth-source-pass-enable)
  :init (auth-source-pass-enable)
  :ensure nil)

(use-package pass)

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

