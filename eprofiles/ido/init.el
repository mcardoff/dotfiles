;; -*- lexical-binding: t; -*-
;;; Startup stuff
(defvar mpc--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq backup-directory-alist '(("." . "~/eprofiles/default/.emacs_saves/")))

;; (org-babel-load-file (expand-file-name "EmacsInit.org" user-emacs-directory))

(defvar cache-file "~/eprofiles/default/cache/autoloads")
;; (defvar 

;; (defun initialize ()
;;   (unless (load cache-file t t)
;;     (setq package-activated-list nil)
;;     (package-initialize)
;;     (with-temp-buffer
;;       ;; (cl-pushnew doom-core-dir load-path :test #'string=)
;;       (dolist (desc (delq nil (mapcar #'cdr package-alist)))
;;         (let ((load-file-name (concat (package--autoloads-file-name desc) ".el")))
;;           (when (file-readable-p load-file-name)
;;             (condition-case _
;;                 (while t (insert (read (current-buffer))))
;;               (end-of-file)))))
;;       (prin1 `(setq load-path ',load-path
;;                     auto-mode-alist ',auto-mode-alist
;;                     Info-directory-list ',Info-directory-list)
;;              (current-buffer))
;;       (write-file (concat cache-file ".el"))
;;       (byte-compile-file cache-file))))

;; (initialize)

;;;; BEGIN EMACSINIT.EL

;; package stuff

(setq package-archives '(
          ("melpa" . "https://melpa.org/packages/")
          ("org" . "https://orgmode.org/elpa/")
          ("elpa" . "https://elpa.gnu.org/packages/")))

;; (require 'package)
(require 'use-package)

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

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 40)
  (doom-modeline-icon t))

;; dashboard
;; (use-package dashboard
 ;; :custom
 ;; (dashboard-startup-banner "~/repos/mcardoff/Profile.png")
 ;; (dashboard-items '((recents  . 10)
                    ;; (agenda   . 10)))
 ;; (dashboard-set-heading-icons t)
 ;; (dashboard-set-file-icons t)
 ;; :config (dashboard-setup-startup-hook))

;; Completion frameworks

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)))

;; Auto mode for Octave
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; defuns
(defun mpc/LaTeX-setup ()
  ;; (hl-line-mode 1)
  (visual-line-mode 1))

(defun mpc/org-mode-setup ()
  (org-indent-mode)
  ;; (hl-line-mode 1)
  (visual-line-mode 1))

(defun mpc/doc-view-setup ()
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

(defun dotemacs ()
  "Opens init.el"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

;; (defun initorg ()
;;   "Opens EmacsInit.org"
;;   (interactive)
;;   (find-file (concat user-emacs-directory "EmacsInit.org")))

;; (load-file (concat user-emacs-directory "configfuns.el"))

;; Maybe using general?
(use-package general
  :config
  (global-unset-key (kbd "C-z"))
  ;; (general-define-key
  ;;  :prefix "C-z"
  ;;  "a" 'org-agenda
  ;;  "d" 'org-roam-dailies-capture-today
  ;;  "i" 'dotemacs
  ;;  "l" 'org-agenda-list
  ;;  "m" 'counsel-imenu
  ;;  "o" 'initorg)
  
  ;; (general-define-key
  ;;  :prefix "C-z c"
  ;;  "a" 'alaconfig
  ;;  "e" 'dotemacs
  ;;  "i" 'i3config
  ;;  "k" 'kakconfig
  ;;  "p" 'pbconfig
  ;;  "r" 'rngconfig)
  
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
   "C-<SPC>" 'rectangle-mark-mode
   "C-x <SPC>" 'set-mark-command))

;; which-key because there are so many bindings
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.3))

;; (use-package auctex
;;   :defer
;;   :hook
;;   (TeX-mode . mpc/LaTeX-setup)
;;   (plain-TeX-mode . mpc/LaTeX-setup)
;;   (TeXinfo-mode . mpc/LaTeX-setup)
;;   (LaTeX-mode . mpc/LaTeX-setup)
;;   (docTeX-mode . mpc/LaTeX-setup)
;;   :custom
;;   (TeX-view-program-selection
;;     '(((output-dvi has-no-display-manager) "dvi2tty") 
;;       ((output-dvi style-pstricks)  "dvips and gv")
;;        (output-dvi "xdvi")
;;        (output-pdf "Zathura")
;;        (output-html "xdg-open")))
  
;;   (LaTeX-indent-environment-list
;;    '(("verbatim" current-indentation)
;;      ("verbatim*" current-indentation)
;;      ("filecontents" current-indentation)
;;      ("filecontents*" current-indentation)
;;      ("tabular" LaTeX-indent-tabular)
;;      ("tabular*" LaTeX-indent-tabular)
;;      ("array" LaTeX-indent-tabular)
;;      ("picture")
;;      ("tabbing"))))

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
  :init (yas-global-mode)
  :custom (yas-snippet-dirs '("~/eprofiles/default/mysnippets")))

;; (use-package yasnippet-snippets
  ;; :after yasnippet)

;; (require 'org-tempo)
;; (use-package org-bullets
;;   :defer
;;   ;; :after org
;;   :hook (org-mode . org-bullets-mode))

;; (use-package org-roam
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-graph-executable "dot")
;;   (org-roam-directory "~/Roam")
;;   (org-roam-completion-everywhere t)
;;   (org-roam-completion-system 'ivy)
;;   :bind (("C-z n l" . org-roam-buffer-toggle)
;;          ("C-z n f" . org-roam-node-find)
;;          ("C-z n i" . org-roam-node-insert))
;;   :config
;;   (org-roam-setup))

;; (use-package org
;;   :defer
;;   :hook (org-mode . mpc/org-mode-setup)
;;   :bind (("<C-M-return>" . org-insert-todo-subheading))
;;   :custom
;;   (org-ellipsis " [+]")
;;   (org-directory "~/repos/org-agenda/School Schedules/")
;;   (org-agenda-files (concat user-emacs-directory "org_agenda.org"))
;;   (org-structure-template-alist
;;    '(("s"  . "src")
;;      ("e"  . "example")
;;      ("q"  . "quote")
;;      ("v"  . "verse")
;;      ("V"  . "verbatim")
;;      ("c"  . "center")
;;      ("C"  . "comment")
;;      ("l"  . "latex")
;;      ("a"  . "ascii")
;;      ("i"  . "index")
;;      ("el" . "src emacs-lisp")))
;;   :custom-face
;;   (org-block    ((t :foreground "#e4e4ef")))
;;   (org-ellipsis ((t :foreground "#FFFFFF" :underline nil)))
;;   (org-level-1  ((t :inherit 'outline-1 :height 1.15)))
;;   :config
;;   (setq org-tempo-keywords-alist nil)
;;   (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

;; (use-package doc-view
  ;; :ensure nil
  ;; :defer 2
  ;; :hook (doc-view-mode . mpc/doc-view-setup))

;; (use-package magit
  ;; :defer 5)

(use-package tramp :ensure t)

;; ;; (use-package projectile
;;   ;; :diminish projectile-mode
;;   ;; :config (projectile-mode)
;;   ;; :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-z p" . projectile-command-map)
;;   :init
;;   (when (file-directory-p "~/Projects/Code")
;;     (setq projectile-project-search-path '("~/Projects/Code")))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; (use-package counsel-projectile
;;   :defer
;;   :after 'projectile
;;   :config (counsel-projectile-mode))

(use-package cuda-mode
  :defer
  :config
  (add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode)))

(use-package octave
  :defer
  :ensure nil
  :config (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))

;; (use-package haskell-mode
;;   :defer
;;   :bind (("C-c C-c" . compile))
;;   :hook ((haskell-mode . interactive-haskell-mode)
;; 	 (haskell-mode . haskell-indent-mode))
;;   :custom
;;   (haskell-stylish-on-save t))

;; ;; linting in haskell
;; (use-package hlint-refactor
;;   :defer
;;   :after haskell-mode
;;   :hook (haskell-mode . hlint-refactor-mode))

;; (use-package yaml-mode
;;   :defer)

;; (use-package elfeed
;;   :defer 5
;;   :custom
;;   (elfeed-feeds '("http://www.reddit.com/r/emacs/.rss"
;;                   "http://www.reddit.com/r/Physics/.rss")))

;; (use-package mu4e
;;   :ensure nil
;;   :load-path "/usr/share/emacs/site-lisp/mu4e/"
;;   :defer 1 ; Wait until 1 seconds after startup
;;   :custom
;;   ;; This is set to 't' to avoid mail syncing issues when using mbsync
;;   (mu4e-change-filenames-when-moving t)
  
;;   ;; Refresh mail using isync every 10 minutes
;;   (mu4e-update-interval (* 10 60))
;;   (mu4e-get-mail-command "mbsync -a")
;;   (mu4e-maildir "~/Mail")
  
;;   (mu4e-drafts-folder "/[Gmail]/Drafts")
;;   (mu4e-sent-folder   "/[Gmail]/Sent Mail")
;;   (mu4e-refile-folder "/[Gmail]/All Mail")
;;   (mu4e-trash-folder  "/[Gmail]/Trash")
  
;;   (mu4e-maildir-shortcuts
;;    '(("/Inbox"             . ?i)
;;      ("/[Gmail]/Sent Mail" . ?s)
;;      ("/[Gmail]/Trash"     . ?t)
;;      ("/[Gmail]/Drafts"    . ?d)
;;      ("/[Gmail]/All Mail"  . ?a)
;;      ("/[Gmail]/Teacher Emails/Sullivan"   . ?z)
;;      ("/[Gmail]/Teacher Emails/Dr. Z"      . ?x)
;;      ("/[Gmail]/Teacher Emails/Littlejohn" . ?c)
;;      ("/[Gmail]/Teacher Emails/Rosenberg"  . ?v)
;;      ("/[Gmail]/Teacher Emails/Hood"       . ?b)
;;      ("/[Gmail]/Teacher Emails/IPRO"       . ?n))))

;; ;; -------------------- ;;
;; (defvar schoolpath "~/school/")
;; (defvar templatepath "~/school/template.tex")
  
;; (defun gencopy (subj code)
;;   (let ((fname
;;          (read-file-name
;;          (concat subj ": ")
;;          (concat schoolpath (concat code "/HW/")))))
;;     (copy-file templatepath fname)
;;     (find-file fname)))

;; (defun starthw ()
;;   (interactive)
;;   (let ((x (upcase (read-string "Class Shorthand: "))))
;;     (cond ((string= x "CM") (gencopy "CM" "PHYS309"))
;;           ((string= x "QM") (gencopy "QM" "PHYS406"))
;;           ((string= x "EM") (gencopy "EM" "PHYS414"))
;;           ((string= x "MM") (gencopy "MM" "PHYS502"))
;;           ((string= x "GQ") (gencopy "GQ" "PHYS510"))
;;           (t "failed"))))

;; (defun continuehw ()
;;   (interactive)
;;   (let ((x (upcase (read-string "Class Shorthand: "))))
;;     (cond ((string= x "CM") (find-file (concat schoolpath "/PHYS309/HW/")))
;;           ((string= x "QM") (find-file (concat schoolpath "/PHYS406/HW/")))
;;           ((string= x "EM") (find-file (concat schoolpath "/PHYS414/HW/")))
;;           ((string= x "MM") (find-file (concat schoolpath "/PHYS502/HW/")))
;;           ((string= x "GQ") (find-file (concat schoolpath "/PHYS510/HW/")))
;;           (t "failed"))))

;; ;;;; END OF EMACSINIT.EL

(add-hook 'emacs-startup-hook
  (lambda () (setq file-name-handler-alist mpc--file-name-handler-alist)))

(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
