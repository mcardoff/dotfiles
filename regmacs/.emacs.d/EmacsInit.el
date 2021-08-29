(require 'package)
(setq package-archives '(
      ("melpa" . "https://melpa.org/packages/")
      ("org" . "https://orgmode.org/elpa/")
      ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package) 
(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(window-divider-mode)
(column-number-mode)
(show-paren-mode)
(fringe-mode 1)
(setq inhibit-startup-screen 1)
(setq visible-bell 1)

(set-face-attribute 'default nil :font "Source Code Pro" :height 140)

(use-package command-log-mode)

(set-face-attribute 'window-divider nil :foreground "#282828")
(set-face-attribute 'window-divider-first-pixel nil :foreground "#282828")
(set-face-attribute 'window-divider-last-pixel nil :foreground "#282828")
(set-face-attribute 'fringe nil :foreground "282828" :background "#282828")

(load-theme 'gruber-darker t)

(global-display-line-numbers-mode)
(setq display-line-numbers-type t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 40)
  (doom-modeline-icon t))

(setq dired-listing-switches "-lXGAh --group-directories-first")

(use-package ivy
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
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist)
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)
)

(use-package ivy-rich  
  :init (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package counsel 
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil))

(set-face-attribute
  'ivy-current-match 'nil :foreground "black" :background "#ffdd33")

;;(ido-mode 1)
;;(ido-everywhere 1)
;;
;;(use-package smex
;;  :ensure t
;;  :bind (("M-x" . smex)
;;         ("M-X" . smex-major-mode-commands))
;;)
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; (add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode))

(defun enlarge-fun () (interactive) (enlarge-window 2))
(defun shrink-fun () (interactive) (shrink-window 2))
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-r") 'enlarge-fun)
(global-set-key (kbd "M-R") 'shrink-fun)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3)
)

(add-hook 'latex-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'latex-mode-hook (lambda () (outline-minor-mode 1)))

(use-package auctex
  :defer t
  :ensure t
  :custom
  (TeX-view-program-selection 
    '(((output-dvi has-no-display-manager) "dvi2tty") 
      ((output-dvi style-pstricks)  "dvips and gv")
       (output-dvi "xdvi")
       (output-pdf "Zathura")
       (output-html "xdg-open"))))

(defun my/TeX-view-once (doc)
  "View TeX output and clean up after `my/TeX-compile-and-view'.
  Call `TeX-view' to display TeX output, and remove this function
  from `TeX-after-TeX-LaTeX-command-finished-hook', where it may
  have been placed by `my/TeX-compile-and-view'."
  (TeX-view)
  (remove-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'my/TeX-view-once))

(defun my/TeX-compile-and-view ()
  "Compile current master file using LaTeX then view output. Run the \"LaTeX\" command on the master file for active buffer. When compilation is complete, view output with default viewer (using `TeX-view')."
  (interactive)
  (TeX-command "LaTeX" 'TeX-master-file)
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'my/TeX-view-once))

(use-package cuda-mode :ensure t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  ;; :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after 'projectile
  :config (counsel-projectile-mode))

(use-package multiple-cursors
:diminish
:bind (("C-S-c C-S-c" . mc/edit-lines)
       ("C->" . mc/mark-next-like-this)
	 ("C-<" . 'mc/mark-previous-like-this)
	 ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package move-text
  :diminish 
  :bind (("M-p" . 'move-text-up)
         ("M-n" . 'move-text-down)))

(defun mpc/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (hl-line-mode 1))

(use-package org
  :hook (org-mode . mpc/org-mode-setup)
  :init
  (org-reload)
  :config
  (setq org-ellipsis " [+]")
  (setq org-directory "~/repos/org-agenda/School Schedules/")
  (setq org-agenda-files "~/regmacs/.emacs.d/org_agenda.org")
  (set-face-attribute
  'org-ellipsis 'nil :underline 'nil :foreground "FFFFFF"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/school/Roam")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'ivy)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(set-face-attribute 'org-block 'nil :foreground "#e4e4ef")

(use-package yasnippet
  :init (yas-global-mode)
  :custom (yas-snippet-dirs '("~/.emacs.d/mysnippets")))

;; (use-package outline-minor-mode)
(global-set-key (kbd "C-;") 'outline-hide-subtree)
(global-set-key (kbd "C-:") 'outline-show-subtree)
(global-set-key (kbd "C-'") 'outline-hide-entry)
(global-set-key (kbd "C-\"") 'outline-show-entry)

(setq TeX-outline-extra
   '(("%chapter" 1)
     ("%section" 2)
     ("%subsection" 3)
     ("%subsubsection" 4)
     ("%paragraph" 5)))

(font-lock-add-keywords
'latex-mode
'(("^%\\(chapter\\|\\(sub\\|subsub\\)?section\\|paragraph\\)" 0 'font-lock-keyword-face t)
("^%chapter{\\(.*\\)}"       1 'font-latex-sectioning-1-face t)
("^%section{\\(.*\\)}"       1 'font-latex-sectioning-2-face t)
("^%subsection{\\(.*\\)}"    1 'font-latex-sectioning-3-face t)
("^%subsubsection{\\(.*\\)}" 1 'font-latex-sectioning-4-face t)
("^%paragraph{\\(.*\\)}"     1 'font-latex-sectioning-5-face t)))

(use-package elfeed
:ensure t
:custom
(elfeed-feeds '("http://www.reddit.com/r/emacs/.rss"
                "http://www.reddit.com/r/Physics/.rss")))

(use-package elfeed-goodies :ensure t)

;; (use-package mu4e
;;   :ensure nil
;;   :config
;;     (setq mu4e-change-filenames-when-moving t)
;;     (setq mu4e-update-interval (* 10 60))
;;     (setq mu4e-get-mail-command "offlineimap")
;;     (setq mu4e-maildir "~/Mail")

;;     (setq mu4e-drafts-folder "/[Gmail].Drafts")
;;     (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;;     (setq mu4e-refile-folder "/[Gmail].All Mail")
;;     (setq mu4e-trash-folder  "/[Gmail].Trash")


;;     (setq mu4e-maildir-shortcuts
;;     '((:maildir "/INBOX"    :key ?i)
;;       (:maildir "/[Gmail].Sent Mail" :key ?s)
;;       (:maildir "/[Gmail].Trash"     :key ?t)
;;       (:maildir "/[Gmail].Drafts"    :key ?d)
;;       (:maildir "/[Gmail].All Mail"  :key ?a))))

(defun dotemacs () (interactive) (find-file "~/regmacs/.emacs.d/init.el"))

(defun initorg () (interactive) (find-file "~/regmacs/.emacs.d/EmacsInit.org"))

(setq backup-directory-alist '(("." . "~/.emacs_saves")))

(setq schoolpath "~/School/")
(setq templatepath "~/School/template.tex")
  
(defun gencopy (subj code)
  (let ((fname
         (read-file-name
         (concat subj ": ")
	     (concat schoolpath (concat code "/HW/")))))
  (copy-file templatepath fname) (find-file fname)))

(defun starthw ()
  (interactive)
  (let ((x (upcase (read-string "Class Shorthand: "))))
    (cond ((string= x "CM") (gencopy "CM" "PHYS309")) ;; Classical
  	  ((string= x "QM") (gencopy "QM" "PHYS406")) ;; UG Quantum
  	  ((string= x "EM") (gencopy "EM" "PHYS414")) ;; E&M
  	  ((string= x "MM") (gencopy "MM" "PHYS502")) ;; Grad Math Methods
  	  ((string= x "GQ") (gencopy "GQ" "PHYS510")) ;; Grad Quantum
  	  (t "failed"))))

(defun continuehw ()
  (interactive)
  (let ((x (upcase (read-string "Class Shorthand: "))))
    (cond ((string= x "CM") (find-file (concat schoolpath "/PHYS309/HW/"))) ;; Classical
  	  ((string= x "QM") (find-file (concat schoolpath "/PHYS406/HW/"))) ;; UG Quantum
  	  ((string= x "EM") (find-file (concat schoolpath "/PHYS414/HW/"))) ;; E&M
  	  ((string= x "MM") (find-file (concat schoolpath "/PHYS502/HW/"))) ;; Grad Math Methods
  	  ((string= x "GQ") (find-file (concat schoolpath "/PHYS510/HW/"))) ;; Grad Quantum
  	  (t "failed"))))

(defun mpc/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'mpc/display-startup-time)

(setq initial-major-mode 'lisp-interaction-mode)
