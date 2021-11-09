;; -*- lexical-binding: t; -*-
;;; Startup stuff
(defvar mpc--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq backup-directory-alist '(("." . "~/eprofiles/regmacs/.emacs_saves/")))

;; (org-babel-load-file (expand-file-name "EmacsInit.org" user-emacs-directory))

;;;; BEGIN EMACSINIT.EL

;; package stuff

(setq package-archives '(
          ("melpa" . "https://melpa.org/packages/")
          ("org" . "https://orgmode.org/elpa/")
          ("elpa" . "https://elpa.gnu.org/packages/")))

(require 'use-package)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package) (package-install 'use-package))

;; Visual stuff

(set-face-attribute 'default nil
 :font "Source Code Pro"
 :foundry 'regular
 :height 140)

(load-theme 'gruber-darker t)

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
  :ensure t
  :config (dashboard-setup-startup-hook))

;; Completion frameworks

;; ivy
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
  :init (all-the-icons-ivy-rich-mode 1))

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

;; Auto mode for Octave
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; defuns
(defun mpc/LaTeX-setup ()
  (visual-line-mode 1)
  (hl-line-mode 1))

(defun mpc/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (hl-line-mode 1))

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

(defun dotemacs () (interactive) (find-file (concat user-emacs-directory "init.el")))

(defun initorg () (interactive) (find-file (concat user-emacs-directory "EmacsInit.org")))

;; Ease of use
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-r") 'enlarge-window)
(global-set-key (kbd "M-R") 'shrink-window)

;; Using C-z as basic map
(define-prefix-command 'mpc-map)
(global-set-key (kbd "C-z") 'mpc-map)
(global-set-key (kbd "C-z a") 'org-agenda)
(global-set-key (kbd "C-z l") 'org-agenda-list)
(global-set-key (kbd "C-z i") 'dotemacs)
(global-set-key (kbd "C-z d") 'initorg)

;; Which-key because there are so many bindings
(use-package which-key
  :init (which-key-mode)
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

(use-package cuda-mode
  :defer
  :config
  (add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode)))

(use-package projectile
  :defer
  :diminish projectile-mode
  :config (projectile-mode)
  ;; :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-z p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; (use-package counsel-projectile
;;   :after 'projectile
;;   :config (counsel-projectile-mode))

(use-package multiple-cursors
  :defer 2
  :diminish
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->"         . mc/mark-next-like-this)
	 ("C-<"         . 'mc/mark-previous-like-this)
	 ("C-c C-<"     . 'mc/mark-all-like-this)))

(use-package move-text
  :defer 2
  :diminish 
  :bind (("M-p" . 'move-text-up)
         ("M-n" . 'move-text-down)))

(use-package org
  :hook (org-mode . mpc/org-mode-setup)
  :custom
  (org-ellipsis " [+]")
  (org-directory "~/repos/org-agenda/School Schedules/")
  (org-agenda-files (concat user-emacs-directory "org_agenda.org"))
  (org-structure-template-alist
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
  :custom-face
  (org-block    ((t (:foreground "#e4e4ef"))))
  (org-ellipsis ((t (:foreground "#FFFFFF" :underline nil))))
  (outline-3    ((t (:foreground "#ffdd33" :weight bold :family "Source Code Pro" :slant normal))))
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

(use-package org-bullets
  :defer
  :after org
  :hook (org-mode . org-bullets-mode))

;; (use-package org-roam
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory "~/school/Roam")
;;   (org-roam-completion-everywhere t)
;;   (org-roam-completion-system 'ivy)
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n i" . org-roam-node-insert))
;;   :config
;;   (org-roam-setup))

(use-package general
  :after evil
  :config
  (general-create-definer mpc/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (mpc/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "f" 'counsel-find-file
    "x" 'counsel-M-x
    ))

;; bindings with SPC


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package hydra
  :defer t)

(use-package magit
  :defer 5)

(use-package yasnippet
  :defer 5
  :init (yas-global-mode)
  :custom (yas-snippet-dirs '("~/eprofiles/evil/mysnippets")))

(use-package elfeed
  :defer 5
  :custom
  (elfeed-feeds '("http://www.reddit.com/r/emacs/.rss"
                  "http://www.reddit.com/r/Physics/.rss")))

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

(setq schoolpath "~/school/")
(setq templatepath "~/school/template.tex")
  
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

;;;; END OF EMACSINIT.EL

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist mpc--file-name-handler-alist)))

;;; shit set by emacs, one day when the world is good I will get rid of this
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elfeed yasnippet which-key websocket visual-fill-column use-package request rainbow-mode projectile powerline popwin polymode pkg-info org-bullets org noflet multiple-cursors move-text magit gruber-darker-theme doom-modeline deferred dashboard cuda-mode counsel cl-libify auctex anaphora all-the-icons-ivy-rich all-the-icons-ivy ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:extend t :background "#ffdd33" :foreground "black"))))
 '(ivy-minibuffer-match-face-1 ((t (:background "#cc8c3c"))))
 '(ivy-minibuffer-match-highlight ((t (:inherit compilation-warning))))
 '(org-block ((t (:foreground "#e4e4ef"))))
 '(org-ellipsis ((t (:foreground "#FFFFFF" :underline nil))))
 '(outline-3 ((t (:foreground "#ffdd33" :weight bold :family "Source Code Pro" :slant normal)))))
