;; -*- lexical-binding: t; -*-
;;; Startup stuff
(defvar mpc--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq backup-directory-alist '(("." . "~/eprofiles/default/.emacs_saves/")))

;; (org-babel-load-file (expand-file-name "EmacsInit.org" user-emacs-directory))

(defvar cache-file "~/eprofiles/default/cache/autoloads")
;;;; BEGIN EMACSINIT.EL

;; package stuff
(setq package-archives '(
          ("melpa" . "https://melpa.org/packages/")
          ("org" . "https://orgmode.org/elpa/")
          ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
;; (require 'package)
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

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 40)
  (doom-modeline-icon t))

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)))

;; Auto mode for Octave
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(defun dotemacs ()
  "Opens init.el"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

;; Maybe using general?
(use-package general
  :config
  (global-unset-key (kbd "C-z"))
  (general-define-key
   :prefix "C-z"
   "i" 'dotemacs)
  
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

;; move text helps editing
(use-package move-text
  :defer 2
  :diminish 
  :bind (("M-p" . 'move-text-up)
         ("M-n" . 'move-text-down)))

;; multiple cursors for efficient editing
(use-package multiple-cursors
  :defer 2
  :diminish
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
	 ("C->"         . 'mc/mark-next-like-this)
	 ("C-<"         . 'mc/mark-previous-like-this)
	 ("C-c C-<"     . 'mc/mark-all-like-this)))

(use-package tramp :ensure t)

(use-package cuda-mode
  :defer
  :config
  (add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode)))

(use-package org-bullets              
  :defer                              
  ;; :after org                       
  :hook (org-mode . org-bullets-mode))

(defun mpc/org-mode-setup ()
  (org-indent-mode)
  ;; (hl-line-mode 1)
  (visual-line-mode 1))

(use-package org
  :defer
  :hook (org-mode . mpc/org-mode-setup)
  :bind (("<C-M-return>" . org-insert-todo-subheading))
  :custom
  (org-ellipsis " [+]")
  :custom-face
  (org-block    ((t :foreground "#e4e4ef")))
  (org-ellipsis ((t :foreground "#FFFFFF" :underline nil)))
  (org-level-1  ((t :inherit 'outline-1 :height 1.15)))
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

;;;; END OF EMACSINIT.EL

(add-hook 'emacs-startup-hook
  (lambda () (setq file-name-handler-alist mpc--file-name-handler-alist)))

(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
