;;; -*- lexical-binding: t; -*-
;; (setq package-enable-at-startup nil)
;; (setq user-emacs-directory "~/.config/emacs/"
;;       user-init-file       "~/.config/emacs/init.el"
;;       auto-save-list-file-name "~/.config/emacs/cache")

;;(setq-default inhibit-redisplay t
;;              inhibit-message t)

(add-to-list 'custom-theme-load-path
             "/Users/mcardiff/.emacs.d/elpa/gruber-darker-theme-20231026.2031/")

(setq ffap-machine-p-known 'reject)
(setq ad-redefinition-action 'accept)
(setq warning-suppress-types '((lexical-binding)))
(setq idle-update-delay 1.0)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
(defalias #'view-hello-file #'ignore)  ; Never show the hello file
(setq read-process-output-max (* 512 1024))
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq backup-by-copying-when-linked t)
(setq backup-by-copying t)  ; Backup by copying rather renaming
(setq delete-old-versions t)  ; Delete excess backup versions silently
(setq version-control t)  ; Use version numbers for backup files
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq vc-make-backup-files nil)  ; Do not backup version controlled files
(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))

(setq package--init-file-ensured t
      native-comp-async-report-warnings-errors nil
      package-native-compile t
      load-prefer-newer t
      native-comp-jit-compilation t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      byte-compile-warnings '(cl-functions)
      inhibit-startup-screen 0
      visible-bell 1
      compile-angel-enable-byte-compile t
      compile-angel-enable-native-compile t
      native-comp-async-query-on-exit t
      confirm-kill-processes t
      comp-deferred-compilation t
      warning-suppress-types '((comp))
      comp-deferred-compilation t
      display-line-numbers-type t
      backup-directory-alist '(("." . "~/.emacs.d/cache/backup"))
      custom-file (concat user-emacs-directory "old_files/.emacs-custom.el")
      recentf-save-file (format "%scache/recentf" user-emacs-directory)
      ;; bookmark-file (format "%scache/bookmarks" user-emacs-directory)
      bookmark-default-file (format "%scache/bookmarks" user-emacs-directory)
      mc/list-file (format "%scache/mc-lists.el" user-emacs-directory)
      lsp-session-file (format "%scache/.lsp-session-v1" user-emacs-directory)
      nsm-settings-file (format "%scache/network-security.data" user-emacs-directory)
      message-log-max t)

(set-face-attribute 'default t
 :font "Source Code Pro"
 :foundry 'regular
 :height 140)

;; (set-frame-font "Source Code Pro 15" nil t)

(menu-bar-mode                    0)
(tool-bar-mode                    0)
(scroll-bar-mode                  0)
(tooltip-mode                     0)
(window-divider-mode              1)
(column-number-mode               1)
(show-paren-mode                  1)
(global-display-line-numbers-mode 1)
(fringe-mode                      8)

(set-face-attribute 'window-divider t
 :foreground "#282828")
(set-face-attribute 'window-divider-first-pixel t
 :foreground "#282828")
(set-face-attribute 'window-divider-last-pixel t
 :foreground "#282828")
(set-face-attribute 'fringe t
 :foreground "#181818"
 :background "#181818")

(defun my-append-env-var (var-name value)
  "Append VALUE to the beginning of current value of env variable VAR-NAME."
  (setenv var-name (if (getenv var-name)
                       (format "%s:%s" value (getenv var-name))
                     value)))

(let ((gccjitpath "/opt/homebrew/lib/gcc/11:/opt/homebrew/lib"))
  (mapc (lambda (var-name) (my-append-env-var var-name gccjitpath))
        '("LIBRARY_PATH" "LD_LIBRARY_PATH" "PATH")))

(defun mpc/display-startup-time ()
  (interactive)
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))


(add-hook 'emacs-startup-hook #'mpc/display-startup-time)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 16777216
					       gc-cons-percentage 0.1)))
