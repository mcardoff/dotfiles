;;; -*- lexical-binding: t; -*-
;; (setq package-enable-at-startup nil)
;; (setq user-emacs-directory "~/.config/emacs/"
;;       user-init-file       "~/.config/emacs/init.el"
;;       auto-save-list-file-name "~/.config/emacs/cache")

;;(setq-default inhibit-redisplay t
;;              inhibit-message t)

(setq package--init-file-ensured t
      package-native-compile t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      auth-sources '("~/.config/emacs/authinfo.gpg" "~/.authinfo" "~/.authinfo.gpg")
      load-prefer-newer noninteractive
      byte-compile-warnings '(cl-functions)
      inhibit-startup-screen 0
      visible-bell 1
      comp-deferred-compilation t
      warning-suppress-types '((comp))
      comp-deferred-compilation t
      display-line-numbers-type t
      dired-listing-switches "-lgXGDAh --group-directories-first"
      backup-directory-alist '(("." . "~/.config/emacs/cache/backup"))
      custom-file (concat user-emacs-directory "old_files/.emacs-custom.el")
      recentf-save-file (format "%scache/recentf" user-emacs-directory)
      ;; bookmark-file (format "%scache/bookmarks" user-emacs-directory)
      bookmark-default-file (format "%scache/bookmarks" user-emacs-directory)
      mc/list-file (format "%scache/mc-lists.el" user-emacs-directory)
      lsp-session-file (format "%scache/.lsp-session-v1" user-emacs-directory)
      nsm-settings-file (format "%scache/network-security.data" user-emacs-directory)
      message-log-max t)

(set-face-attribute 'default nil
 :font "Source Code Pro"
 :foundry 'regular
 :height 140)

(menu-bar-mode                    0)
(tool-bar-mode                    0)
(scroll-bar-mode                  0)
(tooltip-mode                     0)
(window-divider-mode              1)
(column-number-mode               1)
(show-paren-mode                  1)
(global-display-line-numbers-mode 1)
(fringe-mode                      8)

(set-face-attribute 'window-divider nil
 :foreground "#282828")
(set-face-attribute 'window-divider-first-pixel nil
 :foreground "#282828")
(set-face-attribute 'window-divider-last-pixel nil
 :foreground "#282828")
(set-face-attribute 'fringe nil
 :foreground "#181818"
 :background "#181818")


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
