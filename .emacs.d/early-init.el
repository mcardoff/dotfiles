;;; -*- lexical-binding: t; -*-

(setq package--init-file-ensured t)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq load-prefer-newer noninteractive)
(setq byte-compile-warnings '(cl-functions))
(setq inhibit-startup-screen 0)
(setq visible-bell 1)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(window-divider-mode)
(column-number-mode)
(show-paren-mode)
(fringe-mode 1)

(set-face-attribute 'window-divider nil
 :foreground "#282828")
(set-face-attribute 'window-divider-first-pixel nil
 :foreground "#282828")
(set-face-attribute 'window-divider-last-pixel nil
 :foreground "#282828")
(set-face-attribute 'fringe nil
 :foreground "#282828"
 :background "#282828")

(global-display-line-numbers-mode t)
(setq display-line-numbers-type t)
(setq dired-listing-switches "-lgXGDAh --group-directories-first")
(setq message-log-max t)

(defun mpc/display-startup-time ()
  (interactive)
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))


(add-hook 'emacs-startup-hook #'mpc/display-startup-time)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

(require 'chemacs
         (expand-file-name "chemacs.el"
                           (file-name-directory
                            (file-truename load-file-name))))

(chemacs-load-user-early-init)
