;;; -*- lexical-binding: t; -*-
;; (setq package-enable-at-startup nil)
(setq package--init-file-ensured t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      load-prefer-newer noninteractive
      byte-compile-warnings '(cl-functions)
      inhibit-startup-screen 0
      visible-bell 1
      comp-deferred-compilation t
      warning-suppress-types '((comp))
      comp-deferred-compilation t
      display-line-numbers-type t
      dired-listing-switches "-lgXGDAh --group-directories-first"
      message-log-max t)

(menu-bar-mode                    0)
(tool-bar-mode                    0)
(scroll-bar-mode                  0)
(tooltip-mode                     0)
(window-divider-mode              1)
(column-number-mode               1)
(show-paren-mode                  1)
(fringe-mode                      1)
(global-display-line-numbers-mode 1)

(set-face-attribute 'window-divider nil
 :foreground "#282828")
(set-face-attribute 'window-divider-first-pixel nil
 :foreground "#282828")
(set-face-attribute 'window-divider-last-pixel nil
 :foreground "#282828")
(set-face-attribute 'fringe nil
 :foreground "#282828"
 :background "#282828")


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

(require 'chemacs
         (expand-file-name "chemacs.el"
                           (file-name-directory
                            (file-truename load-file-name))))

(chemacs-load-user-early-init)