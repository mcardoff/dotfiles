;;; early-init.el --- -*- lexical-binding: t; -*-

(setq gc-cons-threshold 8000000)

(require 'chemacs
         (expand-file-name "chemacs.el"
                           (file-name-directory
                            (file-truename load-file-name))))
(chemacs-load-user-early-init)
