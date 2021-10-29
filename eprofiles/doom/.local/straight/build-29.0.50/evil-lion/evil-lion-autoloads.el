;;; evil-lion-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "evil-lion" "evil-lion.el" (0 0 0 0))
;;; Generated autoloads from evil-lion.el
(autoload 'evil-lion-left "evil-lion" nil t)
(autoload 'evil-lion-right "evil-lion" nil t)

(defvar evil-lion-mode nil "\
Non-nil if Evil-Lion mode is enabled.
See the `evil-lion-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-lion-mode'.")

(custom-autoload 'evil-lion-mode "evil-lion" nil)

(autoload 'evil-lion-mode "evil-lion" "\
evil-lion global mode, defines align operators 'gl' and 'gL'.

This is a minor mode.  If called interactively, toggle the
`Evil-Lion mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'evil-lion-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

  Align with `gl MOTION CHAR` or right-align with `gL MOTION CHAR`.

  If CHAR is `/` you will be prompted for a regular expression instead
  of a plain character.

  If CHAR is `RET` alignment will be performed with align.el's rules
  specific for the current major mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-lion" '("evil-lion-"))

;;;***

(provide 'evil-lion-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-lion-autoloads.el ends here
