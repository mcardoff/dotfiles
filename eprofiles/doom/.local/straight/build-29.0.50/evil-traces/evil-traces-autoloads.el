;;; evil-traces-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "evil-traces" "evil-traces.el" (0 0 0 0))
;;; Generated autoloads from evil-traces.el

(defvar evil-traces-mode nil "\
Non-nil if Evil-Traces mode is enabled.
See the `evil-traces-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-traces-mode'.")

(custom-autoload 'evil-traces-mode "evil-traces" nil)

(autoload 'evil-traces-mode "evil-traces" "\
Global minor mode for evil-traces.

This is a minor mode.  If called interactively, toggle the
`Evil-Traces mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'evil-traces-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-traces" '("evil-traces-"))

;;;***

(provide 'evil-traces-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-traces-autoloads.el ends here
