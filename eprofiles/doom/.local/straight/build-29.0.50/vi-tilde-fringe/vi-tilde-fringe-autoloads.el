;;; vi-tilde-fringe-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "vi-tilde-fringe" "vi-tilde-fringe.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from vi-tilde-fringe.el

(autoload 'vi-tilde-fringe-mode "vi-tilde-fringe" "\
Buffer-local minor mode to display tildes in the fringe when the line is
empty.

This is a minor mode.  If called interactively, toggle the
`Vi-Tilde-Fringe mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `vi-tilde-fringe-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-vi-tilde-fringe-mode 'globalized-minor-mode t)

(defvar global-vi-tilde-fringe-mode nil "\
Non-nil if Global Vi-Tilde-Fringe mode is enabled.
See the `global-vi-tilde-fringe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-vi-tilde-fringe-mode'.")

(custom-autoload 'global-vi-tilde-fringe-mode "vi-tilde-fringe" nil)

(autoload 'global-vi-tilde-fringe-mode "vi-tilde-fringe" "\
Toggle Vi-Tilde-Fringe mode in all buffers.
With prefix ARG, enable Global Vi-Tilde-Fringe mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Vi-Tilde-Fringe mode is enabled in all buffers where
`vi-tilde-fringe-mode--turn-on' would do it.

See `vi-tilde-fringe-mode' for more information on Vi-Tilde-Fringe
mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "vi-tilde-fringe" '("vi-tilde-fringe-"))

;;;***

(provide 'vi-tilde-fringe-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vi-tilde-fringe-autoloads.el ends here
