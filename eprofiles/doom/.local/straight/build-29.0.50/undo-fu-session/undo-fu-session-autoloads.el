;;; undo-fu-session-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "undo-fu-session" "undo-fu-session.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from undo-fu-session.el

(autoload 'undo-fu-session-mode "undo-fu-session" "\
Toggle saving the undo data in the current buffer (Undo-Fu Session Mode).

This is a minor mode.  If called interactively, toggle the
`Undo-Fu-Session mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `undo-fu-session-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-undo-fu-session-mode 'globalized-minor-mode t)

(defvar global-undo-fu-session-mode nil "\
Non-nil if Global Undo-Fu-Session mode is enabled.
See the `global-undo-fu-session-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-fu-session-mode'.")

(custom-autoload 'global-undo-fu-session-mode "undo-fu-session" nil)

(autoload 'global-undo-fu-session-mode "undo-fu-session" "\
Toggle Undo-Fu-Session mode in all buffers.
With prefix ARG, enable Global Undo-Fu-Session mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Undo-Fu-Session mode is enabled in all buffers where
`undo-fu-session-mode-turn-on' would do it.

See `undo-fu-session-mode' for more information on Undo-Fu-Session
mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "undo-fu-session" '("undo-fu-session-"))

;;;***

(provide 'undo-fu-session-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; undo-fu-session-autoloads.el ends here
