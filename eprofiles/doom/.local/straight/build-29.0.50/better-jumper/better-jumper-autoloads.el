;;; better-jumper-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "better-jumper" "better-jumper.el" (0 0 0 0))
;;; Generated autoloads from better-jumper.el

(autoload 'better-jumper-set-jump "better-jumper" "\
Set jump point at POS.
POS defaults to point.

\(fn &optional POS)" nil nil)

(autoload 'better-jumper-jump-backward "better-jumper" "\
Jump backward COUNT positions to previous location in jump list.
If COUNT is nil then defaults to 1.

\(fn &optional COUNT)" t nil)

(autoload 'better-jumper-jump-forward "better-jumper" "\
Jump forward COUNT positions to location in jump list.
If COUNT is nil then defaults to 1.

\(fn &optional COUNT)" t nil)

(autoload 'better-jumper-jump-newest "better-jumper" "\
Jump forward to newest entry in jump list." t nil)

(autoload 'better-jumper-clear-jumps "better-jumper" "\
Clears jump list for WINDOW-OR-BUFFER.
WINDOW-OR-BUFFER should be either a window or buffer depending on the
context and will default to current context if not provided.

\(fn &optional WINDOW-OR-BUFFER)" nil nil)

(autoload 'better-jumper-get-jumps "better-jumper" "\
Get jumps for WINDOW-OR-BUFFER.
WINDOW-OR-BUFFER should be either a window or buffer depending on the
context and will default to current context if not provided.

\(fn &optional WINDOW-OR-BUFFER)" nil nil)

(autoload 'better-jumper-set-jumps "better-jumper" "\
Set jumps to JUMPS for WINDOW-OR-BUFFER.
WINDOW-OR-BUFFER should be either a window or buffer depending on the
context and will default to current context if not provided.

\(fn JUMPS &optional WINDOW-OR-BUFFER)" nil nil)

(autoload 'turn-on-better-jumper-mode "better-jumper" "\
Enable better-jumper-mode in the current buffer." nil nil)

(autoload 'turn-off-better-jumper-mode "better-jumper" "\
Disable `better-jumper-local-mode' in the current buffer." nil nil)

(autoload 'better-jumper-local-mode "better-jumper" "\
better-jumper minor mode.

This is a minor mode.  If called interactively, toggle the
`better-jumper-Local mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `better-jumper-local-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'better-jumper-mode 'globalized-minor-mode t)

(defvar better-jumper-mode nil "\
Non-nil if Better-Jumper mode is enabled.
See the `better-jumper-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `better-jumper-mode'.")

(custom-autoload 'better-jumper-mode "better-jumper" nil)

(autoload 'better-jumper-mode "better-jumper" "\
Toggle Better-Jumper-Local mode in all buffers.
With prefix ARG, enable Better-Jumper mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Better-Jumper-Local mode is enabled in all buffers where
`turn-on-better-jumper-mode' would do it.

See `better-jumper-local-mode' for more information on
Better-Jumper-Local mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "better-jumper" '("better-jumper-"))

;;;***

(provide 'better-jumper-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; better-jumper-autoloads.el ends here
