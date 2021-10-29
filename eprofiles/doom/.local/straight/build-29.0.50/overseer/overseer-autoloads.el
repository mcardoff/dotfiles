;;; overseer-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "overseer" "overseer.el" (0 0 0 0))
;;; Generated autoloads from overseer.el

(autoload 'overseer-version "overseer" "\
Get the Overseer version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

(autoload 'overseer-mode "overseer" "\
Minor mode for emacs lisp files to test through ert-runner.

This is a minor mode.  If called interactively, toggle the
`overseer mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `overseer-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Key bindings:
\\{overseer-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'overseer-enable-mode "overseer" nil nil nil)

(dolist (hook '(emacs-lisp-mode-hook)) (add-hook hook 'overseer-enable-mode))

(register-definition-prefixes "overseer" '("overseer-"))

;;;***

(provide 'overseer-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; overseer-autoloads.el ends here
