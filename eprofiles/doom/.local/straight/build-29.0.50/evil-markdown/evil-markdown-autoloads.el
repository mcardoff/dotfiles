;;; evil-markdown-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "evil-markdown" "evil-markdown.el" (0 0 0 0))
;;; Generated autoloads from evil-markdown.el

(autoload 'evil-markdown-mode "evil-markdown" "\
Buffer local minor mode for evil-markdown

This is a minor mode.  If called interactively, toggle the
`Evil-Markdown mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-markdown-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'evil-markdown-set-key-theme "evil-markdown" "\
Select what key THEMEs to enable.

\(fn THEME)" nil nil)

(register-definition-prefixes "evil-markdown" '("evil-markdown-"))

;;;***

(provide 'evil-markdown-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-markdown-autoloads.el ends here
