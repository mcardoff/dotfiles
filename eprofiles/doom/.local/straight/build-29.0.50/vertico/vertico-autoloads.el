;;; vertico-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "vertico" "vertico.el" (0 0 0 0))
;;; Generated autoloads from vertico.el

(defvar vertico-mode nil "\
Non-nil if Vertico mode is enabled.
See the `vertico-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-mode'.")

(custom-autoload 'vertico-mode "vertico" nil)

(autoload 'vertico-mode "vertico" "\
VERTical Interactive COmpletion.

This is a minor mode.  If called interactively, toggle the
`Vertico mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'vertico-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "vertico" '("vertico-"))

;;;***

;;;### (autoloads nil "vertico-buffer" "vertico-buffer.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from vertico-buffer.el

(defvar vertico-buffer-mode nil "\
Non-nil if Vertico-Buffer mode is enabled.
See the `vertico-buffer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-buffer-mode'.")

(custom-autoload 'vertico-buffer-mode "vertico-buffer" nil)

(autoload 'vertico-buffer-mode "vertico-buffer" "\
Display Vertico in a buffer instead of the minibuffer.

This is a minor mode.  If called interactively, toggle the
`Vertico-Buffer mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'vertico-buffer-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "vertico-buffer" '("vertico-buffer-"))

;;;***

;;;### (autoloads nil "vertico-directory" "vertico-directory.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from vertico-directory.el

(autoload 'vertico-directory-enter "vertico-directory" "\
Enter directory or exit completion with current candidate." t nil)

(autoload 'vertico-directory-up "vertico-directory" "\
Delete directory before point." t nil)

(autoload 'vertico-directory-delete-char "vertico-directory" "\
Delete directory or char before point." t nil)

(autoload 'vertico-directory-delete-word "vertico-directory" "\
Delete directory or word before point." t nil)

(autoload 'vertico-directory-tidy "vertico-directory" "\
Tidy shadowed file name, see `rfn-eshadow-overlay'." nil nil)

(register-definition-prefixes "vertico-directory" '("vertico-directory--completing-file-p"))

;;;***

;;;### (autoloads nil "vertico-flat" "vertico-flat.el" (0 0 0 0))
;;; Generated autoloads from vertico-flat.el

(defvar vertico-flat-mode nil "\
Non-nil if Vertico-Flat mode is enabled.
See the `vertico-flat-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-flat-mode'.")

(custom-autoload 'vertico-flat-mode "vertico-flat" nil)

(autoload 'vertico-flat-mode "vertico-flat" "\
Flat, horizontal display for Vertico.

This is a minor mode.  If called interactively, toggle the
`Vertico-Flat mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'vertico-flat-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "vertico-flat" '("vertico-flat-"))

;;;***

;;;### (autoloads nil "vertico-indexed" "vertico-indexed.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from vertico-indexed.el

(defvar vertico-indexed-mode nil "\
Non-nil if Vertico-Indexed mode is enabled.
See the `vertico-indexed-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-indexed-mode'.")

(custom-autoload 'vertico-indexed-mode "vertico-indexed" nil)

(autoload 'vertico-indexed-mode "vertico-indexed" "\
Prefix candidates with indices.

This is a minor mode.  If called interactively, toggle the
`Vertico-Indexed mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'vertico-indexed-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "vertico-indexed" '("vertico-indexed--"))

;;;***

;;;### (autoloads nil "vertico-mouse" "vertico-mouse.el" (0 0 0 0))
;;; Generated autoloads from vertico-mouse.el

(defvar vertico-mouse-mode nil "\
Non-nil if Vertico-Mouse mode is enabled.
See the `vertico-mouse-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-mouse-mode'.")

(custom-autoload 'vertico-mouse-mode "vertico-mouse" nil)

(autoload 'vertico-mouse-mode "vertico-mouse" "\
Mouse support for Vertico.

This is a minor mode.  If called interactively, toggle the
`Vertico-Mouse mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'vertico-mouse-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "vertico-mouse" '("vertico-"))

;;;***

;;;### (autoloads nil "vertico-quick" "vertico-quick.el" (0 0 0 0))
;;; Generated autoloads from vertico-quick.el

(autoload 'vertico-quick-jump "vertico-quick" "\
Jump to candidate using quick keys." t nil)

(autoload 'vertico-quick-exit "vertico-quick" "\
Exit with candidate using quick keys." t nil)

(autoload 'vertico-quick-insert "vertico-quick" "\
Insert candidate using quick keys." t nil)

(register-definition-prefixes "vertico-quick" '("vertico-quick"))

;;;***

;;;### (autoloads nil "vertico-repeat" "vertico-repeat.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from vertico-repeat.el

(autoload 'vertico-repeat--save "vertico-repeat" "\
Save Vertico status for `vertico-repeat'." nil nil)

(autoload 'vertico-repeat "vertico-repeat" "\
Repeat last Vertico completion session." t nil)

(add-hook 'minibuffer-setup-hook #'vertico-repeat--save)

(register-definition-prefixes "vertico-repeat" '("vertico-repeat--"))

;;;***

;;;### (autoloads nil "vertico-reverse" "vertico-reverse.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from vertico-reverse.el

(defvar vertico-reverse-mode nil "\
Non-nil if Vertico-Reverse mode is enabled.
See the `vertico-reverse-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-reverse-mode'.")

(custom-autoload 'vertico-reverse-mode "vertico-reverse" nil)

(autoload 'vertico-reverse-mode "vertico-reverse" "\
Reverse the Vertico display.

This is a minor mode.  If called interactively, toggle the
`Vertico-Reverse mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'vertico-reverse-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "vertico-reverse" '("vertico-reverse-"))

;;;***

(provide 'vertico-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vertico-autoloads.el ends here