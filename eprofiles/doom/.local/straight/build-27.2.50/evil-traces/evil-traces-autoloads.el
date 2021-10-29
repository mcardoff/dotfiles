;;; evil-traces-autoloads.el --- automatically extracted autoloads
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

If called interactively, enable Evil-Traces mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-traces" '("evil-traces-")))

;;;***

(provide 'evil-traces-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-traces-autoloads.el ends here
