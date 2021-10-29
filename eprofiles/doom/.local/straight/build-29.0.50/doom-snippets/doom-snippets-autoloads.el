;;; doom-snippets-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "doom-snippets" "doom-snippets.el" (0 0 0 0))
;;; Generated autoloads from doom-snippets.el

(autoload 'doom-snippets-remove-compiled-snippets "doom-snippets" "\
Delete all .yas-compiled-snippets.el files." t nil)

(autoload 'doom-snippets-initialize "doom-snippets" "\
Add `doom-snippets-dir' to `yas-snippet-dirs', replacing the default
yasnippet directory." nil nil)

(eval-after-load 'yasnippet (lambda nil (doom-snippets-initialize)))

(register-definition-prefixes "doom-snippets" '("doom-snippets-"))

;;;***

;;;### (autoloads nil "doom-snippets-lib" "doom-snippets-lib.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from doom-snippets-lib.el

(register-definition-prefixes "doom-snippets-lib" '("doom-snippets-"))

;;;***

(provide 'doom-snippets-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; doom-snippets-autoloads.el ends here
