(defun mpc/org-mode-setup ()
  (org-indent-mode)
  (display-line-numbers-mode 0)
  (visual-line-mode 1))

(defun mpc/org-agenda-list ()
  (delq nil (mapcar (lambda (buffer) (buffer-file-name buffer)) (org-buffer-list 'files t))))

(defun mpc/no-lines-setup ()
  (display-line-numbers-mode 0))

(setq org-agenda-custom-commands
      '(("z" "View Current Semester"
         ((agenda)
          (tags-todo "SP24")))))

(setq org-structure-template-alist
      '(("s"  . "src"           )
        ("el" . "src emacs-lisp")
	("sb" . "src bash"      )
	("e"  . "example"       )
	("q"  . "quote"         )
	("v"  . "verse"         )
        ("V"  . "verbatim"      )
	("c"  . "center"        )
	("C"  . "comment"       )
	("l"  . "latex"         )
        ("a"  . "ascii"         )
	("i"  . "index"         )))
