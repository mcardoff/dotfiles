(defun mpc/LaTeX-setup ()
  (hl-line-mode 1)
  (visual-line-mode 1))

(defun mpc/TeX-view-once (doc)
  "View TeX output and clean up after `mpc/TeX-compile-and-view'.
  Call `TeX-view' to display TeX output, and remove this function
  from `TeX-after-TeX-LaTeX-command-finished-hook', where it may
  have been placed by `mpc/TeX-compile-and-view'."
  (TeX-view)
  (remove-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'mpc/TeX-view-once))

(defun mpc/TeX-compile-and-view ()
  "Compile current master file using LaTeX then view output. Run the \"LaTeX\" command on the master file for active buffer. When compilation is complete, view output with default viewer (using `TeX-view')."
  (interactive)
  (TeX-command "LaTeX" 'TeX-master-file)
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'mpc/TeX-view-once))

(defun mpc/toggle-latex-preamble ()
  "Toggle visibility of the LaTeX preamble in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "\\\\begin{document}" nil t)
        (let ((start (point-min))
              (end (match-beginning 0)))
          (if (invisible-p start)
              (remove-overlays start end 'invisible t)
            (let ((overlay (make-overlay start end)))
              (overlay-put overlay 'invisible t)
              (overlay-put overlay 'isearch-open-invisible 'delete-overlay))))
      (message "No LaTeX preamble found."))))
