(defun dotemacs ()
  "Opens init.el"
  (interactive)
  (find-file user-init-file))

(defun initorg ()
  "Opens EmacsInit.org"
  (interactive)
  (find-file (format "%s%s" user-emacs-directory "EmacsInit.org")))

(defun projectorg ()
  "Opens Current_Projects.org"
  (interactive)
  (find-file "~/Org/Agenda/Current_Projects.org"))

(defun agendafile ()
  "open the latest modified org-agenda file"
  (interactive)
  (find-file (shell-command-to-string "/home/mcard/.local/scripts/latestorg.sh")))

(defun mpc/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))
