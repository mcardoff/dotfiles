(setq smtpmail-default-smtp-server "smtp.gmail.com")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer 1
  :hook
  (mu4e-view-mode . mpc/no-lines-setup)
  (mu4e-headers-mode . mpc/no-lines-setup)
  (mu4e-main-mode . mpc/no-lines-setup)
  (mu4e-compose-mode . mpc/no-lines-setup)
  :custom
  ;; Mail signature
  (mu4e-compose-signature-auto-include t)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (mu4e-change-filenames-when-moving t)
  (mu4e-mu-home "~/.local/cache/mu/")
  
  ;; Refresh mail using isync every 10 minutes
  (mu4e-update-interval (* 10 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/Mail")
  (mu4e-drafts-folder "/Gmail/[Gmail]/Drafts")
  (mu4e-sent-folder   "/Gmail/[Gmail]/Sent Mail")
  (mu4e-refile-folder "/Gmail/[Gmail]/All Mail")
  (mu4e-trash-folder  "/Gmail/[Gmail]/Trash")

  (mu4e-maildir-shortcuts
   '(("/Gmail/Inbox"                . ?q)
     ("/Brandeis/Inbox"             . ?a)
     ("/Gmail/[Gmail]/Sent Mail"    . ?w)
     ("/Brandeis/[Gmail]/Sent Mail" . ?s)
     ("/Gmail/[Gmail]/Trash"        . ?e)
     ("/Brandeis/[Gmail]/Trash"     . ?d)
     ("/Gmail/[Gmail]/Drafts"       . ?r)
     ("/Brandeis/[Gmail]/Drafts"    . ?f)
     ("/Gmail/[Gmail]/All Mail"     . ?t)
     ("/Brandeis/[Gmail]/All Mail"  . ?g)
     ))

  ;; smtp stuff
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-stream-type 'starttls)
  (smtpmail-local-domain "gmail.com")
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  ;; password
  (auth-sources '("~/.config/.authinfo" password-store))
  ;; (auth-sources '(password-store))
  (auth-source-debug t)
  (auth-source-do-cache nil)
  (auth-source-pass-filename "~/.local/share/pass/mbsync/")
  
  :config
  (setq mu4e-contexts
        (list
         ;; IIT Account
         (make-mu4e-context
          :name "IIT"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address  . "mcardiff@hawk.iit.edu")
		    (smtpmail-smtp-user . "mcardiff@hawk.iit.edu")
                    (user-full-name     . "Michael Cardiff")
                    (mu4e-drafts-folder . "/Gmail/[Gmail]/Drafts")
                    (mu4e-sent-folder   . "/Gmail/[Gmail]/Sent Mail")
                    (mu4e-refile-folder . "/Gmail/[Gmail]/All Mail")
                    (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")
		    (mu4e-compose-signature . "Michael Cardiff\nSenior\nIIT PHYS '22")))
	 ;; Brandeis Account
         (make-mu4e-context
          :name "Brandeis"
          :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Brandeis" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address  . "mcardiff@brandeis.edu")
		    (smtpmail-smtp-user . "mcardiff@brandeis.edu")
                    (user-full-name     . "Michael Cardiff")
                    (mu4e-drafts-folder . "/Brandeis/[Gmail]/Drafts")
                    (mu4e-sent-folder   . "/Brandeis/[Gmail]/Sent Mail")
                    (mu4e-refile-folder . "/Brandeis/[Gmail]/All Mail")
                    (mu4e-trash-folder  . "/Brandeis/[Gmail]/Trash")
		    (mu4e-compose-signature . "Michael Cardiff\nGraduate Student\nBrandeis University")))
	 ))
  (auth-source-pass-enable)
  (require 'org-mu4e))
