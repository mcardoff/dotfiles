(setq smtpmail-default-smtp-server "smtp.gmail.com")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  ;; :defer 1
  :hook
  (mu4e-view-mode . mpc/no-lines-setup)
  (mu4e-headers-mode . mpc/no-lines-setup)
  (mu4e-main-mode . mpc/no-lines-setup)
  (mu4e-compose-mode . mpc/no-lines-setup)
  :custom
  ;; Mail signature
  (mu4e-compose-signature-auto-include t)
  (mu4e-compose-signature "Michael Cardiff\nSenior\nIIT PHYS '22")

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (mu4e-change-filenames-when-moving t)

  (mu4e-mu-home "~/.config/mu/")
  
  ;; Refresh mail using isync every 10 minutes
  (mu4e-update-interval (* 10 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/Mail")
  (mu4e-drafts-folder "/[Gmail]/Drafts")
  (mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (mu4e-refile-folder "/[Gmail]/All Mail")
  (mu4e-trash-folder  "/[Gmail]/Trash")

  (mu4e-maildir-shortcuts
   '(("/Inbox"             . ?i)
     ("/[Gmail]/Sent Mail" . ?s)
     ("/[Gmail]/Trash"     . ?t)
     ("/[Gmail]/Drafts"    . ?d)
     ("/[Gmail]/All Mail"  . ?a)
     ("/Teacher Emails/Sullivan"   . ?z)
     ("/Teacher Emails/Hood"       . ?x)
     ("/Teacher Emails/Rosenberg"  . ?c)
     ("/Teacher Emails/IPRO"       . ?v)
     ("/Teacher Emails/Littlejohn" . ?b)
     ("/Teacher Emails/Dr. Z"       . ?n)))

  :config (require 'org-mu4e))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      user-full-name "Michael Cardiff"
      user-mail-address "mcardiff@hawk.iit.edu"
      smtpmail-smtp-user "mcardiff@hawk.iit.edu"
      smtpmail-local-domain "gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")
      auth-source-pass-filename "~/.password-store/mbsync/")
  
