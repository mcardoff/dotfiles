(defvar mpc/latest-org-file "~/Org/Agenda/Research.org")

(defvar fixed-timestamp
  "%%(org-insert-time-stamp (org-read-date nil t \"+%s\"))")

(defvar script-path "~/.local/scripts/find_next_hw.sh")

(defun mpc/next-hw-num (class sem schoolpath)
  (shell-command-to-string (format "/home/mcard/.local/scripts/next_hw_num.sh %s %s" sem class)))

(defun mpc/make-latest-hw-file (class sem school-path)
  "class: Subject indicator and number, sem: [FA/SP]YY, school path: no slash at end"
  (format "%s/%s/%s/%s" school-path sem class (shell-command-to-string (format "%s %s %s" script-path sem class))))

(defun mpc/create-todo-entry (num subj semester)
  "Create a Homework todo entry following the format:
* TODO <Class> HW <Number> <Link to LaTeX File>"
  (format
   "* TODO %s HW %%(mpc/next-hw-num \"%s%s\" \"%s\" \"~/school\") [[%%(mpc/make-latest-hw-file \"%s%s\" \"%s\" \"~/school\")][LaTeX File]]"
   num subj num semester subj num semester))

(defun mpc/basic-capture-skeleton (content)
  "Basic todo skeleton"
  (format "* TODO %s" content))

(defun mpc/basic-capture-deadline-skeleton (content deadline)
  "Basic todo skeleton with deadline"
  (mpc/basic-capture-skeleton
   (format "%s\nDEADLINE: %s" content deadline)))

(defun mpc/capture-template-skeleton (prefix title time deadlinetext)
  "Basic todo skeleton with deadline date and time, following the format:
* TODO <Category> <Name of item> @ <time> \n <DEADLINE>
prefix: determines the category, e.g. if it is a SLAC, ATLAS, Brandeis item
title: the actual name of the item, description of what to do
time: deadline time
deadlinetext: formatted timestamp"
  (mpc/basic-capture-deadline-skeleton
   (format "%s%s @ %s" prefix title time)
   deadlinetext))

(defun mpc/action-item-skeleton (prefix item-name deadlinetext)
  "Basic todo skeleton specified for action items, see mpc/capture-template-skeleton for more info"
  (mpc/basic-capture-deadline-skeleton
   (format "%s %s" prefix item-name)
   deadlinetext))

(defun mpc/meeting-custom-title-dl (prefix)
  (mpc/capture-template-skeleton
   prefix "%^{Meeting}" "%^{Start Time}" "%^{DEADLINE}t"))

(defun mpc/meeting-custom-linktitle-dl (prefix)
  (mpc/capture-template-skeleton
   prefix " [[%^{Link to Event}][%^{Event Name}]]" "%^{Start Time}" "%^{DEADLINE}t"))

(defun mpc/meeting-fixed-dl (prefix time dow)
  (mpc/capture-template-skeleton
   (format "%s Meeting" prefix) "" time
   (format fixed-timestamp dow)))

(defun mpc/action-item-dl (prefix)
  "Action item, prompted for deadline"
  (mpc/action-item-skeleton
   prefix "Action Item: %^{}" "%^{DEADLINE}t"))

(defun mpc/action-item-title ()
  (mpc/action-item-skeleton
   "%^{CATEGORY}" "Action Item: %^{}" "%^{DEADLINE}t"))

(setq org-capture-templates
      '(("p" "PHYS 167b")
        ("w" "Weekly Meetings")
        ("i" "Action Items")
        ("m" "Mail Workflow")
        ("pr" "167b Reading"
         entry (file+olp "SP24.org" "PHYS 167b" "Readings")
         "* TODO 167b %?")
        ("pe" "167b Exam"
         entry (file+olp "SP24.org" "PHYS 167b" "Exams")
         "* TODO 167b Exam %?")
        ("ph" "167b HW"
         entry (file+olp "SP24.org" "PHYS 167b" "Homework")
         (function (lambda () (mpc/create-todo-entry "167b" "PHYS" "SP24"))))))

;; Meetings
;; Brandeis meetings: w+top row
(add-to-list 'org-capture-templates
             '("wq" "Brandeis-ATLAS Meeting"
               entry (file+olp "Research.org" "Brandeis")
               (function
                (lambda ()
		  (mpc/meeting-fixed-dl "Brandeis-ATLAS" "08:00" "Wed")))
               :immediate-finish t))

(add-to-list 'org-capture-templates
             '("ww" "Aram Meeting"
               entry (file+olp "Research.org" "Brandeis")
               (function
                (lambda ()
		  (mpc/meeting-fixed-dl "Aram Group" "08:30" "Mon")))
               :immediate-finish t))

;; SLAC meetings: w+mid row
(add-to-list 'org-capture-templates
             '("wa" "SLAC-ATLAS Meeting"
               entry (file+olp "Research.org" "SLAC" "Meetings")
               (function
                (lambda ()
		  (mpc/meeting-fixed-dl "SLAC-ATLAS" "08:30" "Fri")))
               :immediate-finish t))

(add-to-list 'org-capture-templates
             '("ws" "SLAC 4D Meeting"
               entry (file+olp "Research.org" "SLAC" "Meetings")
               (function
                (lambda ()
		  (mpc/meeting-fixed-dl "SLAC 4D" "11:00" "Fri")))
               :immediate-finish t))

(add-to-list 'org-capture-templates
             '("wd" "SLAC LGAD"
               entry (file+olp "Research.org" "SLAC" "Meetings")
               (function
                (lambda ()
		  (mpc/meeting-fixed-dl "SLAC LGAD" "14:00" "Mon")))
               :immediate-finish t))

(add-to-list 'org-capture-templates
             '("wf" "Other SLAC Indico Event"
               entry (file+olp "Research.org" "SLAC" "Meetings")
               (function
                (lambda ()
		  (mpc/meeting-custom-linktitle-dl "SLAC")))
               :immediate-finish t))

;; CERN/ATLAS Meetings w+bot row
(add-to-list 'org-capture-templates
             '("wz" "ATLAS 4D"
               entry (file+olp "Research.org" "ATLAS" "4D Tracking")
               (function
                (lambda ()
		  (mpc/meeting-fixed-dl "4D Tracking" "07:30" "Tue")))
               :immediate-finish t))

(add-to-list 'org-capture-templates
             '("wx" "VBS VVH Meeting"
               entry (file+olp "Research.org" "ATLAS" "VBS VVH")
               (function
                (lambda ()
		  (mpc/meeting-fixed-dl "VBS Higgs" "07:00" "Mon")))
               :immediate-finish t))

(add-to-list 'org-capture-templates
             '("wc" "Other ATLAS Event"
               entry (file+olp "Research.org" "ATLAS" "Other")
               (function
                (lambda ()
		  (mpc/meeting-custom-linktitle-dl "Indico Event")))
               :immediate-finish t))

(add-to-list 'org-capture-templates
             '("wv" "Other ATLAS Indico Event"
               entry (file+olp "Research.org" "ATLAS" "Other")
               (function
                (lambda ()
		  (mpc/meeting-custom-title-dl "Indico Event")))
               :immediate-finish t))

(add-to-list 'org-capture-templates
             '("wm" "Other Meeting"
               entry (file+olp "Research.org" "Other")
               (function
                (lambda () (mpc/meeting-custom-title-dl "")))
               :immediate-finish t))

;; Action Items
(add-to-list 'org-capture-templates
             '("in" "VBS VVH Action Item"
               entry (file+olp "Research.org" "VBS VVH")
               (function (lambda () (mpc/action-item-dl "VBS Higgs")))
               :immediate-finish t))

(add-to-list 'org-capture-templates
             '("is" "SLAC Action Item"
               entry (file+olp "Research.org" "SLAC" "Action Items")
               (function (lambda () (mpc/action-item-dl "SLAC")))
               :immediate-finish t))

(add-to-list 'org-capture-templates
             '("ii" "Misc TODO"
               entry (file+headline "Research.org" "Other")
               (function (lambda () (mpc/action-item-title)))
               :refile-targets ((nil :maxlevel . 2))
               :immediate-finish t))

;; Follow up on Email
(add-to-list 'org-capture-templates
             '("mf" "Follow Up" entry (file+olp "Mail.org" "Follow Up")
                "* TODO Follow up with %:fromname on %a\nSCHEDULED: %t DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i"))
;; Read Email later
(add-to-list 'org-capture-templates
             '("mr" "Read Later" entry (file+olp "SU23.org" "MAIL" "Read Later")
                "* TODO Read %:subject\nSCHEDULED: %t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i"
                :immediate-finish t))
;; Attend Event in Email
(add-to-list 'org-capture-templates
             '("mm" "Attend Included Event" entry (file+olp "Mail.org" "Meetings")
               "* TODO Attend %:subject %a\nSCHEDULED: %t\n%i"))
;; Send email to someone
(add-to-list 'org-capture-templates
             '("ms" "Send Email" entry (file+olp "Mail.org" "Send Email")
               "* TODO Send Email to %? about \nSCHEDULED: %t DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))"))
