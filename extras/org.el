;;; Emacs Bedrock
;;;
;;; Extra config: Org-mode starter config

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; Org-mode is a fantastically powerful package. It does a lot of things, which
;;; makes it a little difficult to understand at first.
;;;
;;; We will configure Org-mode in phases. Work with each phase as you are
;;; comfortable.
;;;
;;; YOU NEED TO CONFIGURE SOME VARIABLES! The most important variable is the
;;; `org-directory', which tells org-mode where to look to find your agenda
;;; files.

;;; See "org-intro.txt" for a high-level overview.

;;; Contents:
;;;
;;;  - Critical variables
;;;  - Phase 1: editing and exporting files
;;;  - Phase 2: todos, agenda generation, and task tracking
;;;  - Phase 3: extensions (org-roam, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables need to be set for Org-mode's full power to be unlocked!
;;;
;;; You can read the documentation for any variable with `C-h v'. If you have
;;; Consult configured (see the `base.el' file) then it should help you find
;;; what you're looking for.

;; Agenda variables
(setq org-directory "~/org/")

(setq org-agenda-files '("inbox.org" "work.org" "notes.org"))

;; Default tags
;; TODO: figure out if i can somehow group them to start with a key
;; like, for projects, i want to first press p, and then each of the key, and so on.
(setq org-tag-alist '(
		      (:startgrouptag)
		      ("work")
		      (:grouptags)
		      ("meapp" . ?m)
		      ("gorgias" . ?g)
		      ("taxfix" . ?t)
		      ("codereview" . ?r)
		      (:endgrouptag)
     		      (:endgrouptag)
		      (:newline)
                      ;; locale
                      (:startgroup)
                      ("home" . ?h)
                      ("school" . ?s)
		      ("selfstudy" . ?S)
                      (:endgroup)
                      (:newline)
                      ;; scale
                      (:startgroup)
                      ("project" . ?j)
                      ("onetime" . ?O)
                      (:endgroup)
                      ;; misc
                      ("review")
		      ("paper")
		      ("buy" . ?b)
		      ("checkout" . ?c)
		      ("online" . ?i)
		      ("offline" . ?o)
                      ("reading" . ?R)))

;; Apply commands over headlines in active region only
(setq org-loop-over-headlines-in-active-region t)

;; Refile up to 9 headings deep
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

;;; Optional variables

;; Advanced: Custom link types
;; This example is for linking a person's 7-character ID to their page on the
;; free genealogy website Family Search.
(setq org-link-abbrev-alist
      '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))

;; Allow converting to markdown using hugo
(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j")
  :bind (:map global-map
        ("C-c d" . org-journal-new-entry))
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"))

;; Not really org-related but still. Better pdf viewer than the default docview
(use-package pdf-tools
  :config
  (pdf-tools-install))

;; Epub viewer
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package org
  :hook ((org-mode . visual-line-mode))  ; wrap lines at word breaks

  :bind (:map global-map
	      ("C-c a" . org-agenda)
	      ("C-c c" . org-capture)
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config
  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)

  ;; Log time at which task was finished
  (setq org-log-done t)
  
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IDEA(i)" "WAITING(w@/!)" "STARTED(s!)" "LATER(l@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
  
  (setq org-todo-keyword-faces
	'(("CANCELLED" . "gray")
	  ("LATER" . "blue")
	  ("WAITING" . "cyan")
	  ("IDEA" . "magenta")
	  ("STARTED" . "yellow")))

	;; Refile configuration
	(setq org-outline-path-complete-in-steps nil)
	(setq org-refile-use-outline-path 'file)

	(setq org-capture-templates
              '(("t" "Default Capture" entry (file "inbox.org")
		 "* TODO %?\n:CREATED: %U\n%i")
		("w" "work-related todo" entry (file+olp "work.org" "EBS Integrator" "2023" "Tasks")
		 "* TODO %?\n:CREATED: %U\n%i")
		("i" "idea" entry (file "inbox.org")
		 "* IDEA %?\n:CREATED: %U\n%i")))

	(defvar prot-org-custom-daily-agenda
	  '((agenda "" ((org-agenda-span 1)
			(org-deadline-warning-days 0)
			(org-deadline-past-days 0)
			(org-agenda-block-separator nil)
			(org-scheduled-past-days 0)
			(org-agenda-show-log t)
			;; We don't need the `org-agenda-date-today'
			;; highlight because that only has a practical
			;; utility in multi-day views.
			(org-agenda-day-face-function (lambda (date) 'org-agenda-date))
			(org-agenda-format-date "%A %-e %B %Y")
			(org-agenda-overriding-header "Today's agenda\n")))
	    (agenda "" ((org-agenda-start-on-weekday nil)
			(org-agenda-start-day "+1d")
			(org-agenda-span 3)
			(org-deadline-warning-days 0)
			(org-agenda-block-separator nil)
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			(org-agenda-overriding-header "\nNext three days\n")))
	    (agenda "" ((org-agenda-start-on-weekday nil)
			;; We don't want to replicate the previous section's
			;; three days, so we start counting from the day after.
			(org-agenda-start-day "+4d")
			(org-agenda-span 14)
			(org-agenda-show-all-dates nil)
			(org-deadline-warning-days 0)
			(org-agenda-block-separator nil)
			(org-agenda-entry-types '(:deadline))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			(org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))
	    (agenda "" ((org-agenda-overriding-header "Overdue")
			(org-agenda-time-grid nil)
			(org-agenda-start-on-weekday nil)
			(org-agenda-show-all-dates nil)
			(org-agenda-format-date "")  ;; Skip the date
			(org-agenda-span 1)
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			(org-agenda-entry-types '(:deadline :scheduled))
			(org-scheduled-past-days 999)
			(org-deadline-past-days 999)
			(org-deadline-warning-days 0)))
	    (tags-todo "work+SCHEDULED=\"<today>\""
		       ((org-agenda-overriding-header "\nToday's work to do\n")
			(org-agenda-span 'day)
			(org-scheduled-past-days 0))))
	  "Custom agenda for use in `org-agenda-custom-commands'.")

	(setq org-agenda-custom-commands
	      `(("A" "Daily agenda and top priority tasks"
		 ,prot-org-custom-daily-agenda)
		("w" "Work" agenda ""
		 ((org-agenda-files '("work.org")))))))
  
