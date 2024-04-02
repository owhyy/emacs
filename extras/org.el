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
;;;   critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; these variables need to be set for Org-mode's full power to be unlocked!
;;;
;;; You can read the documentation for any variable with `C-h v'. If you have
;;; Consult configured (see the `base.el' file) then it should help you find
;;; what you're looking for.

;; Agenda variables
(setq org-directory "~/org/")

(setq org-agenda-files '("inbox.org" "work.org" "notes.org" "projects.org"))

;; Apply commands over headlines in active region only
(setq org-loop-over-headlines-in-active-region t)

(setq org-image-actual-width nil)

;; Refile up to 9 headings deep
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

;; Allow converting to markdown using hugo
(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

;; Allow journaling in emacs
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
  :ensure t
  :config
  (pdf-tools-install))

;; Epub viewer
(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Prettier bullets
(use-package org-bullets
  :ensure t
  :hook ((org-mode . org-bullets-mode)))

(use-package org
  :hook ((org-mode . visual-line-mode))  ; wrap lines at word breaks

  :bind (:map global-map
	      ("C-c a" . org-agenda)
	      ("C-c c" . org-capture)
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config
  (require 'oc-csl)                     ; citation support
  ;; Allow exporting in markdown
  (add-to-list 'org-export-backends 'md)

  ;; Custom effort: 15 minutes, 1 hour or 4 hours
  (customize-set-variable 'org-global-properties
                          '(("Effort_ALL" . "0 0:15 1:00 4:00")))
  
  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)

  ;; Log time at which task was finished
  (setq org-log-done t)

  ;; Show effort next to task
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                                   (todo . " %i %-12:c %-6e")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))
  
  ;; Set states that task can be in: CANCELLED and WAITING
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELLED(c@/!)" "WAITING(W@)")))

  ;; Set colors for the keywords
  (setq org-todo-keyword-faces
	'(("CANCELLED" . "gray")
	  ("WAITING" . "blue")))

	;; Refile configuration
	(setq org-outline-path-complete-in-steps nil)
	(setq org-refile-use-outline-path 'file)

	;; Create entries by using C-c c i for inbox and C-c c n for notes
	(setq org-capture-templates
              '(("i" "Inbox" entry (file "inbox.org")
		 "* TODO %?\n:CREATED: %U\n%i")
		("n" "Note" entry (file "raw-notes.org")
		 "* %?\n:CREATED: %U\n%i")))
	
	;; Configure org-agenda view
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
			(org-scheduled-delay-days 1)
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
	    (todo "TODO" ((org-agenda-overriding-header "Inbox")
			  (org-agenda-skip-function '(org-agenda-skip-entry-if 'done 'scheduled 'deadline)))))

	  "Custom agenda for use in `org-agenda-custom-commands'.")

	(setq org-agenda-custom-commands
	      `(("A" "Daily agenda and top priority tasks"
		 ,prot-org-custom-daily-agenda))))
  
