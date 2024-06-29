;; Enable MELPA
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package recentf
  :defer 1
  :config
  (recentf-mode 1))

;; Set theme
(use-package modus-themes
  :ensure t
  :init
  (load-theme 'modus-vivendi t)
  :bind ("<f5>" . modus-themes-toggle))

;; EPUB reader
(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode)))

;; Better pdf reader
(use-package pdf-tools
  :ensure t
  :config
  (pdf-loader-install))

;; Set font
(set-frame-font "Iosevka Comfy 10" nil t)

;; Don't blink cursor
(blink-cursor-mode -1)

;; Repeat action by pressing same key
(repeat-mode)

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save buffer and command history
;; Why do i need this? Vertico uses it
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control)

;; Show line and colun
(setq line-number-mode t)
(setq column-number-mode t)

;; See what this does
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)

(use-package expand-region
  :ensure t
  :config
  (require 'expand-region)
  :bind ("C-=" .  'er/expand-region))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-scroll-margin 0)
  (setq vertico-count 5)
  (setq vertico-resize t)
  (setq vertico-cycle t))

;; Cycle through completions if there's only 3 left
(setq completion-cycle-threshold 3)

;; Show docstrings when completing for describe-* functions
(setq completions-detailed t)

;; Show line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)

;; Enable line wrapping
(add-hook 'text-mode-hook 'visual-line-mode)

;; Highlight lines in hl-line mode
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Support opening new minibuffers from inside existing minibuffers
(setq enable-recursive-minibuffers t)

;; Hide commands in M-x which do not work in the current mode
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Use the `orderless' completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40))

(use-package minibuffer
  :ensure nil
  :config
  (setq completion-styles '(basic substring initials flex orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles . (basic partial-completion orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless))))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s s" . consult-line)       ; Alternative: rebind C-s to use
	 ("M-s f" . consult-find)
	 ("C-r" . consult-history)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s s" . consult-line)            ; needed by consult-line to detect isearch
	 ("M-s f" . consult-find)
         ))

(use-package avy
  :ensure t
  :bind ("C-c f" . avy-goto-char-timer))

(use-package python-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.py[iw]?\\'" . python-mode)))

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(set-default 'tab-always-indent 'complete)
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package eglot
  :ensure nil
  :functions (eglot-ensure)
  :commands (eglot)
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t))

(use-package emacs
  :config
  (setq major-mode-remap-alist
 '((js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (python-mode . python-ts-mode)
   (go-mode . go-ts-mode))))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

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
        '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d!)" "CANCELLED(c@/!)")))

  (setq org-directory "~/org/")

  (setq org-agenda-files '("inbox.org" "work.org" "notes.org" "projects.org"))

  ;; Apply commands over headlines in active region only
  (setq org-loop-over-headlines-in-active-region t)

  (setq org-image-actual-width nil)

  ;; Refile up to 9 headings deep
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  
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
		 "* %?\n:CREATED: %U\n%i")
		("c" "Contact" entry (file+headline "~/org/contacts.org" "Contacts")
		 "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:"
		 :empty-lines 1)))
	
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

;; (use-package org-journal
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; Change default prefix key; needs to be set before loading org-journal
;;   (setq org-journal-prefix-key "C-c j")
;;   :bind (:map global-map
;;         ("C-c d" . org-journal-new-entry))
;;   :config
;;   (setq org-journal-dir "~/org/journal/"
;;         org-journal-date-format "%A, %d %B %Y"))

(use-package ruff-format
  :ensure t
  :hook
  ((python-mode . ruff-format-on-save-mode)))

(use-package activity-watch-mode
  :ensure t
  :config
  (global-activity-watch-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2" default))
 '(package-selected-packages
   '(verb activity-watch-mode ruff-format expand-region pyvenv pyenv-mode org-journal elixir-mode php-mode json-mode python-mode magit corfu go-mode ef-themes add-node-modules-path avy consult marginalia which-key vertico pdf-tools orderless nov modus-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
