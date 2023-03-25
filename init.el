;; Don't use custom file
(setq custom-file null-device)

;; Don't create lockfiles
(setq create-lockfiles nil)

;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;: GUI Tweaks
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode t)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<f5>") 'modus-themes-toggle)

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(eshell-mode-hook
		term-mode-hook
		pdf-view-mode-hook
		olivetti-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-z"))

;; Package configuration
(require 'package)
(setq package-archives '(("mepla". "https://melpa.org/packages/")
 			 ("org". "https://orgmode.org/elpa/")
 			 ("elpa". "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Installs use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; Ensures all packages are installed before running
(setq use-package-always-ensure t)

;; Theme
(use-package modus-themes)
(load-theme 'modus-vivendi :no-confirm)
;; Fonts
(use-package all-the-icons)

;; Autocompletion
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)))
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)
(use-package magit)

(use-package olivetti
  :bind (("C-x z" . olivetti-mode)))

;; Org-specific configuration
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-directory "~/org")
(setq org-agenda-files '("~/org/work.org" "~/org/me.org" "~/org/notes.org" "~/org/posts.org"))
(setq org-agenda-block-separator 8411)
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
(setq org-todo-keywords '("TODO(t)" "NEXT(n)" "LATER(l)" "MERGE(m)" "|" "CANCELLED(c)" "DONE(d)"))
(setq org-todo-keyword-faces
      '(("CANCELLED" . "gray")
	("LATER" . "blue")
	 ("MERGE" . "pink")))
(setq org-capture-templates
      '(("t" "todo" entry (file "~/org/notes.org")
	 "* TODO %?\nDEADLINE: %^t")
	("w" "work-related todo" entry (file "~/org/work.org")
	 "* TODO %? \nSCHEDULED: %^t")
	("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	   "** NEXT %? \nDEADLINE: %t")))

(defvar prot-org-custom-daily-agenda
    '((agenda "" ((org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
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
                (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n"))))
  "Custom agenda for use in `org-agenda-custom-commands'.")

(setq org-agenda-custom-commands
      `(("A" "Daily agenda and top priority tasks"
         ,prot-org-custom-daily-agenda)
	("W" "Work-related tasks" tags-todo "work")
	("C" "Computer-science-related tasks" tags-todo "cs")
	("S" "School-related tasks" tags-todo "school")))
(setq org-tags-column 0) ;; make tags be closer to text
(setq org-agenda-tags-column 0) ;; same thing but in agenda view

(use-package ox-hugo)
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package pdf-tools)
(use-package nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(pdf-tools-install)
(use-package org-superstar)

(windmove-default-keybindings)
(winner-mode)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-loop-over-headlines-in-active-region t)
(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
