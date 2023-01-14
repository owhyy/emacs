;; Don't use custom file
(setq custom-file null-device)

;: GUI Tweaks
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode t)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 150)
(global-set-key (kbd "<f5>") 'modus-themes-toggle)

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(eshell-mode-hook
		term-mode-hook))
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

;; Org-specific configuration
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/me/time.org"))
(setq org-agenda-files '(org-default-notes-file))
(setq org-agenda-block-separator 8411)
(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %? \nSCHEDULED: %^t" :clock-in t :clock-resume t)
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "* NEXT %? \nDEADLINE: %t") ))

(use-package org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; Magit config
(use-package magit
  :ensure t
  )
