(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(column-number-mode)

(setq-default fill-column 80)
(global-display-fill-column-indicator-mode 1)

(set-face-attribute 'default nil :font "Iosevka-14")
(setq-default line-spacing 5)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq modus-themes-mode-line '(borderless))
(setq modus-themes-region '(bg-only))
(setq modus-themes-bold-constructs t)
(load-theme 'modus-vivendi t)

(global-visual-line-mode t)
(delete-selection-mode t)
(electric-pair-mode t)
(setq-default indent-tabs-mode nil)

;; Autosaves & Backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; org-mode packages

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
