;; Speed up initial startup
(setq
 gc-cons-threshold (* 100 1024 1024)
 gc-cons-percentage 0.6
 read-process-output-max (* 1024 1024))

(setq
 ring-bell-function 'ignore
 inhibit-splash-screen t
 display-line-numbers-type 'relative
 make-backup-files nil
 auto-save-default nil
 initial-scratch-message nil
 recentf-max-saved-items 50
 confirm-kill-emacs 'y-or-n-p
 password-cache t
 password-cache-expiry 3600)

(setq-default
 fill-column 80
 line-spacing 5
 indent-tabs-mode nil
 cursor-in-non-selected-windows nil)

(set-frame-parameter nil 'alpha-background 100)
(add-to-list 'default-frame-alist '(alpha-background . 100))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode)
(global-display-line-numbers-mode t)
(column-number-mode)
(recentf-mode)
(global-visual-line-mode t)
(delete-selection-mode t)
(electric-pair-mode t)
(global-display-fill-column-indicator-mode 1)
(fido-vertical-mode)

;; Font setting
(set-face-attribute 'default nil :font "Iosevka-14")

;; Default maximize the screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Themes
;; (use-package gruvbox-theme
;;   :config
;;   (load-theme 'gruvbox-dark-hard t))

(use-package timu-caribbean-theme
  :config
  (load-theme 'timu-caribbean t))

;; General Editing

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Org Config

(require 'org-bullets)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "|" "DONE")))

;; Org Tempo
(require 'org-tempo)

;; Org Table of Contents
(use-package toc-org
  :commands toc-org-enable
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))

;; Org Roam
(use-package org-roam
  :custom
  (org-roam-directory "~/vinci/roam-notes")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  )

;; Org Roam Dependencies
(use-package dash)
(use-package f)
(use-package s)
(use-package emacsql)
(use-package magit-section)

;; Software Development Packages
;; TODO
;; Rust, Python, Go, ASM, JS, TS, programming config

;; Elisp
(use-package paredit)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode t)
            (rainbow-delimiters-mode t)
            (show-paren-mode t)))

(add-hook 'lisp-interaction-mode
          (lambda ()
            (paredit-mode t)
            (rainbow-delimiters-mode t)
            (show-paren-mode t)))

;; IELM setting to allow multiline input.
;; M-RET sends the current input to IELM process.
(require 'ielm)

(defun ielm/clear-repl ()
  "Clear current REPL buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ielm-send-input)))

(define-key inferior-emacs-lisp-mode-map
            (kbd "M-RET")
            #'ielm-return)

(define-key inferior-emacs-lisp-mode-map
            (kbd "C-j")
            #'ielm-return)

(define-key inferior-emacs-lisp-mode-map
            (kbd "RET")
            #'electric-newline-and-maybe-indent)

(define-key inferior-emacs-lisp-mode-map
            (kbd "<up>")
            #'previous-line)

(define-key inferior-emacs-lisp-mode-map
            (kbd "<down>")
            #'next-line)

(define-key inferior-emacs-lisp-mode-map
            (kbd "C-c C-q")
            #'ielm/clear-repl)

;; Completion
(use-package company
  :after lsp-mode
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(add-hook 'after-init-hook 'global-company-mode)

;; C
(defun vinci/c-setup ()
  "C programming configuration"
  (setq-local c-default-style "linux"
              tab-width 4
              c-basic-offset 4
              indent-tabs-mode nil)
  (setq-local c-ts-mode-indent-style "linux"
              c-ts-mode-indent-offset 4))

(add-hook 'c-mode-hook 'vinci/c-setup)
(add-hook 'c-ts-mode-hook 'vinci/c-setup)
(add-hook 'c++-mode-hook 'vinci/c-setup)
(add-hook 'c++-ts-mode-hook 'vinci/c-setup)

;; Python
(use-package lsp-pyright
  :custom
  (lsp-pyright-langserver-command "pyright")
  :hook
  (python-ts-mode . (lambda ()
                      (require 'lsp-pyright)
                      (lsp))))

;; Dart
(use-package dart-mode)
(use-package lsp-dart
  :after lsp)

;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  :hook
  ((c-ts-mode . lsp)
   (bash-ts-mode . lsp)
   (dart-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-idle-delay 0.1))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; Snippets
(use-package yasnippet
  :config
  (yas-reload-all)
  :hook
  ((lsp-mode . yas-minor-mode)))

;; Installing a tree-sitter parser
;; Run "M-x treesit-install-language-grammar", then follow the instructions.
;; Tree-sitter config
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (python-mode . python-ts-mode)))

;; Custom function to run hooks of non-ts mode counterparts.
(defun run-non-ts-hooks ()
  (let ((major-name (symbol-name major-mode)))
    (when (string-match-p ".*-ts-mode" major-name)
      (run-hooks (intern (concat (replace-regexp-in-string "-ts" "" major-name) "-hook"))))))

(add-hook 'prog-mode-hook 'run-non-ts-hooks)

;; Version Control
;; Magit
(use-package magit)

;; Shell/Terminal
;; VTerm
(use-package vterm)

;; Helper Packages
(use-package which-key
  :config (which-key-mode))
