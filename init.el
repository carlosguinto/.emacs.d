;; Speed up initial startup
(setq
 gc-cons-threshold 402653184
 gc-cons-percentage 0.6)

(setq
 ring-bell-function 'ignore
 inhibit-splash-screen t
 display-line-numbers-type t
 make-backup-files nil
 auto-save-default nil
 initial-scratch-message nil
 initial-major-mode 'eshell-mode
 recentf-max-saved-items 50
 confirm-kill-emacs 'y-or-n-p
 password-cache t
 password-cache-expiry 3600)

(setq-default
 fill-column 80
 line-spacing 5
 indent-tabs-mode nil
 cursor-in-non-selected-windows nil)

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

;; theme
(use-package kanagawa-themes
  :config (load-theme 'kanagawa-wave t))

;; general editing config

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; org-mode packages

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; To add
;; org toc
;; org tempo
;; org roam


;; TODO
;; C, Rust, Python, Go, ASM, JS, TS, programming config

;; C config
(defun vinci/c-setup ()
  "C programming configuration"
  (c-set-style "linux")
  (setq-local tab-width 4)
  (setq-local c-basic-offset 4))

(add-hook 'c-mode-hook 'vinci/c-setup)
(add-hook 'c-ts-mode-hook 'vinci/c-setup)
(add-hook 'c++-mode-hook 'vinci/c-setup)
(add-hook 'c++-ts-mode-hook 'vinci/c-setup)


;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((c-ts-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

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
(defun run-non-ts-hooks ()
  (let ((major-name (symbol-name major-mode)))
    (when (string-match-p ".*-ts-mode" major-name)
      (run-hooks (intern (concat (replace-regexp-in-string "-ts" "" major-name) "-hook"))))))

(add-hook 'prog-mode-hook 'run-non-ts-hooks)

;; magit
(use-package magit)

;; which-key
(use-package which-key
  :config (which-key-mode))
