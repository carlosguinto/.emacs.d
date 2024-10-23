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

;; Theme
(use-package timu-caribbean-theme
  :config (load-theme 'timu-caribbean t))

;; General Editing

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Org Config

(require 'org-bullets)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "|" "DONE")))

;; To add
;; org toc
;; org tempo
;; org roam


;; Software Development Packages
;; TODO
;; Rust, Python, Go, ASM, JS, TS, programming config

;; C config
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
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)))

;; Custom function to run hooks of non-ts mode counterparts.
(defun run-non-ts-hooks ()
  (let ((major-name (symbol-name major-mode)))
    (when (string-match-p ".*-ts-mode" major-name)
      (run-hooks (intern (concat (replace-regexp-in-string "-ts" "" major-name) "-hook"))))))

(add-hook 'prog-mode-hook 'run-non-ts-hooks)

;; Version Control
;; Magit
(use-package magit)

;; Helper Packages
(use-package which-key
  :config (which-key-mode))

;; Minibuffer Packages

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(timu-caribbean-theme yasnippet which-key rainbow-delimiters org-bullets magit lsp-ui kanagawa-themes doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
