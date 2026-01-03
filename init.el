;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Aleksandr Bogdanov
;; Homepage: https://github.com/aldebogdanov/.emacs.d.got

;;; Commentary:
;; Emacs 29.4+ configuration adapted for Overtone/Clojure.

;;; Code:

;;; 1. Package Management & Defaults
;; -----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Initialize use-package on non-Linux platforms or if missing
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Default to ensuring packages are installed

;;; 2. Performance & Audio (Crucial for Overtone)
;; -----------------------------------------------------------------------------

;; GCMH (Garbage Collector Magic Hack)
;; Crucial for audio: Prevents GC pauses while you are typing or triggering sounds.
(use-package gcmh
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024)
        gcmh-verbose nil)
  :config
  (gcmh-mode 1))

;; Restore sane GC limits after startup (handled by gcmh largely, but safe fallback)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;;; 3. Core UI & Experience
;; -----------------------------------------------------------------------------

(use-package emacs
  :init
  ;; Clean UI
  (setq inhibit-startup-message t
        ring-bell-function 'ignore
        use-short-answers t)     ; y/n instead of yes/no
  (global-display-line-numbers-mode t)
  
  ;; Encoding
  (set-language-environment "UTF-8")

  ;; Files & Backups
  (setq make-backup-files nil    ; Stop creating ~ files
        auto-save-default nil    ; Stop creating # files
        create-lockfiles nil)    ; Stop creating .# files

  ;; Tab bar (as per your previous config)
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  
  ;; Mac/System specifics
  (when (eq system-type 'darwin)
    (setq mac-right-command-modifier 'control
          delete-by-moving-to-trash t))
  
  ;; Better Scrolling (Emacs 29+)
  (pixel-scroll-precision-mode 1))

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(use-package all-the-icons
  :if (display-graphic-p))

;; Highlight delimiters (Essential for Lisp)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Visual Undo
(use-package vundo
  :bind ("C-x u" . vundo))

;;; 4. Modern Completion Stack (Vertico + Corfu)
;; -----------------------------------------------------------------------------

;; Minibuffer completion (Files, M-x, etc)
(use-package vertico
  :init
  (vertico-mode))

;; Better annotations for Minibuffer (e.g. file descriptions)
(use-package marginalia
  :init
  (marginalia-mode))

;; Fuzzy matching styles
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; In-buffer completion (Replaces Company for a more modern feel)
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                 ; Enable auto completion
  (corfu-cycle t)                ; Cycle through candidates
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-mode t)       ; Show documentation in popup
  (corfu-popupinfo-delay 0.5))

;; Add extensions for Corfu
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; 5. Clojure & Overtone Setup
;; -----------------------------------------------------------------------------

(use-package clojure-mode
  :config
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :bind (:map cider-mode-map
         ("C-c C-p" . cider-pprint-eval-last-sexp-to-comment))
  :custom
  ;; Overtone/Live Coding optimizations
  (cider-repl-pop-to-buffer-on-connect nil) ; Don't steal focus when connecting
  (cider-show-error-buffer nil)             ; Don't pop up error buffer automatically
  (cider-auto-select-error-buffer nil)
  (cider-repl-display-help-banner nil)
  (cider-font-lock-dynamically '(macro var deprecated))
  (cider-use-fringe-indicators nil)         ; Clean UI
  :config
  ;; Allow jack-in without project (useful for quick scratchpads)
  (setq cider-allow-jack-in-without-project t))

;; Structured Editing (Puni is great, kept it)
(use-package puni
  :hook ((prog-mode . puni-mode)
         (term-mode . puni-disable-puni-mode))
  :bind (:map puni-mode-map
         ("C-M-="  . puni-expand-region)
	       ("C-M--"  . puni-contract-region)
	       ("C-M-+"  . puni-mark-sexp-around-point)
	       ("C-M-_"  . puni-mark-sexp-at-point)
	       ("C-M-|"  . puni-mark-list-around-point)
	       ("C-M-]"  . puni-slurp-forward)
	       ("C-M-["  . puni-slurp-backward)
	       ("C-M-}"  . puni-barf-forward)
	       ("C-M-{"  . puni-barf-backward)
	       ("C-M-'"  . puni-raise)
	       ("C-M-\"" . puni-convolute)
	       ("C-M-;"  . puni-squeeze)))

;;; 6. LSP (Language Server Protocol)
;; -----------------------------------------------------------------------------
;; Kept lsp-mode as it handles Java/Clojure heavy-lifting well.

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((clojure-mode . lsp-deferred)
         (clojurec-mode . lsp-deferred)
         (clojurescript-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (scala-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-headerline-breadcrumb-enable nil)  ; Clean UI
  (lsp-lens-enable t)                     ; Show reference counts
  (lsp-enable-symbol-highlighting t)
  (lsp-idle-delay 0.5)
  (lsp-clojure-custom-server-command nil) ; Ensure it uses automatic path or define manually
  :config
  ;; Integrate with Orderless/Corfu
  (setq lsp-completion-provider :none)
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))
  (add-hook 'lsp-completion-mode-hook #'corfu-lsp-setup))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable nil)) ;; Disable sideline for cleaner live-coding view

(use-package lsp-java
  :after lsp-mode)

(use-package lsp-metals
  :after lsp-mode)

;;; 7. Tools & Utils
;; -----------------------------------------------------------------------------

(use-package magit
  :commands magit-status)

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package wakatime-mode
  :config (global-wakatime-mode))

(use-package vterm
  :commands vterm)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Formatter (Prettier for JS/TS)
(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :mode ("\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'" "\\.html\\'"))

(use-package dockerfile-mode)
(use-package terraform-mode)
(use-package yaml-mode)

(provide 'init)
;;; init.el ends here