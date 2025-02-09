;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Aleksandr Bogdanov
;; Homepage: https://github.com/aldebogdanov/.emacs.d.got

;; Commentary:
;; Emacs 29.4+ configuration.

;; Code:

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))

(use-package early-init
             :no-require
             :unless (featurep 'early-init)
             :config
             (load-file (locate-user-emacs-file "early-init.el")))

(use-package all-the-icons
  :ensure t)

(use-package clj-refactor
  :ensure t)

(use-package cljstyle-format
  :ensure t)

(use-package clojure-mode
  :ensure t
  :hook (((clojure-mode clojurec-mode clojurescript-mode) . my-clojure-mode-hook)
         (clojurescript-mode . (lambda ()
                                (setq-local cider-default-cljs-repl 'shadow))))
  :custom
  (clojure-toplevel-inside-comment-form t)
  :config
  (defun my-clojure-mode-hook ()
    (cider-mode 1)
    (clj-refactor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-e")
    (yas-minor-mode 1)
    (cljstyle-format-on-save-mode 1)
    (electric-pair-mode 1)
    (rainbow-delimiters-mode 1)))

(use-package cider
  :ensure t
  ;; :delight " CIDER"
  :commands cider-find-and-clear-repl-buffer
  :functions (cider-nrepl-request:eval
              cider-find-and-clear-repl-output
              cider-random-tip)
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         ;; (cider-repl-mode . common-lisp-modes-mode)
         (cider-popup-buffer-mode . cider-disable-linting))
  :bind ( :map cider-repl-mode-map
          ("C-c C-S-o" . cider-repl-clear-buffer)
          :map cider-mode-map
          ("C-c C-S-o" . cider-find-and-clear-repl-buffer)
          ("C-c C-p" . cider-pprint-eval-last-sexp-to-comment))
  :custom-face
  (cider-result-overlay-face ((t (:box (:line-width -1 :color "grey50")))))
  (cider-error-highlight-face ((t (:inherit flymake-error))))
  (cider-warning-highlight-face ((t (:inherit flymake-warning))))
  (cider-reader-conditional-face ((t (:inherit font-lock-comment-face))))
  :custom
  (nrepl-log-messages nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-tab-command #'indent-for-tab-command)
  (nrepl-hide-special-buffers t)
  (cider-test-show-report-on-success t)
  (cider-allow-jack-in-without-project t)
  (cider-use-fringe-indicators nil)
  (cider-font-lock-dynamically '(macro var deprecated))
  (cider-save-file-on-load nil)
  (cider-inspector-fill-frame nil)
  (cider-auto-select-error-buffer t)
  (cider-show-eval-spinner t)
  (nrepl-use-ssh-fallback-for-remote-hosts t)
  (cider-repl-history-file (expand-file-name "~/.cider-history"))
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  (cider-use-tooltips nil)
  (cider-connection-message-fn #'cider-random-tip)
  (cider-repl-prompt-function #'cider-repl-prompt-newline)
  (cider-auto-inspect-after-eval nil)
  (cider-enrich-classpath nil)
  (cider-cljs-lein-repl
   "(do (require 'shadow.cljs.devtools.api)
        (shadow.cljs.devtools.api/nrepl-select :app))")
  :config
  (put 'cider-clojure-cli-aliases 'safe-local-variable #'listp)
  (defun cider-disable-linting ()
    "Disable linting integrations for current buffer."
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1)))
  (defun cider-repl-prompt-newline (namespace)
    "Return a prompt string that mentions NAMESPACE with a newline."
    (format "%s\n> " namespace))
  (defun cider-find-and-clear-repl-buffer ()
    "Find the current REPL buffer and clear it.
See `cider-find-and-clear-repl-output' for more info."
    (interactive)
    (cider-find-and-clear-repl-output 'clear-repl))
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :hook (after-init . (lambda ()
                        (setq custom-safe-themes
                              '("04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f" default))
                        (color-theme-sanityinc-tomorrow-eighties)
                        (set-face-attribute 'default nil :height 100))))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; Increase performance
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil))

(use-package defaults
  :no-require
  :hook (prog-mode . display-line-numbers-mode)
  :preface
  (setq-default
   custom-file (concat user-emacs-directory "custom.el")
   indent-tabs-mode nil
   tooltip-mode nil
   load-prefer-newer t
   truncate-lines nil
   bidi-paragraph-direction 'left-to-right
   frame-title-format "Emacs"
   auto-window-vscroll nil
   mouse-highlight t
   hscroll-step 1
   ;; debug-on-error t
   desktop-save-mode 1
   hscroll-margin 1
   scroll-margin 0
   scroll-preserve-screen-position nil
   tab-bar-mode t
   tab-bar-history-mode t
   frame-resize-pixelwise window-system
   window-resize-pixelwise window-system)
  (when (eq system-type 'darwin)
    (setq-default
     mac-right-command-modifier 'control
     x-gtk-use-system-tooltips nil
     delete-by-moving-to-trash t))
  (when (window-system)
    (setq-default
     x-gtk-use-system-tooltips nil
     cursor-type 'box
     cursor-in-non-selected-windows nil))
  (setq
   ring-bell-function 'ignore
   mode-line-percent-position nil
   enable-recursive-minibuffers t)
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (provide 'defaults))

(use-package docker-compose-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
  :config
  (exec-path-from-shell-initialize))

(use-package files
  :preface
  (defvar backup-dir
    (locate-user-emacs-file ".cache/backups")
    "Directory to store backups.")
  (defvar auto-save-dir
    (locate-user-emacs-file ".cache/auto-save/")
    "Directory to store auto-save files.")
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist
   `(("." . ,backup-dir)))
  (auto-save-file-name-transforms
   `((".*" ,auto-save-dir t)))
  :init
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

(use-package flycheck-projectile
  :ensure t
  :custom
  (global-flycheck-mode +1))

(use-package flyspell
  :ensure t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

(use-package grep
  :config
  (setq grep-find-ignored-directories
        (append grep-find-ignored-directories
                '("node_modules" ".clj-kondo" ".git" ".lsp" ".cpcache" ".shadow-cljs"))))

(use-package gptel
  :ensure t)

(use-package emacs
  :config
  (global-set-key (kbd "M-<backspace>") 'backward-delete-word)

  (defun backward-delete-word (arg)
    "Delete characters backward until encountering the beginning of a word.
    This command does not affect the kill ring."
    (interactive "p")
    (delete-region (point) (progn (backward-word arg) (point)))))

(use-package idle-highlight-in-visible-buffers-mode
  :ensure t
  :hook ((prog-mode . idle-highlight-in-visible-buffers-mode)
         (idle-highlight-in-visible-buffers-mode . (lambda ()
                                                     (set-face-attribute 'idle-highlight-in-visible-buffers nil :weight 'bold)))))

;; ;;;; LSP

(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-configure nil)
  (lsp-diagnostics-provider :flymake)
  (lsp-completion-provider :none)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-xref t)
  (lsp-signature-doc-lines 1))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode-maybe))
  :commands (lsp-completion-mode)
  :preface
  (defun lsp-completion-mode-maybe ()
    (unless (bound-and-true-p cider-mode)
      (lsp-completion-mode 1))))

(use-package lsp-treemacs
  :ensure t
  :defer t
  :custom
  (lsp-treemacs-theme "Iconless"))

(use-package lsp-clojure
  :demand t
  :after lsp-mode
  :hook (cider-mode . cider-toggle-lsp-completion-maybe)
  :preface
  (defun cider-toggle-lsp-completion-maybe ()
    (lsp-completion-mode (if (bound-and-true-p cider-mode) -1 1))))

(use-package lsp-clojure
  :no-require
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . lsp))

(use-package lsp-java
  :ensure t
  :after lsp-mode)

(use-package lsp-java
  :hook (java-mode . lsp))

(use-package lsp-metals
  :ensure t
  :after lsp-mode
  :custom
  (lsp-metals-server-args
   '("-J-Dmetals.allow-multiline-string-formatting=off"
     "-J-Dmetals.icons=unicode"))
  (lsp-metals-enable-semantic-highlighting nil))

(use-package lsp-metals
  :hook (scala-mode . lsp))

(defun my/hide-lsp-ui-doc-on-click (&rest _)
  "Hide lsp-ui-doc unconditionally."
  (when (lsp-ui-doc--visible-p)
    (lsp-ui-doc-hide)))


(defun my/toggle-lsp-ui-doc ()
  (interactive)
  (if (lsp-ui-doc--visible-p)
    (lsp-ui-doc-hide)
    (lsp-ui-doc-show)))


(use-package lsp-ui
             :ensure t
             :config
             (setq lsp-ui-doc-position 'at-point)
             (setq lsp-ui-doc-show-with-cursor nil)
             (setq lsp-ui-doc-show-with-mouse nil)

             (advice-add 'mouse-set-point :before #'my/hide-lsp-ui-doc-on-click)

             :bind (:map lsp-ui-mode-map
                         ("C-c d" . lsp-ui-doc-show)
                         ("C-c h" . lsp-ui-doc-hide)
                         ("C-c t" . my/toggle-lsp-ui-doc)))


;;;; Magit

(use-package magit
  :ensure t
  :hook ((git-commit-mode . flyspell-mode)
         ;; (git-commit-mode . magit-git-commit-insert-branch)
         )
  :bind ( :map project-prefix-map
          ("m" . magit-project-status))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  ;; :preface
  ;; (defun magit-extract-branch-tag (branch-name)
  ;;   "Extract branch tag from BRANCH-NAME."
  ;;   (let ((ticket-pattern "\\([[:alpha:]]+-[[:digit:]]+\\)"))
  ;;     (when (string-match-p ticket-pattern branch-name)
  ;;       (upcase (replace-regexp-in-string ticket-pattern "\\1: \n" branch-name)))))
  ;; (defun magit-git-commit-insert-branch ()
  ;;   "Insert the branch tag in the commit buffer if feasible."
  ;;   (when-let ((tag (magit-extract-branch-tag (magit-get-current-branch))))
  ;;     (insert tag)
  ;;     (forward-char -1)))
  )

(use-package magit
  :after project
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit") t))

(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

(use-package markdown-mode
  :ensure t)

(use-package nginx-mode
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package prettier-js
  :ensure t
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

(use-package projectile
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(use-package puni
  :ensure t
  :bind (("C-M-="  . puni-expand-region)
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
	 ("C-M-;"  . puni-squeeze))
  ;; :hook (term-mode-hook . #'puni-disable-puni-mode)
  :init
  (puni-global-mode))

(use-package rainbow-delimiters
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :config
  (setq typescript-indent-level 2))

(use-package vterm
  :ensure t
  :after exec-path-from-shell)

(use-package wakatime-mode
  :ensure t
  :hook (after-init . global-wakatime-mode)
  :custom
  (wakatime-cli-path (expand-file-name "~/.wakatime/wakatime-cli")))

(use-package web-mode
  :ensure t
  :mode ("\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'")
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")
                                       ("tsx" . "\\.ts[x]?\\'"))))

(use-package window-numbering
  :ensure t
  :custom
  (windmove-default-keybindings 'meta)
  (window-numbering-mode t))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :defer t)

(provide 'init)
;;; init.el ends here
