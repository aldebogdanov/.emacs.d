;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Aleksandr Bogdanov
;; Homepage: https://github.com/aldebogdanov/.emacs.d.got

;;; Commentary:
;; Emacs 29.4+ early initialization.
;; Focuses on UI flicker prevention and startup performance.

;;; Code:

;; Defer garbage collection during startup for speed
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; In Emacs 27+, package initialization occurs before `user-init-file' is loaded,
;; but after `early-init-file'. We disable it here to control it in `init.el`.
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Resizing the Emacs frame can be expensive. Inhibiting this halves startup times.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements immediately (also in init.el, but good here for safety)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here