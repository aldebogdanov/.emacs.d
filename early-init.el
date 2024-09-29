;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Aleksandr Bogdanov
;; Homepage: https://github.com/aldebogdanov/.emacs.d.got

;;; Commentary:
;; Emacs 29.4+ early initialization configuration.

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(provide 'early-init)
;;; early-init.el ends here
