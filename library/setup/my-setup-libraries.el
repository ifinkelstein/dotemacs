;; my-setup-libraries.el -*- lexical-binding: t -*-

;;; Commentary:

;; https://github.com/jwiegley/emacs-async, https://github.com/magnars/s.el,
;; https://github.com/magnars/dash.el http://elpa.gnu.org/packages/cl-lib.html
;; Included are built-in libraries and those for asynchronous processing string
;; manipulation, list manipulation and backward compatibility respectively.

(message "Setting up core libraries...")

;;* Built in libraries; lots of packages depend on these
(use-package subr-x
  :ensure nil
  :defer 1)

(use-package cl-lib
  :ensure nil
  :defer t)

;;* Include this only for compatibility purposes
(use-package cl
  :ensure nil
  :defer t)

;;* Asynchronous commands.
;; Adds the ability to call asynchronous functions and
;; process with ease. See the documentation for `async-start' and
;; `async-start-process'.
(use-package async
  :defer
  :config
  (dired-async-mode 1)
  (setq dired-async--modeline-mode nil))

;;* A modern list API for Emacs. No 'cl required.
(use-package dash
  :defer 2)

;;* String manipulation
(use-package s
  :defer 2)

;;* APIs for files & directories
(use-package f
  :defer 2)

;;*
(provide 'my-setup-libraries)
;;; my-setup-libraries.el ends here
