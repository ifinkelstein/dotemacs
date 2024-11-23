;;; my-server.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein

;;; Commentary: Emacs Server setup

;; Emacs server needs to be running for magit to work, I think

;;; Code:
(message "Setting up the Emacs server...")

;;; Server

;; start server for emacsclient
(use-package server
  :ensure nil
  :if window-system
  :hook (after-init . server-mode)
  :defer 2
  :config
  ;; t/nil for instructions
  (setq server-client-instructions nil)
  ;; avoid warning screen
  ;; TODO: figure out why this gives an error
  (unless (server-running-p)
    (server-start)))

;; functions for killing server-related emacsen
(defun my-kill-all-emacsen ()
  (interactive)
  (progn
    (save-buffers-kill-emacs)
    (shell-command-to-string "pkill -i emacs")))

(defun my-kill-emacs-capture-daemon ()
  (interactive)
  (shell-command-to-string "pkill -f /Applications/Emacs.app/Contents/MacOS/emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-setup-server)
