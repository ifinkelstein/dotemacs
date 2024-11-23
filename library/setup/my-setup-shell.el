;;; my-setup-shell.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein

;;; Commentary: Emacs Shell setup

;; borrowed from https://github.com/Lambda-Emacs/lambda-emacs/blob/main/lambda-library/lambda-setup/lem-setup-shell.el

;;; Code:
(message "Setting up the Emacs shells...")

;;; vterm and helpers
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :commands vterm-toggle)

;;; Tramp
;; An easy way to manage files over ssh/scp
(use-package tramp
  :ensure nil
  :defer 1
  :config
  (setq tramp-persistency-file-name (concat my-cache-dir "tramp")
        ;; the most reliable tramp setup I have found (used at work every day...)
        tramp-default-method "ssh"
        tramp-copy-size-limit nil
        tramp-use-ssh-controlmaster-options nil))

;; I recommend the following ~/.ssh/config settings be used with the tramp settings in this cfg:
;; Host *
;; ForwardAgent yes
;; AddKeysToAgent yes
;; ControlMaster auto
;; ControlPath ~/.ssh/master-%r@%h:%p
;; ControlPersist yes
;; ServerAliveInterval 10
;; ServerAliveCountMax 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-setup-shell)
