;;; my-setup-server.el  -*- lexical-binding: t; -*-
;; start server for emacsclient
;; taken from: https://github.com/Lambda-Emacs/lambda-emacs/blob/main/lambda-library/lambda-setup/lem-setup-server.el

(message "Setting up emacs server...")

(use-package server
  :ensure nil
  :defer 1
  :custom
  ;; t/nil for instructions
  (server-client-instructions nil)
  :config
  ;; avoid warning screen
  (or (server-running-p)
      (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-setup-server)
