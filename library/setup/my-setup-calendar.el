;;; my-setup-calendar.el -*- lexical-binding: t -*-

(message "Setting up calendar packages...")

;;* gcal-add — Add events to Google Calendar via AI extraction
;; Package source and tests: ~/projects/elisp/gcal-add/
(defvar my-gcal-add-pi-model "claude-haiku-latest"
  "LLM model passed to the pi CLI backend for gcal-add.")

(use-package gcal-add
  :load-path "~/projects/elisp/gcal-add"
  :commands (gcal-add gcal-add-from-mu4e)
  :custom
  (gcal-add-default-calendar "REDACTED@example.com")
  (gcal-add-timezone "America/Chicago")
  :config
  ;; Pi CLI backend (uses gcal-add helpers for process management)
  (declare-function gcal-add--fresh-buffer "gcal-add")
  (declare-function gcal-add--make-cli-sentinel "gcal-add")
  (defun my-gcal-add-backend-pi (prompt callback)
    "Send PROMPT to pi CLI, call CALLBACK with the response.
Uses `my-gcal-add-pi-model' for the model."
    (unless (executable-find "pi")
      (user-error "`pi' CLI not found"))
    (let* ((tmpfile (make-temp-file "gcal-add-" nil ".txt"))
           (buf (gcal-add--fresh-buffer " *gcal-add-pi*")))
      (with-temp-file tmpfile (insert prompt))
      (condition-case err
          (set-process-sentinel
           (start-process "gcal-add-pi" buf "pi"
                          "--no-tools" "--no-session" "--no-extensions"
                          "--no-skills" "--thinking" "off"
                          "--model" my-gcal-add-pi-model
                          "-p" (concat "@" tmpfile))
           (gcal-add--make-cli-sentinel buf tmpfile callback))
        (error
         (when (file-exists-p tmpfile) (delete-file tmpfile))
         (when (buffer-live-p buf) (kill-buffer buf))
         (signal (car err) (cdr err))))))
  (setq gcal-add-backend 'my-gcal-add-backend-pi)

  (with-eval-after-load 'mu4e
    (defvar mu4e-view-actions)
    (defvar mu4e-headers-actions)
    (add-to-list 'mu4e-view-actions
                 '("Add to gcal" . gcal-add-from-mu4e) t)
    (add-to-list 'mu4e-headers-actions
                 '("Add to gcal" . gcal-add-from-mu4e) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-setup-calendar)
;;; my-setup-calendar.el ends here
