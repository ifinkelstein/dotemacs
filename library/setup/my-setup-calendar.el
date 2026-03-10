;;; my-setup-calendar.el -*- lexical-binding: t -*-

(message "Setting up calendar packages...")

;;* gcal-add — Add events to Google Calendar via AI extraction
;; Package source and tests: ~/projects/elisp/gcal-add/
(defvar my-gcal-add-pi-model "claude-haiku-latest"
  "LLM model passed to the pi CLI backend for gcal-add.")

(use-package gcal-add
  :load-path "~/projects/elisp/gcal-add"
  :commands (gcal-add)
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
  (setq gcal-add-backend 'my-gcal-add-backend-pi))

;;* mu4e DWIM calendar action
;; Registered eagerly (outside use-package :config) so the action
;; appears in mu4e menus immediately.  Calling `gcal-add' inside the
;; function triggers the autoload, which runs :config above.
(with-eval-after-load 'mu4e
  (defvar mu4e-view-actions)
  (defvar mu4e-headers-actions)
  (declare-function mu4e-message-readable-path "mu4e-message")
  (declare-function mm-dissect-buffer "mm-decode")
  (declare-function mm-destroy-parts "mm-decode")
  (declare-function mm-find-part-by-type "mm-decode")
  (declare-function gnus-icalendar-event-from-handle "gnus-icalendar")
  (declare-function gnus-icalendar-event:summary "gnus-icalendar")
  (declare-function gnus-icalendar-event:start-time "gnus-icalendar")
  (declare-function gnus-icalendar-event:end-time "gnus-icalendar")
  (declare-function gnus-icalendar-event:location "gnus-icalendar")
  (declare-function gnus-icalendar-event:description "gnus-icalendar")
  (declare-function gcal-add--format-summary "gcal-add")
  (declare-function gcal-add--run-gws "gcal-add")

  (defun my-mu4e--ics-handle-to-event-plist (handle)
    "Parse ICS HANDLE via gnus-icalendar into a gcal-add event plist.
Returns a plist compatible with `gcal-add--format-summary' and
`gcal-add--run-gws', or nil if parsing fails."
    (when-let* ((event (gnus-icalendar-event-from-handle handle))
                (summary (gnus-icalendar-event:summary event))
                (start (gnus-icalendar-event:start-time event))
                (end (gnus-icalendar-event:end-time event)))
      (let ((loc (gnus-icalendar-event:location event))
            (desc (gnus-icalendar-event:description event)))
        (when (and desc (not (string-empty-p desc)))
          (setq desc (truncate-string-to-width
                      (replace-regexp-in-string "<[^>]+>" "" desc) 500)))
        (list :title summary
              :date (format-time-string "%Y-%m-%d" start)
              :start_time (format-time-string "%H:%M" start)
              :end_time (format-time-string "%H:%M" end)
              :location (and loc (not (string-empty-p loc)) loc)
              :description (and desc (not (string-empty-p desc)) desc)))))

  (defun my-mu4e-action-add-to-calendar (msg)
    "Add event to calendar from MSG — DWIM.

If MSG has a text/calendar (or application/ics) MIME part, parse
it with `gnus-icalendar-event-from-handle' and add the event to
Google Calendar via gws.  Otherwise, extract text from the message
and send it through `gcal-add' to create the event via LLM.

Both paths create events in Google Calendar via `gcal-add--run-gws'.
Works from headers and view mode."
    (let* ((path (mu4e-message-readable-path msg))
           (parts (with-temp-buffer
                    (insert-file-contents-literally path nil nil nil t)
                    (ignore-errors (run-hooks 'gnus-article-decode-hook))
                    (mm-dissect-buffer t t)))
           (handle (or (mm-find-part-by-type (list parts) "text/calendar" nil t)
                       (mm-find-part-by-type (list parts) "application/ics" nil t)))
           (event-plist (when handle (my-mu4e--ics-handle-to-event-plist handle))))
      (mm-destroy-parts parts)
      (if event-plist
          ;; ICS parsed — confirm and add via gcal-add's shared execution path.
          (let ((summary (gcal-add--format-summary event-plist)))
            (if (y-or-n-p (format "Add to %s?\n%s\n"
                                  gcal-add-default-calendar summary))
                (gcal-add--run-gws event-plist summary)
              (message "Cancelled.")))
        ;; No ICS — fall through to LLM extraction via gcal-add.
        (gcal-add)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-setup-calendar)
;;; my-setup-calendar.el ends here
