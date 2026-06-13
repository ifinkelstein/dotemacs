;;; my-setup-mail.el -*- lexical-binding: t -*-

;; I use mbsync and mu4e
;;; Mu4e
;; Note: mu4e 1.12.* uses gnus for how it processes messages.
;;* mu4e
(use-package mu4e
  :ensure nil
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
  :commands (mu4e mu4e-compose-new mu4e-update-mail-and-index)
  ;; Change keybindings to look more like elfeed
  ;; in headers view, "r" for read messages, "u" for unread messages
  :bind (:map mu4e-view-mode-map
              ("c" . mu4e-copy-thing-at-point)
              ("f" . link-hint-open-link)
              ;; Quickly switch between plain text and HTML mime type.
              ("K" . my-mu4e-copy-message-to-kill-ring)
              ("r" . mu4e-view-mark-for-read)
              ("u" . mu4e-view-mark-for-unread)
              ("!" . mu4e-view-mark-for-refile)
              ("?" . mu4e-view-mark-for-unmark)
              ("L" . org-store-link)
              ("M-o" . my-transient-email)

              :map mu4e-headers-mode-map
              ("r" . mu4e-headers-mark-for-read)
              ("u" . mu4e-headers-mark-for-unread)
              ("!" . mu4e-headers-mark-for-refile)
              ("?" . mu4e-headers-mark-for-unmark)
              ("K" . my-mu4e-copy-message-to-kill-ring)
              ("L" . org-store-link)
              ("M-o" . my-transient-email)
              :map mu4e-main-mode-map
              ("u" . mu4e-update-index )
              ("M-o" . my-transient-email)
              :map mu4e-compose-minor-mode-map
              ("R" . my-mu4e-compose-reply-ask-wide))

  :config
  ;; Fix a problem with some escape characters
  ;; https://github.com/djcb/mu/issues/2662
  (setq rfc2047-quote-decoded-words-containing-tspecials t)

  ;; Fix encoding errors when viewing emails with mismatched/undeclared charsets
  ;; Prefer UTF-8 for MIME decoding, and provide sensible fallbacks
  (setq mm-coding-system-priorities '(utf-8 utf-8-auto))
  (setq mm-default-coding-system 'utf-8)
  (setq mm-body-charset-encoding-alist '((iso-8859-1  . utf-8)
                                         (iso-8859-15 . utf-8)
                                         (us-ascii     . utf-8)))
  ;; Override declared charsets → utf-8 during MIME decoding.
  ;; Only override charsets that are routinely mislabeled; do NOT
  ;; override iso-8859-1/15 — genuine Latin-1 emails (e.g. "María")
  ;; contain bytes like 0xED that are invalid UTF-8, causing
  ;; "unknown encoding" errors on reply.
  (setq mm-charset-override-alist '((us-ascii . utf-8)
                                    (gb2312   . utf-8)
                                    (gbk      . utf-8)))

  ;; Ensure Emacs decodes Windows-1252 (cp1252) correctly in mu4e.
  ;; Outlook sends email in Windows-1252 whose 0x80–0x9F bytes (smart
  ;; quotes, en-dashes, ellipses) are valid glyphs but fall in the C1
  ;; control range of ISO-8859-1.  Without these two lines Emacs may
  ;; fall back to latin-1 and render them as garbage.
  (prefer-coding-system 'utf-8)
  (set-coding-system-priority 'utf-8 'cp1252)

  ;; Finding the binary (installed w/homebrew)
  (setq mu4e-mu-binary (executable-find "mu"))

  ;; (mu4e-toggle-logging) ;;turn on for error logging buffer

  ;;** Syncing
  ;; Sync imap servers w/mbsync (via isync installed w/homebrew):
  ;; tee so both mu4e and the log file see stdout+stderr;
  ;; pipefail preserves mbsync's exit code through the pipe
  (setq mu4e-get-mail-command
        (concat "bash -o pipefail -c '"
                (executable-find "mbsync")
                " -c ~/.config/.mbsyncrc -aV 2>&1"
                " | tee ~/.config/.mbsync.log'"))
  ;; Skip files whose ctime/mtime haven't changed — much faster with 200K+ messages
  (setq mu4e-index-lazy-check t)
  ;; Change filenames when moving
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  ;; i.e. makes sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)
  ;; Refresh mail using mbsync every 5 minutes
  (setq mu4e-update-interval (* 5 60))

  ;;** Sleep/Wake Recovery for mu4e
  ;;
  ;; PROBLEM: On a laptop, macOS sleep/wake breaks mu4e in several
  ;; sneaky ways that are hard to notice:
  ;;
  ;;   1. STUCK TIMERS — Emacs repeating timers store an absolute
  ;;      fire-time.  When the laptop sleeps for hours, the timer
  ;;      fires into the void during the wake transition.  Its
  ;;      `triggered' flag gets set to t, but the rescheduling logic
  ;;      in `timer-event-handler' fails silently.  Result: the timer
  ;;      sits in `timer-list' with a fire-time in the past and
  ;;      `triggered' = t, so it never fires again.  mbsync stops
  ;;      running and you don't notice for hours.
  ;;
  ;;   2. STALE MBSYNC PROCESS — If mbsync was mid-sync when sleep
  ;;      hit, the process gets killed by the OS.  But Emacs might
  ;;      not run the process sentinel (race condition), so
  ;;      `mu4e--update-buffer' still looks "live" to Emacs.  The
  ;;      guard in `mu4e-update-mail-and-index' sees this and says
  ;;      "Update process is already running", blocking ALL future
  ;;      syncs.  This is the sneakiest failure — everything looks
  ;;      fine, the timer fires, but mbsync never actually runs.
  ;;
  ;;   3. NETWORK NOT READY — Wi-Fi takes a few seconds to reconnect
  ;;      after wake.  If mbsync fires immediately, it fails with a
  ;;      DNS or connection error and the sync is silently skipped.
  ;;
  ;;   4. MU SERVER DEATH — The mu subprocess can occasionally die
  ;;      during sleep.  Without it, mu4e can't index or search.
  ;;
  ;; SOLUTION: Two-layer detection + single recovery orchestrator.
  ;;
  ;;   Detection Layer 1: TIME-GAP HEARTBEAT
  ;;     A cheap timer writes a timestamp every 60 seconds.  Any code
  ;;     can compare `current-time' to this timestamp — if the gap is
  ;;     much larger than 60s, the machine was asleep.  This is fully
  ;;     portable (no macOS APIs) and catches sleep even if you don't
  ;;     touch Emacs immediately after wake.
  ;;
  ;;   Detection Layer 2: FOCUS CHANGE
  ;;     `after-focus-change-function' fires when Emacs regains focus
  ;;     (e.g. you switch to it after waking the laptop).  This gives
  ;;     faster response than waiting for the heartbeat, but only
  ;;     triggers if you actually focus Emacs.
  ;;
  ;;   Recovery: MY-MU4E-SLEEP-WAKE-RECOVER
  ;;     A single orchestrator function that, on wake detection:
  ;;       1. Kills any stale mbsync process / update buffer
  ;;       2. Resets the mu4e update timer
  ;;       3. Waits briefly for network (with timeout)
  ;;       4. Checks mu server health, restarts if needed
  ;;       5. Triggers an immediate mail sync
  ;;     This is idempotent — safe to call multiple times (the
  ;;     debounce guard prevents double-firing from both detection
  ;;     layers within the same wake event).

  ;; ---- Heartbeat: detect time gaps from sleep ----

  (defvar my-mu4e--heartbeat-time (current-time)
    "Timestamp of the last heartbeat tick.
Updated every `my-mu4e--heartbeat-interval' seconds by a timer.
If `current-time' minus this value is much larger than the
interval, the machine was probably asleep.")

  (defvar my-mu4e--heartbeat-interval 60
    "Seconds between heartbeat ticks.
Kept short so we detect sleep quickly.  The timer itself is
trivially cheap (just writes a timestamp).")

  (defvar my-mu4e--sleep-threshold 90
    "Seconds of heartbeat gap that counts as a sleep event.
Should be comfortably larger than `my-mu4e--heartbeat-interval'
to avoid false positives from Emacs being busy with GC or a
long-running command.  90s means: if more than 90 seconds elapsed
since the last heartbeat, we assume the machine slept.")

  (defvar my-mu4e--last-recovery-time nil
    "Timestamp of the last sleep/wake recovery.
Used as a debounce guard: if we recovered less than 30 seconds
ago, skip the recovery.  This prevents double-firing when both
the heartbeat and focus-change detect the same wake event.")

  (defvar my-mu4e--network-wait-seconds 8
    "Seconds to wait for network after a detected wake event.
Wi-Fi typically reconnects within 3-5 seconds on macOS.  We poll
every 2 seconds up to this limit.  If network never comes back
\(e.g. airplane mode), we give up and let the next timer tick
retry naturally.")

  (defun my-mu4e--heartbeat-tick ()
    "Record the current time; detect sleep if gap is too large.
Called every `my-mu4e--heartbeat-interval' seconds by a repeating
timer.  If the gap between now and the last tick exceeds
`my-mu4e--sleep-threshold', call the recovery orchestrator."
    (let ((now (current-time))
          (gap (float-time (time-subtract (current-time)
                                          my-mu4e--heartbeat-time))))
      (setq my-mu4e--heartbeat-time now)
      ;; If the gap is suspiciously large, we probably slept.
      (when (> gap my-mu4e--sleep-threshold)
        (mu4e-message "Heartbeat gap %.0fs (threshold %ds) — recovering from sleep"
                      gap my-mu4e--sleep-threshold)
        (my-mu4e-sleep-wake-recover))))

  ;; Start the heartbeat timer.  `run-at-time' with a repeat arg
  ;; creates a repeating timer.  We deliberately use a short first
  ;; delay (10s) so the heartbeat is established quickly after init.
  ;; Guard: cancel any existing heartbeat timer first so config
  ;; reloads don't accumulate duplicate timers.
  (defvar my-mu4e--heartbeat-timer nil
    "The heartbeat timer object, kept so we can cancel on reload.")
  (when (timerp my-mu4e--heartbeat-timer)
    (cancel-timer my-mu4e--heartbeat-timer))
  (setq my-mu4e--heartbeat-timer
        (run-at-time 10 my-mu4e--heartbeat-interval #'my-mu4e--heartbeat-tick))

  ;; ---- Focus-change: faster detection when user returns ----

  (defun my-mu4e--on-focus-change ()
    "Check for sleep/wake when Emacs gains focus.
Compares current time to `my-mu4e--heartbeat-time'.  If the gap
exceeds `my-mu4e--sleep-threshold', trigger recovery.

This is the fast path: the user wakes the laptop and switches to
Emacs, so we recover before the next heartbeat tick."
    ;; Only act when a frame is focused (not on every focus-out too).
    (when (seq-some #'frame-focus-state (frame-list))
      (let ((gap (float-time (time-subtract (current-time)
                                            my-mu4e--heartbeat-time))))
        (when (> gap my-mu4e--sleep-threshold)
          (my-mu4e-sleep-wake-recover)))))

  (when (boundp 'after-focus-change-function)
    (add-function :after after-focus-change-function
                  #'my-mu4e--on-focus-change))

  ;; ---- Network readiness check ----

  (defun my-mu4e--network-available-p ()
    "Return non-nil if the network appears to be up.
Uses a single ICMP ping to Cloudflare DNS (1.1.1.1) with a 2
second timeout (macOS -W is in milliseconds → 2000).  Returns nil
if the ping fails (no network) or if `ping' is not found."
    (zerop (call-process "ping" nil nil nil "-c1" "-W2000" "1.1.1.1")))

  (defun my-mu4e--wait-for-network ()
    "Block until network is available, up to `my-mu4e--network-wait-seconds'.
Polls every 2 seconds.  Returns non-nil if network came up,
nil if we timed out.

WHY BLOCK?  This runs right after wake, typically from a timer or
focus hook — not during interactive editing.  The brief pause
\(at most 8s) is acceptable to ensure mbsync doesn't fail.  If
blocking bothers you, reduce `my-mu4e--network-wait-seconds'."
    (let ((deadline (+ (float-time) my-mu4e--network-wait-seconds)))
      (catch 'done
        (while (< (float-time) deadline)
          (when (my-mu4e--network-available-p)
            (throw 'done t))
          (sleep-for 2))
        ;; Timed out.
        nil)))

  ;; ---- Stamp update-process start time ----

  (defun my-mu4e--stamp-update-process (&rest _)
    "Record start time on the mu4e update process so we can detect stuck syncs."
    (when-let* ((buf (and (buffer-live-p mu4e--update-buffer) mu4e--update-buffer))
                (proc (get-buffer-process buf)))
      (unless (process-get proc 'my-start-time)
        (process-put proc 'my-start-time (current-time)))))

  (advice-add 'mu4e-update-mail-and-index :after #'my-mu4e--stamp-update-process)

  ;; ---- Stale process cleanup ----

  (defun my-mu4e--kill-stale-update-process ()
    "Kill a stale mbsync process left over from before sleep.

HOW THIS HAPPENS: mbsync was running when the laptop slept.  The
OS killed the process, but Emacs never got the signal (or the
sentinel raced).  `mu4e--update-buffer' still has a process
object that looks alive to `process-live-p' even though the
underlying PID is gone.

The guard in `mu4e-update-mail-and-index' checks:
  (process-live-p (get-buffer-process mu4e--update-buffer))
and if this returns non-nil, it prints \"already running\" and
refuses to start a new sync.

FIX: Check if the buffer exists and its process is dead (or the
process object is stale).  If so, kill the buffer so the guard
no longer blocks.

Returns non-nil if a stale process was cleaned up."
    (when (buffer-live-p mu4e--update-buffer)
      (let* ((proc (get-buffer-process mu4e--update-buffer))
             (stale (or (null proc)                  ; buffer exists but no process
                        (not (process-live-p proc))  ; process object is dead
                        ;; Belt-and-suspenders: if the process has been
                        ;; "running" for > 10 minutes, it's definitely stuck.
                        ;; Normal mbsync finishes in 30-60 seconds.
                        (and (eq (process-status proc) 'run)
                             (let ((start (process-get proc 'my-start-time)))
                               (and start
                                    (> (float-time
                                        (time-subtract (current-time) start))
                                       600)))))))
        (when stale
          (mu4e-message "Cleaning up stale mu4e update process")
          (when (and proc (process-live-p proc))
            (kill-process proc))             ; kill the zombie
          (kill-buffer mu4e--update-buffer)  ; remove the blocking buffer
          t))))

  ;; ---- Timer reset ----

  (defun my-mu4e--reset-update-timer ()
    "Cancel and recreate the mu4e update timer.

WHY: After sleep, the timer's absolute fire-time is in the past,
and its `triggered' flag is stuck at t.  Emacs won't fire it
again.  Cancelling and recreating gives it a fresh fire-time
relative to now."
    (when (and (bound-and-true-p mu4e--update-timer)
               (timerp mu4e--update-timer))
      (cancel-timer mu4e--update-timer)
      (setq mu4e--update-timer nil))
    ;; Recreate only if mu4e is running and has an update interval.
    (when (and (bound-and-true-p mu4e-update-interval)
               (mu4e-running-p))
      (setq mu4e--update-timer
            (run-at-time mu4e-update-interval mu4e-update-interval
                         #'mu4e--refresh-timer))))

  ;; ---- mu server health check ----

  (defun my-mu4e--ensure-server ()
    "Ensure the mu server subprocess is alive.  Restart if dead.

The mu server is a long-running subprocess that handles indexing,
searching, and moving messages.  If it died during sleep, mu4e
is a hollow shell — headers and search silently fail.

Returns non-nil if the server had to be restarted."
    (unless (mu4e-running-p)
      (mu4e-message "mu server is dead — restarting")
      (mu4e 'background)
      t))

  ;; ---- The orchestrator ----

  (defun my-mu4e-sleep-wake-recover ()
    "Single entry point for recovering mu4e after a sleep/wake cycle.

Called by both the heartbeat timer (Layer 1) and the focus-change
hook (Layer 2).  A debounce guard prevents double-firing within
30 seconds — this matters because both layers will often detect
the same wake event.

Recovery steps (in order):
  1. Kill stale mbsync process — unblocks future syncs
  2. Reset the update timer — unsticks the repeating timer
  3. Wait for network — gives Wi-Fi time to reconnect
  4. Ensure mu server — restart the indexer if it died
  5. Trigger immediate sync — fetch mail NOW, don't wait 5 min

Each step is independent and logged.  If one fails, the others
still run.  The whole thing is idempotent."
    ;; Debounce: skip if we recovered very recently.
    (unless (and my-mu4e--last-recovery-time
                 (< (float-time (time-subtract (current-time)
                                               my-mu4e--last-recovery-time))
                    30))
      (setq my-mu4e--last-recovery-time (current-time))

      (mu4e-message "Sleep/wake detected — running recovery…")

      ;; Step 1: Kill stale mbsync process.
      (my-mu4e--kill-stale-update-process)

      ;; Step 2: Reset the update timer so it fires on schedule again.
      (my-mu4e--reset-update-timer)

      ;; Step 3: Wait for network (blocks briefly).
      (if (my-mu4e--wait-for-network)
          (progn
            ;; Step 4: Make sure the mu server is alive.
            (my-mu4e--ensure-server)
            ;; Step 5: Sync immediately (in background).
            (mu4e-message "Network up — triggering immediate sync")
            (run-at-time 2 nil #'mu4e-update-mail-and-index t))
        (mu4e-message "Network not available — skipping immediate sync"))))


  ;;** Add SVG tags
  (with-eval-after-load 'svg-tag-mode
    ;; ref: https://github.com/rougier/nano-emacs/blob/master/nano-mu4e.el
    ;; --- Nicer actions display using SVG tags -----------------------------------
    (plist-put (cdr (assq 'refile   mu4e-marks)) :char "×")
    (plist-put (cdr (assq 'trash    mu4e-marks)) :char "×")
    (plist-put (cdr (assq 'untrash  mu4e-marks)) :char "×")
    (plist-put (cdr (assq 'read   mu4e-marks)) :char "×")
    (plist-put (cdr (assq 'unread   mu4e-marks)) :char "×")
    (plist-put (cdr (assq 'delete   mu4e-marks)) :char "×")
    (plist-put (cdr (assq 'flag     mu4e-marks)) :char "×")
    (plist-put (cdr (assq 'unflag   mu4e-marks)) :char "×")
    (plist-put (cdr (assq 'move     mu4e-marks)) :char "×")
    (plist-put (cdr (assq 'tag      mu4e-marks)) :char "×")
    (setq mu4e-headers-show-target nil)

    (defun mu4e-mark-at-point-advice (mark _target)
      "Show an SVG tag overlay at the end of the marked header line."
      (let ((overlay (make-overlay (- (line-end-position) 12)
                                   (line-end-position)))
            (tag (alist-get mark '((refile  . "ARCHIVE")
                                   (trash   . "TRASH")
                                   (untrash . "UNTRASH")
                                   (delete  . "DELETE")
                                   (read    . "READ")
                                   (unread  . "UNREAD")
                                   (flag    . "FLAG")
                                   (unflag  . "UNFLAG")
                                   (move    . "MOVE")
                                   (tag     . "TAG")))))
        (save-excursion
          (if (eql mark 'unmark)
              (delete-overlay overlay)
            (when tag
              (overlay-put overlay 'display (svg-tag-make tag nil 3 0)))))))

    (advice-add 'mu4e-mark-at-point :after #'mu4e-mark-at-point-advice)) ;;with-eval-after-load

  ;;** Attachments
  ;; Set default attachment dir (create if missing).  mu4e's built-in
  ;; `mu4e-view-save-attachments' (bound to `e' in view mode) saves all
  ;; attachments when none are selected.
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads/_Mail"))
  (make-directory mu4e-attachment-dir t)

  ;;** searching
  (defun my-mu4e-search-for-sender (&optional msg)
    "Find mails sent from the sender of MSG."
    (mu4e-search (concat "from:"
                         (plist-get (car (mu4e-message-field msg :from)) :email))))

  (defun my-mu4e-capture-mail-todo (msg)
    "Capture email MSG as a TODO with an org-link to the message."
    (interactive)
    (org-store-link nil t)
    (org-capture nil "M"))

  ;;* mu4e actions (headers & view)
  ;; Shortcut key is the first character of the action name.
  ;;   a → add contact          g → gcal add
  ;;   o → org link             r → retag message
  ;;   R → Remove all tags      t → todo from email
  ;;   v → view in browser      y → yank path
  ;;   z → search for sender
  (defun my-mu4e-action-gcal-add (msg)
    "Add a calendar event from MSG via `gcal-add'.
`gcal-add' is context-aware: it pulls the message at point in mu4e
buffers, so MSG is only used to ensure a message is present."
    (require 'gcal-add)
    (gcal-add))
  (dolist (action '(("add contact"       . mu4e-action-add-org-contact)
                    ("gcal add"           . my-mu4e-action-gcal-add)
                    ("org link"           . org-store-link)
                    ("retag message"      . mu4e-action-retag-message)
                    ("Remove all tags"    . my-mu4e-remove-all-tags)
                    ("todo from email"   . my-mu4e-capture-mail-todo)
                    ("view in browser"   . mu4e-action-view-in-browser)
                    ("yank path"         . my-mu4e-copy-message-path)
                    ("zsearch for sender" . my-mu4e-search-for-sender)))
    (setf (alist-get (car action) mu4e-headers-actions nil nil #'equal) (cdr action))
    (setf (alist-get (car action) mu4e-view-actions nil nil #'equal) (cdr action)))
  ;; Remove xwidget duplicate — "view in browser" covers it
  (setq mu4e-view-actions
        (cl-remove-if (lambda (a) (equal (car a) "xview in xwidget"))
                       mu4e-view-actions))

  ;;* Viewing
  (defun my-mu4e-get-mailbox (msg)
    "Return \"account|maildir\" for MSG, for the :mailbox-short header."
    (let ((parts (split-string (mu4e-message-field msg :maildir) "/" t)))
      (format "%s|%s" (car parts) (car (last parts)))))

  ;;** Headers
  (add-to-list 'mu4e-header-info-custom
               '(:mailbox-short . (:name "Mailbox"
                                         :shortname ""
                                         :function my-mu4e-get-mailbox)))

  (setq mu4e-headers-date-format "%D";; "%Y-%m-%d %H:%M:%S"
        mu4e-headers-fields '(
                              (:flags          .  10)
                              (:human-date     .  10)
                              (:mailbox-short  .  15)
                              (:from-or-to     .  15)
                              (:tags           .  10)
                              (:subject        . nil)
                              ))

  ;; how to handle html-formatted emails

  ;; other display settings
  (setq mu4e-speedbar-support t)
  (setq mu4e-use-fancy-chars t)

  ;; enable seeing related messages in a thread.
  ;; useful for searches: https://groups.google.com/g/mu-discuss/c/yWvilXfLunE
  (setq mu4e-search-include-related nil)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

  ;;** Useful functions and actions
  ;; this function will remove all tags from a msg by erasing the mu4e-actions-tags-header-line
  ;; based on mu4e-action-retag-message
  (defun my-mu4e-remove-all-tags (msg)
    "Remove all tags from MSG."
    (let ((path   (mu4e-message-field msg :path))
          (header mu4e-action-tags-header))
      (if (not (mu4e--contains-line-matching (concat header ":.*") path))
          ;; Add tags header just before the content
          (mu4e--replace-first-line-matching
           "^$" (concat header ": \n") path)
        ;; replaces keywords, restricted to the header
        (mu4e--replace-first-line-matching
         (concat header ":.*")
         (concat header ": ")
         path))
      (mu4e-message (concat "All tags removed."))
      (mu4e--refresh-message path)))

  ;; tag or untag one or many emails in header view. Can use '*' to act on multiple.
  ;; Use setf/alist-get (not add-to-list) so config reloads don't stack duplicate
  ;; entries.  The two marks must use distinct keys (tag/untag) -- assq lookup in
  ;; mark execution returns the first match, so a shared key shadows the other.
  (setf (alist-get 'tag mu4e-marks)
        '(:char       "g"
          :prompt     "gtag"
          :ask-target nil
          :action     (lambda (docid msg target)
                        (mu4e-action-retag-message msg target))))

  (setf (alist-get 'untag mu4e-marks)
        '(:char       "G"
          :prompt     "Guntag"
          :action     (lambda (docid msg target)
                        (my-mu4e-remove-all-tags msg))))

  (defun my-mu4e-copy-message-path (msg)
    "Copy the file path of MSG to the kill ring."
    (let ((path (mu4e-message-field msg :path)))
      (kill-new path)
      (message "Copied: %s" path)))

  ;; create org link to message
  (require 'mu4e-org)

  ;; org-contacts file for "add contact" action
  ;; NOTE: org-contacts can aggressively override mu4e's native contact
  ;; completion. If that happens, set `org-contacts-enable-completion' to
  ;; nil in the org-contacts use-package (in my-setup-notes.el).
  (setq mu4e-org-contacts-file my-org-contacts-file)

;;;; Composing Email
  ;; Give compose the full frame.  We use `mu4e-compose-switch' = nil
  ;; (current window), so compose already replaces whatever buffer was
  ;; in that window.  If mu4e split to show headers + view before
  ;; composing, delete the *other* window so compose is alone.
  ;;
  ;; IMPORTANT: mu4e saves the window configuration BEFORE
  ;; `mu4e-compose-mode-hook' runs, so modifying windows here is safe —
  ;; the restore after send/kill will recreate the original layout.
  (add-hook 'mu4e-compose-mode-hook
            (defun my-mu4e-compose-settings ()
              "My initial settings for message composition."
              (my-mu4e-add-cc-bcc)
              (my-swap-email-from-field)
              (delete-other-windows)
              ;; Check spelling
              (jinx-mode 1)
              (olivetti-mode 1)
              (olivetti-set-width 0.8)
              (auto-fill-mode -1)
              (hl-line-mode -1)))

  ;; Use mu4e system-wide
  (setq mail-user-agent 'mu4e-user-agent)

  (setopt read-mail-command 'mu4e)

  ;; Compose in new buffer (can make 'window for new window)
  (setq mu4e-compose-switch nil)

  ;; Inline forwarding of multipart/alternative messages (e.g. from
  ;; Outlook) leaves the body empty between the forward markers —
  ;; the content lands in a sibling MIME part that Outlook ignores.
  ;; Forwarding as message/rfc822 attachment avoids this.
  (setq message-forward-as-mime t)

  ;; Don't keep message compose buffers around after sending:
  (setq message-kill-buffer-on-exit t)
  ;;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; where reply should be positioned
  (setopt message-cite-reply-position 'above)

  ;; strip off signatures when replying to message (not sure what this does)
  (setopt message-cite-function #'message-cite-original-without-signature)

  ;; TODO: remove mime part buttons. Appears impossible for now
  ;; https://github.com/djcb/mu/issues/2639

  ;; Possible fix for outlook client reading problems in inline messages
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#How-can-I-avoid-Outlook-display-issues_003f
  (setq  message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote...")

  ;; Filtering function to remove annoying composition auto-completes
  ;; https://emacs.stackexchange.com/questions/47789/how-to-remove-email-address-from-local-database-in-mu4e
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Contact-functions.html
  (defun my-mu4e-contact-filter (addr)
    "Remove annoying completions from the email autocomplete.
Checks `my-mu4e-suppressed-addresses' (private.el) and generic patterns."
    (cond
     ;; Suppress specific addresses from private.el
     ((seq-some (lambda (a) (string-match-p (regexp-quote a) addr))
                my-mu4e-suppressed-addresses)
      nil)
     ;; Generic patterns
     ((string-match-p "no-reply" addr) nil)
     ((string-match-p "noreply" addr) nil)
     ((string-match-p "@finkelsteinlab" addr) nil)
     ((string-match-p "@fortelabs" addr) nil)
     ((string-match-p "via Canvas" addr) nil)
     (t addr)))

  (setq mu4e-contact-process-function 'my-mu4e-contact-filter)

  ;;tweak the composition window to include CC and BCC
  (defun my-mu4e-add-cc-bcc ()
    "Add CC and BCC to all new e-mails"
    (interactive)
    "Add a Cc: & Bcc: header."
    (save-excursion (message-add-header "Cc: \nBcc: \n")))

  (defun my-swap-email-from-field ()
    "Swap the From header to the primary email address.
Uses `my-email-primary-name' and `my-email-primary-address' from private.el."
    (interactive)
    (save-excursion
      (message-remove-header "From" nil 'FIRST)
      (goto-char (point-min))
      (insert "From: " (message-make-from my-email-primary-name
                                           my-email-primary-address) "\n")))

;;;; Sending Mail
  ;; Configure the function to use for sending mail
  ;; use msmtp to manage sending mail from different e-mail accounts
  ;; reference: https://tushartyagi.com/blog/configure-mu4e-and-msmtp
  (setq sendmail-program (executable-find "msmtp")
        message-sendmail-f-is-evil t
        ;; NB: use backquote + expand-file-name because Emacs calls
        ;; msmtp via call-process-region (no shell), so ~ is NOT expanded.
        message-sendmail-extra-arguments `("--read-envelope-from"
                                           "-C" ,(expand-file-name ".msmtprc" "~/.config/"))
        message-send-mail-function 'message-send-mail-with-sendmail)


  ;;** Contexts
  ;; Account data lives in `my-mu4e-context-accounts' (private.el)
  (defun my--build-mu4e-context (account)
    "Build a `mu4e-context' from ACCOUNT plist."
    (let ((name   (plist-get account :name))
          (prefix (plist-get account :maildir-prefix)))
      (make-mu4e-context
       :name name
       :match-func
       (lambda (msg)
         (when msg
           (string-prefix-p prefix (mu4e-message-field msg :maildir))))
       :vars `((user-mail-address   . ,(plist-get account :email))
               (user-full-name      . ,(plist-get account :full-name))
               (mu4e-drafts-folder  . ,(plist-get account :drafts))
               (mu4e-sent-folder    . ,(plist-get account :sent))
               (mu4e-refile-folder  . ,(plist-get account :refile))
               (mu4e-trash-folder   . ,(plist-get account :trash))
               (smtpmail-smtp-user  . ,(plist-get account :smtp-user))))))

  ;; Non-PII shared/lab accounts kept in this tracked file; personal
  ;; accounts live in `my-mu4e-context-accounts' (private.el).  The
  ;; hisserlab app password is in the macOS Keychain, not here.
  (defvar my-mu4e-extra-context-accounts
    '((:name "Hisser"
       :maildir-prefix "/Hisser"
       :email "hisserlab@gmail.com"
       :full-name "Hisser Lab"
       :smtp-user "hisserlab@gmail.com"
       :drafts "/Hisser/[Gmail]/Drafts"
       :sent "/Hisser/[Gmail]/Sent Mail"
       :refile "/Hisser/[Gmail]/All Mail"
       :trash "/Hisser/[Gmail]/Trash"))
    "Shared account definitions (no PII) merged into `mu4e-contexts'.")

  (setq mu4e-contexts
        (mapcar #'my--build-mu4e-context
                (append my-mu4e-context-accounts
                        my-mu4e-extra-context-accounts)))

  ;; Ask for context if none is set
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'pick-first)

  ;;** Quick Actions & helper functions
  ;; Helpful discussion at
  ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-05.org

  ;;disable threading in search results
  (setq mu4e-search-threads nil)

  ;; Note that :hide-unread is implied when the query is not a string; this for the common case where the query function involves some user input, which would be disruptive in this case.
  ;; Personal bookmarks from `my-mu4e-personal-bookmarks' (private.el)
  (setq mu4e-bookmarks
        (append
         ;; Generic bookmarks
         `((:name "Unread"
                  :query ,(mu4e-make-query
                           '(and (flag unread) (not (flag trashed)) (maildir (regex "Inbox$"))))
                  :key ?u)
           (:name "Today"
                  :query ,(mu4e-make-query
                           '(and (date (today .. now)) (not (flag trashed)) (maildir (regex "Inbox$"))))
                  :key ?t)
           (:name "New This Week"
                  :query ,(mu4e-make-query
                           '(and (date (1w .. now)) (flag unread) (not (flag trashed)) (maildir (regex "Inbox$"))))
                  :key ?w)
           (:name "New This Month"
                  :query ,(mu4e-make-query
                           '(and (date (1m .. now)) (flag unread) (not (flag trashed)) (maildir (regex "Inbox$"))))
                  :key ?m)
           (:name "New Last Month"
                  :query ,(mu4e-make-query
                           '(and (date (2m .. 1m)) (flag unread) (not (flag trashed)) (maildir (regex "Inbox$"))))
                  :key ?M)
           (:name "Unread + Untagged"
                  :query "flag:unread AND NOT tag:/.+/ AND maildir:/Inbox$/"
                  :key ?U))
         ;; Personal bookmarks (PII in private.el)
         (mapcar (lambda (bm)
                   (list :name (plist-get bm :name)
                         :query (mu4e-make-query (plist-get bm :query-sexp))
                         :key (plist-get bm :key)))
                 my-mu4e-personal-bookmarks)))


;;;;; Maildirs
  ;; maildirs used frequently; access them with 'j' ('jump')
  (setq mu4e-maildir-shortcuts
	    '(("/UT/Inbox"               . ?u)
	      ("/Lab/Inbox"              . ?l)
          ("/Hisser/Inbox"           . ?h)
          ("/Lab/Inbox/@SaneJobs"   . ?j)
	      ("/Lab/Inbox/@SaneLater"   . ?s)
          ("/Lab/Inbox/@SaneReceipts"   . ?r)
          ("/Lab/Inbox/@SaneNoReplies"   . ?x)
          ("/Lab/Inbox/@SaneNews"   . ?n)
          ("/Lab/Inbox/@SaneBlackHole"   . ?b)))
  ;; Show maildirs with 0 messages
  (setq mu4e-main-hide-fully-read nil)

;;;; Better Marking (w/Icons & SVG Tags)
;;;;; Unicode icons for marking
  ;; --- Nicer actions display using unicode tags -----------------------------------
  (setq mu4e-headers-unread-mark    '("u" . "📩 "))
  (setq mu4e-headers-draft-mark     '("D" . "🚧 "))
  (setq mu4e-headers-flagged-mark   '("F" . "🚩 "))
  (setq mu4e-headers-new-mark       '("N" . "✨ "))
  (setq mu4e-headers-passed-mark    '("P" . "↪ "))
  (setq mu4e-headers-replied-mark   '("R" . "↩ "))
  (setq mu4e-headers-seen-mark      '("S" . " "))
  (setq mu4e-headers-trashed-mark   '("T" . "🗑️"))
  (setq mu4e-headers-attach-mark    '("a" . "📎 "))
  (setq mu4e-headers-encrypted-mark '("x" . "🔑 "))
  (setq mu4e-headers-signed-mark    '("s" . "🖊 "))

  ;;;; Miscellaneous helper functions
  ;; remap how replies are composed
  (defun my-mu4e-compose-reply-ask-wide ()
    "Ask whether to reply-to-all (wide reply) when replying with 'R'.
Tested with mu4e 1.12.2"
    (interactive)
    (if (mu4e-message-contact-field-matches-me (mu4e-message-at-point) :from)
        (mu4e-compose-reply)
      (let ((tos (length (mu4e-message-field-at-point :to)))
            (ccs (length (mu4e-message-field-at-point :cc))))
        (mu4e-compose-reply
         (and (> (+ tos ccs) 1)
              (yes-or-no-p "Reply to all?"))))))

  (defun my-mu4e--extract-body (path)
    "Extract plain-text body from the email at PATH via `mu view'."
    (with-temp-buffer
      (when (zerop (call-process mu4e-mu-binary nil t nil "view" path))
        (string-trim (buffer-string)))))

  (defun my-mu4e-copy-message-to-kill-ring ()
    "Copy the current mu4e message (headers + body) to the kill ring.
Works in both `mu4e-headers-mode' and `mu4e-view-mode'.
Formats the message as structured text suitable for pasting into
Claude Code or other LLM contexts."
    (interactive)
    (let* ((msg (or (mu4e-message-at-point)
                    (user-error "No message at point")))
           (from (mapconcat
                  (lambda (c)
                    (let ((name (plist-get c :name))
                          (email (plist-get c :email)))
                      (if name (format "%s <%s>" name email) email)))
                  (mu4e-message-field msg :from) ", "))
           (to (mapconcat
                (lambda (c)
                  (let ((name (plist-get c :name))
                        (email (plist-get c :email)))
                    (if name (format "%s <%s>" name email) email)))
                (mu4e-message-field msg :to) ", "))
           (subject (or (mu4e-message-field msg :subject) "(no subject)"))
           (date (format-time-string "%Y-%m-%d %a %H:%M"
                                     (mu4e-message-field msg :date)))
           (msgid (mu4e-message-field msg :message-id))
           (body (when-let* ((path (mu4e-message-field msg :path))
                             ((file-exists-p path)))
                   (condition-case err
                       (my-mu4e--extract-body path)
                     (error
                      (message "Body extraction failed: %s" err)
                      nil))))
           (text (concat "From: " from "\n"
                         "To: " to "\n"
                         "Subject: " subject "\n"
                         "Date: " date "\n"
                         (when msgid (concat "Message-ID: " msgid "\n"))
                         "\n"
                         (or body "(body not available)"))))
      (kill-new text)
      (message "Copied: %s" (truncate-string-to-width subject 60 nil nil t))))

  (defun my-email-to-kill-ring ()
    "Prompt user to search for an email address. Save selected ones to the kill ring.

If there's no match, return the input. Since I'm not using completing-read-multiple, can only
select one email at a time.
"
    (interactive)
    (let* ((contact (completing-read "Email address: " mu4e--contacts-set))
           (email (string-trim contact ".*<" ">*")))
      (message "Copied %s to the clipboard" email)
      (kill-new email)))

  ;; Store link to message if in header view, not to header query
  (setq mu4e-org-link-query-in-headers-mode nil)

  ;; Go to unread. Only show unread mail from main inbox.
  (defvar my-mu4e-unread-query "flag:unread AND NOT flag:trashed AND (maildir:/UT/Inbox OR maildir:/Lab/Inbox)")
  ) ;; use-package mu4e
;;** End Mu4e

;; Use Dired to attach files via C-c <RET> C-a (gnus-dired feature).
;; Top-level add-hook so first dired doesn't pull in the whole mail stack.
;; https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html
(add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

;;* mu4e-goodies (AI chat + spam check)
(use-package mu4e-goodies
  :load-path "~/projects/elisp/mu4e-goodies"
  :after mu4e
  :config
  (mu4e-goodies-setup))

;;* mu4e-query for building searches
(use-package mu4e-query
  :vc (:url "https://github.com/mickeynp/mu4e-query")
  :commands (mu4e-make-query))
;;* Using Org & HTML (Org-Msg)
(use-package org-msg
  :after (mu4e)
  :ensure t
  :vc (:url "https://github.com/jeremy-compostella/org-msg" :branch "main")
  ;; avoid pesky org ASCII Export buffer
  ;; https://github.com/jeremy-compostella/org-msg/issues/169
  :preface
  (defun org-msg-no-temp-buffer (orig-fun &rest args)
    "Advice to set `org-export-show-temporary-export-buffer' to `nil'."
    (let ((org-export-show-temporary-export-buffer nil))
      (apply orig-fun args)) )
  (defun my-org-msg-maybe-enable ()
    "Enable org-msg-mode except when forwarding."
    (unless (eq mu4e-compose-type 'forward)
      (org-msg-mode)))
  :bind (:map org-msg-edit-mode-map
              ("s-<return>" . org-msg-goto-body)
              ("M-o" . my-transient-email-compose))
  :hook ((mu4e-compose-pre . my-org-msg-maybe-enable))
  :config
  ;; org-msg-edit-mode-map shares org-mode-map's ESC prefix keymap by
  ;; reference — give it its own so :bind M-o doesn't collide.  This must
  ;; run in :config (not :init): org-msg-edit-mode-map is void until the
  ;; package loads, so an :init with-eval-after-load 'org errors at startup.
  (let ((esc-map (make-sparse-keymap)))
    (set-keymap-parent esc-map (lookup-key org-mode-map [?\e]))
    (define-key org-msg-edit-mode-map [?\e] esc-map))

  ;; rapid navigation between composition fields
  (transient-define-prefix my-transient-email-compose ()
    [ ;; ("a" "attach" mml-attach-file)
     [("b" "bcc" message-goto-bcc)
      ("c" "cc" message-goto-cc)
      ("f" "from" message-goto-from)
      ]
     [
      ("m" "msg body" org-msg-goto-body)
      ("s" "subject" message-goto-subject)
      ("t" "to" message-goto-to)
      ]
     [("ec" "🤷‍♂️" (lambda () (interactive) (insert "🤷‍♂️")))
      ("ed" "👋" (lambda () (interactive) (insert "👋")))
      ("ef" "🎉" (lambda () (interactive) (insert "🎉")))
      ("eg" "😂" (lambda () (interactive) (insert "😂")))
      ("eh" "❤️" (lambda () (interactive) (insert "❤️")))
      ("ei" "🙏" (lambda () (interactive) (insert "🙏")))
      ("ej" "😐" (lambda () (interactive) (insert "😐")))]
     [("er" "🌈" (lambda () (interactive) (insert "🌈")))
      ("es" "🙂" (lambda () (interactive) (insert "🙂")))
      ("et" "👍" (lambda () (interactive) (insert "👍")))
      ("eu" "🦄" (lambda () (interactive) (insert "🦄")))
      ("ex" "🤞" (lambda () (interactive) (insert "🤞")))
      ("ew" "🔥" (lambda () (interactive) (insert "🔥")))]
     [("eR" "recent" emoji-recent)
      ("eS" "search" emoji-search)]
     [("l" "llm expand" my-mail-llm-expand)]
     ])
  ;; Inject quick-insert contacts from private.el (chained after "l" suffix)
  (when (bound-and-true-p my-email-quick-contacts)
    (let ((prev-key "l"))
      (dolist (entry my-email-quick-contacts)
        (let ((key   (nth 0 entry))
              (label (nth 1 entry))
              (email (nth 2 entry)))
          (transient-append-suffix 'my-transient-email-compose prev-key
            `(,key ,label (lambda () (interactive) (insert ,email))))
          (setq prev-key key)))))

  ;;make org-msg work with mu4e 1.12
  (defun my--ensure-text-not-read-only (orig &rest args)
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point-max) '(read-only nil))
      (apply orig args)))

  ;; fix to get working with mu 1.12.xx
  (advice-add 'message-send :around #'my--ensure-text-not-read-only)

  ;; avoid pesky org ASCII Export buffer
  ;; https://github.com/jeremy-compostella/org-msg/issues/169
  (advice-add 'org-msg-preview :around #'org-msg-no-temp-buffer)
  (advice-add 'org-msg-ctrl-c-ctrl-c :around #'org-msg-no-temp-buffer)
  (add-hook 'message-sent-hook
            (lambda ()
              (interactive)
              ;; Do NOT kill *mu4e-article* here — it must survive until
              ;; `mu4e-compose-post-restore-window-configuration' runs
              ;; (via mu4e-compose-post-hook).  Killing it before the
              ;; restore causes `set-window-configuration' to substitute
              ;; the headers buffer into the dead article window slot,
              ;; producing duplicate headers windows.
              (when-let* ((buf (get-buffer "*Org ASCII Export*")))
                (kill-buffer buf))))

  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	    org-msg-startup "hidestars indent inlineimages"
	    org-msg-greeting-fmt nil
	    org-msg-recipient-names nil
	    org-msg-greeting-name-limit 3
	    org-msg-default-alternatives '((new		        . (text html))
				                       (reply-to-html	. (text html))
				                       (reply-to-text	. (text html)))
        ;; nil to avoid "Args out of range" errors when replying to
        ;; emails with unusual HTML citation formatting
        org-msg-convert-citation nil)
  ;; org-msg checks for attachments via a regex search for keywords.
  ;; `regexp-unmatchable' never matches, disabling that nag.
  (setq org-msg-attached-file-reference regexp-unmatchable)

  (defun cpm/org-msg-hooks ()
    "Hooks for org-msg"
    (auto-fill-mode -1)
    (hl-line-mode -1)  ;; disable highlight line when composing emails
    (diff-hl-mode -1) ;; disable diff gutter
    ;; Shrink org-meta-line (#+begin_src, #+PROPERTIES, etc.) in compose buffers only
    (face-remap-add-relative 'org-meta-line :height 0.5 :foreground "gray60"))
  (add-hook 'org-msg-edit-mode-hook #'cpm/org-msg-hooks)

  ;; After compose setup, `mu4e--jump-to-a-reasonable-place' calls
  ;; `message-goto-body' which lands on the #+OPTIONS line (inside the
  ;; org-msg metadata).  Adjust it to land after :END: instead.
  ;; Unlike the old `message-goto-body' advice this only fires during
  ;; compose setup so it cannot interfere with `org-msg-prepare-to-send'.
  (defun my--org-msg-fix-jump (orig &rest args)
    "In org-msg buffers, ensure cursor lands after :END:, not in metadata."
    (apply orig args)
    (when (derived-mode-p 'org-msg-edit-mode)
      (let ((end-pos (save-excursion
                       (goto-char (point-min))
                       (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                         (line-beginning-position 2)))))
        (when (and end-pos (< (point) end-pos)
                   (message-field-value "To")
                   (message-field-value "Subject"))
          (goto-char end-pos)))))
  (advice-add 'mu4e--jump-to-a-reasonable-place :around #'my--org-msg-fix-jump)



  (defun my-mu4e-add-attachment-icons ()
    "Display file-type icons next to the PROPERTIES drawer for each attachment.
Uses `nerd-icons-icon-for-file' to pick an appropriate icon per filename."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (when-let* ((attachments (org-msg-get-prop "attachment"))
                  ((search-forward ":PROPERTIES:" nil t))
                  (pos (match-beginning 0)))
        (dolist (file attachments)
          (let ((ov (make-overlay pos pos))) ; zero-width overlay at :PROPERTIES:
            (overlay-put ov 'attachment-icon t)
            (overlay-put ov 'after-string
                         (concat " " (nerd-icons-icon-for-file file))))))))

  (defun my-mu4e-remove-attachment-icons ()
    "Remove all attachment icon overlays from the current buffer."
    (save-restriction
      (widen)
      (remove-overlays (point-min) (point-max) 'attachment-icon t)))

  (defun my-mu4e-reset-attachment-icons (orig-fun &rest args)
    "Around advice to refresh attachment icons after ORIG-FUN runs.
Pass ARGS through to ORIG-FUN, then redraw icons."
    (apply orig-fun args)
    (my-mu4e-remove-attachment-icons)
    (my-mu4e-add-attachment-icons))

  (advice-add 'org-msg-attach-attach :around #'my-mu4e-reset-attachment-icons)
  (advice-add 'org-msg-dired-attach :around #'my-mu4e-reset-attachment-icons)
  (advice-add 'org-msg-attach-delete :around #'my-mu4e-reset-attachment-icons)
  ) ;; org-msg

;;* Org Heading to Email/Slack (org-heading-send)
;; Convert org headings to mu4e/org-msg emails or Slack messages.
;; Single merged package; mail deps (org-msg/mu4e) are lazy-required
;; inside the mail backend, so loading this never pulls the mail stack.
(use-package org-heading-send
  :load-path "~/projects/elisp/org-heading-send"
  :commands (org-heading-mail-send
             org-heading-mail-check-scheduled
             org-heading-slack-send)
  :custom
  (org-heading-mail-scheduled-file "~/Work/org/drafts.org"))

(defun my-org-heading-send-dwim ()
  "Send org heading as email or Slack message.
Dispatches based on heading fields: Channel -> Slack, To/In-Reply-To -> email."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((end (org-entry-end-position))
           (text (buffer-substring-no-properties (point) end))
           (slack-p (or (string-match-p "^\\*+ Channel$" text)
                        (string-match-p "^Channel:" text)))
           (email-p (or (string-match-p "^\\*+ To$" text)
                        (string-match-p "^To:" text)
                        (string-match-p "^\\*+ Reply-To$" text)
                        (string-match-p "^In-Reply-To:" text))))
      (cond
       ((and slack-p email-p)
        (user-error "Heading has both Channel and To/In-Reply-To fields"))
       (slack-p (org-heading-slack-send))
       (email-p (org-heading-mail-send))
       (t (user-error "Heading has no Channel (Slack) or To/In-Reply-To (email) field"))))))

;; TODO: test my-org-msg-llm-draft
(defun my-mail-llm-expand ()
  "Find @tn directive in org-msg buffer and use LLM to draft email.

Place cursor anywhere in the buffer. The function searches for a line
starting with @tn followed by instructions. It sends those instructions
along with the entire email as context to the LLM, then replaces the
@tn line with the generated response.

Example usage in an org-msg reply buffer:
  @tn politely decline, too busy. commend her son."
  (interactive)
  (unless (derived-mode-p 'org-msg-edit-mode)
    (user-error "Not in an org-msg-edit-mode buffer"))
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^@tn\\s-+\\(.+\\)$" nil t)
      (user-error "No @tn directive found"))
    (let* ((instructions (match-string 1))
           (llm-line-beg (match-beginning 0))
           (llm-line-end (match-end 0))
           (email-content (buffer-substring-no-properties (point-min) (point-max))))
      (message "[org-msg-llm] Instructions: %s" instructions)
      (message "[org-msg-llm] Sending to LLM...")
      (let ((target-buffer (current-buffer))
            (insert-pos llm-line-beg))
        (gptel-request
            (concat
             "You are drafting an email response. Follow these instructions:\n"
             instructions
             "\n\nSign off with:\nKind regards,\n" my-email-primary-name
             "\n\nHere is the email context (headers and any quoted reply):\n"
             email-content)
          :system "You are a helpful executive assistant drafting emails. Use short, concise, professional, positive tone. Use simple sentences only. Avoid hyphenated or compound sentences. Output only the email body text, no headers or metadata. Use org-mode formatting if needed."
          :callback
          (lambda (response info)
            (if (not response)
                (message "[org-msg-llm] Failed: %s" (plist-get info :status))
              (with-current-buffer target-buffer
                (save-excursion
                  ;; Delete the @tn line only on success
                  (goto-char llm-line-beg)
                  (delete-region llm-line-beg llm-line-end)
                  ;; Insert response at same position
                  (insert response)))
              (message "[org-msg-llm] Draft inserted:\n%s" response))))))))

(defun my-mail-check-tn-directive ()
  "Check if @tn directive is still present; ask to confirm sending.
This prevents accidentally sending an email with an unexpanded @tn
directive that was meant to be processed by the LLM."
  (when (derived-mode-p 'org-msg-edit-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^@tn\\s-+" nil t)
        (unless (y-or-n-p "@tn directive found (not processed by LLM). Send anyway? ")
          (user-error "Aborted: @tn directive still present"))))))

(add-hook 'message-send-hook #'my-mail-check-tn-directive)

(defun my-mail-sanitize-to-utf8 ()
  "Recode stray raw bytes in the message body to UTF-8 before sending.
Interprets eight-bit bytes as Latin-1, which covers the common case
of quoted Latin-1 text (e.g. \"María\") surviving into the compose
buffer as raw bytes."
  (save-excursion
    (message-goto-body)
    (while (re-search-forward "[[:nonascii:]]" nil t)
      (let ((ch (char-before)))
        (when (eq (char-charset ch) 'eight-bit)
          (let ((byte (multibyte-char-to-unibyte ch)))
            (delete-char -1)
            (insert (decode-coding-string (unibyte-string byte) 'latin-1))))))))

(add-hook 'message-send-hook #'my-mail-sanitize-to-utf8)

;;* Colors for header view
(use-package mu4e-column-faces
  :after mu4e
  :custom-face
  (mu4e-column-faces-date ((t (:foreground unspecified :background unspecified :inherit lambda-green))))
  (gnus-header-name ((t (:foreground unspecified :background unspecified :inherit lambda-green))))
  (mu4e-column-faces-to-from ((t (:foreground unspecified :background unspecified :inherit lambda-focus))))
  :config
  (mu4e-column-faces-mode))

(use-package mail-triage
  :ensure nil
  :load-path "~/projects/elisp/mail-triage"
  ;; :after chaining never fired in the daemon, so the package (and its
  ;; mu4e action registration) never loaded.  Defer on the autoloaded
  ;; entry points instead, and drive setup/config from a mu4e after-load
  ;; hook so `mail-triage-setup' registers its mu4e actions once mu4e is up.
  ;; `M-x mail-triage' remains reachable via the autoload.
  :commands (mail-triage mail-triage-setup)
  :bind (:map mail-triage-mode-map
         ("M-o" . mail-triage-menu))
  :init
  (with-eval-after-load 'mu4e
    (require 'mail-triage)
    (setq mail-triage-llm-backend "Gemini"
          mail-triage-llm-model 'gemini-3-flash-preview
          mail-triage-user-description my-email-user-description)
    (mail-triage-setup)
    (add-hook 'mail-triage-mode-hook
              (lambda ()
                (face-remap-set-base 'magit-section-heading
                                     :inherit 'default
                                     :foreground "#727d97")))))

;;* gcal-add (calendar events from mu4e messages, context-aware)
(use-package gcal-add
  :load-path "~/projects/elisp/gcal-add"
  :commands (gcal-add))

;;* provide my-setup-mail
(provide 'my-setup-mail)
;;* End Setup Email
