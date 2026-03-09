;;; my-setup-mail.el -*- lexical-binding: t -*-

;; I use mbsync and mu4e
;;; Mu4e
;; Note: mu4e 1.12.* uses gnus for how it processes messages.
;;* mu4e
(use-package mu4e
  :ensure nil
  :after (org org-contacts)
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
  :commands (mu4e mu4e-compose-new mu4e-update-mail-and-index)
  ;; Change keybindings to look more like elfeed
  ;; in headers view, "r" for read messages, "u" for unread messages
  :bind (:map mu4e-view-mode-map
              ("c" . mu4e-copy-thing-at-point)
              ("f" . link-hint-open-link)
              ;; Quickly switch between plain text and HTML mime type.
              ("K" . (lambda ()
                       (interactive)
                       (gnus-article-jump-to-part 1)
                       (gnus-article-press-button)
                       (gnus-article-press-button)))
              ("r" . mu4e-view-mark-for-read)
              ("u" . mu4e-view-mark-for-unread)
              ("!" . mu4e-view-mark-for-refile)
              ("?" . mu4e-view-mark-for-unmark)
              ("M-o" . my-transient-email) 
              
              :map mu4e-headers-mode-map
              ("r" . mu4e-headers-mark-for-read)
              ("u" . mu4e-headers-mark-for-unread)
              ("!" . mu4e-headers-mark-for-refile)
              ("?" . mu4e-headers-mark-for-unmark)
              ("M-o" . my-transient-email) 
              :map mu4e-main-mode-map
              ("u" . mu4e-update-index )
              ("M-o" . my-transient-email))

  ;; use Dired to attach files via C-c <RET> C-a
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html
  :hook ((dired-mode . turn-on-gnus-dired-mode))
  
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
  ;; Override declared charsets → utf-8 during MIME decoding (fixes the
  ;; "select encoding" prompt when forwarding messages with
  ;; mismatched/undeclared charsets).
  ;;
  ;; NOTE: Do NOT override windows-1252 → utf-8 here!  Windows-1252
  ;; bytes 0x80–0x9F (smart quotes, en-dashes, ellipses) are NOT valid
  ;; UTF-8.  Overriding causes garbage on decode and "unknown characters"
  ;; prompts on reply.  Emacs already handles cp1252 natively via
  ;; `set-coding-system-priority' below.
  (setq mm-charset-override-alist '((iso-8859-1  . utf-8)
                                    (iso-8859-15 . utf-8)
                                    (us-ascii    . utf-8)
                                    (gb2312      . utf-8)
                                    (gbk         . utf-8)))

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
  ;; Maildir
  (setq mu4e-maildir "~/Mail")
  ;; Sync imap servers w/mbsync (via isync installed w/homebrew):
  ;; redirect STDERR to STDOUT to a log file
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -c ~/.config/.mbsyncrc  -aV 2>&1 > ~/.config/.mbsync.log"))
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

  (when (fboundp 'after-focus-change-function)
    (add-function :after after-focus-change-function
                  #'my-mu4e--on-focus-change))

  ;; ---- Network readiness check ----

  (defun my-mu4e--network-available-p ()
    "Return non-nil if the network appears to be up.
Uses a single ICMP ping to Cloudflare DNS (1.1.1.1) with a 2
second timeout.  Returns nil if the ping fails (no network) or
if `ping' is not found."
    (zerop (call-process "ping" nil nil nil "-c1" "-W2" "1.1.1.1")))

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
                             (> (float-time
                                 (time-subtract (current-time)
                                                (process-start-time proc)))
                                600)))))
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

    (defun mu4e-mark-at-point-advice (mark target)
      (interactive)
      (let* ((msg (mu4e-message-at-point))
             (docid (mu4e-message-field msg :docid))
             (overlay (make-overlay (- (line-end-position) 12)
                                    (- (line-end-position) 0))))
        (save-excursion
          ;; (remove-overlays (line-beginning-position) (line-end-position))
          (delete-overlay (make-overlay (line-beginning-position) (line-end-position)))
          (if (eql mark 'unmark)
              (delete-overlay overlay)
            (cond ((eql mark 'refile)
                   (overlay-put overlay 'display (svg-tag-make "ARCHIVE" nil 3 0)))
                  ((eql mark 'trash)
                   (overlay-put overlay 'display (svg-tag-make "TRASH" nil 5 0)))
                  ((eql mark 'untrash)
                   (overlay-put overlay 'display (svg-tag-make "UNTRASH" nil 3 0)))
                  ((eql mark 'delete)
                   (overlay-put overlay 'display (svg-tag-make "DELETE" nil 4 0)))
                  ((eql mark 'read)
                   (overlay-put overlay 'display (svg-tag-make "READ" nil 4 0)))
                  ((eql mark 'unread)
                   (overlay-put overlay 'display (svg-tag-make "UNREAD" nil 4 0)))
                  ((eql mark 'flag)
                   (overlay-put overlay 'display (svg-tag-make "FLAG" nil 6 0)))
                  ((eql mark 'unflag)
                   (overlay-put overlay 'display (svg-tag-make "UNFLAG" nil 4 0)))
                  ((eql mark 'move)
                   (overlay-put overlay 'display (svg-tag-make "MOVE" nil 6 0)))
                  ((eql mark 'tag)
                   (overlay-put overlay 'display (svg-tag-make "TAG" nil 7 0))))))))

    (advice-add 'mu4e-mark-at-point :after #'mu4e-mark-at-point-advice)) ;;with-eval-after-load

  ;;** Attachments
  ;; Set default attachment dir (create if missing)
  (setq mu4e-attachment-dir (concat (getenv "HOME") "/Downloads/_Mail"))
  (unless (file-directory-p mu4e-attachment-dir)
    (make-directory mu4e-attachment-dir t))
  ;; TODO: Fix the function below so all attachements work
  ;; (bind-key "e" #'mu4e-views-mu4e-save-all-atachments mu4e-headers-mode-map)
  
  (defun my-mu4e-view-save-all-attachments (&optional ask-dir)
    "Save all files from the current view buffer.

Adapted from mu4e-view-save-attachments

This applies to all MIME-parts that are \"attachment-like\" (have a filename),
regardless of their disposition.

With ASK-DIR is non-nil, user can specify the target-directory; otherwise
one is determined using `mu4e-attachment-dir'."
    (interactive "P")
    (let* ((parts (mu4e-view-mime-parts))
           (candidates  (seq-map
                         (lambda (fpart)
                           (cons ;; (filename . annotation)
                            (plist-get fpart :filename)
                            fpart))
                         (seq-filter
                          (lambda (part) (plist-get part :attachment-like))
                          parts)))
           (candidates (or candidates
                           (mu4e-warn "No attachments for this message")))

           (custom-dir (when ask-dir (read-directory-name
                                      "Save to directory: "
                                      mu4e-attachment-dir))))
      ;; we have determined what files to save, and where.

      ;; candidates is an alist of plists, I think?
      (seq-do (lambda (parts)
                ;; (debug)
                (let* ((part (cdr parts))
                       (path (mu4e--uniquify-file-name
                              (mu4e-join-paths
                               (or custom-dir (plist-get part :target-dir))
                               (plist-get part :filename)))))
                  (mm-save-part-to-file (plist-get part :handle) path)))
              candidates)) )

  ;; "export all" removed — mu4e's built-in `mu4e-view-save-attachments'
  ;; (bound to `e' in view mode) saves all when none are selected.



  ;;** searching
  (defun my-mu4e-search-for-sender (&optional msg)
    "Find mails sent from SENDER."
    (interactive)
    (mu4e-search (concat "from:" (nth 1 (flatten-list (mu4e-message-field msg :from))))))

  (defun my-mu4e-search-toggle-unread (&optional msg)
    "Toggle the flag:unread flag for the last query.
If the string 'flag:unread is in the last query, remove it.
Otherwise, add this string to the last query.
Execute search with that query."
    (interactive)
    ;; last query to be checked is stored in mu4e--reach-last-query variable
    ;; use (mu4e-seach ...) to execute the search after testing the search string
    
    )

  ;; define 'z' as the shortcut
  (add-to-list 'mu4e-view-actions
               '("zsearch for sender" . my-mu4e-search-for-sender) t)

  (add-to-list 'mu4e-headers-actions
               '("zsearch for sender" . my-mu4e-search-for-sender) t)

  ;;* Viewing
  (defun mu4e-get-account (msg)
    (let* ((maildir (mu4e-message-field msg :maildir))
           (maildir (substring maildir 1)))
      (nth 0 (split-string maildir "/"))))

  (defun mu4e-get-maildir (msg)
    (let* ((maildir (mu4e-message-field msg :maildir))
           (maildir (substring maildir 1)))
      (nth 0 (reverse (split-string maildir "/")))))

  (defun mu4e-get-mailbox (msg)
    (format "%s|%s" (mu4e-get-account msg) (mu4e-get-maildir msg)))

  ;; Relative dates
  (defun mu4e-headers-is-today (date)
    (= (- (time-to-days (current-time)) (time-to-days date)) 0))

  (defun mu4e-headers-is-yesterday (date)
    (= (- (time-to-days (current-time)) (time-to-days date)) 1))

  (defun mu4e-headers-relative-date (msg)
    (let* ((thread  (mu4e-message-field msg :thread))
           (level (plist-get thread :level))
           (empty-parent (and thread (plist-get thread :empty-parent)))
           (child   (and thread (> (plist-get thread :level) 0)))
           (unread  (memq 'unread  (mu4e-message-field msg :flags)))
           (date (mu4e-msg-field msg :date))
           (diff (- (time-to-days (current-time)) (time-to-days date)))
           (face 'bespoke-salient))
      (setq face 'bespoke-faded)
      (cond ((mu4e-headers-is-today date)
             (mu4e-headers-button (format-time-string "     %H:%M" date)
                                  face
                                  (format-time-string "Mails from today")
                                  (format-time-string "date:%Y%m%d" date)))
            ((mu4e-headers-is-yesterday date)
             (mu4e-headers-button " Yesterday"
                                  face
                                  (format-time-string "Mails from yesterday")
                                  (format-time-string "date:%Y%m%d" date)))
            (t  (mu4e-headers-date-button date face)))))

  ;; Style & determine what flags to show
  (defun mu4e-headers-attach (msg)
    (cond ((memq 'flagged  (mu4e-message-field msg :flags))
           (propertize "!" 'face 'lambda-strong))
          ((memq 'attach  (mu4e-message-field msg :flags))
           (propertize "" 'face 'lambda-faded))
          (t " ")))

  ;;** Headers
  ;; Set headers
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                                 :shortname ""
                                 :function (lambda (msg) "  "))))

  (add-to-list 'mu4e-header-info-custom
               '(:relative-date . (:name "Relative date"
                                         :shortname ""
                                         :function mu4e-headers-relative-date)))

  (add-to-list 'mu4e-header-info-custom
               '(:mailbox-short . (:name "Mailbox"
                                         :shortname ""
                                         :function mu4e-get-mailbox)))

  (add-to-list 'mu4e-header-info-custom
               '(:attach . (:name "Attachment"
                                  :shortname ""
                                  :function mu4e-headers-attach)))

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
  ;; View in browser (works from both headers and view via raw file dissection)
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-headers-actions '("view in browser" . mu4e-action-view-in-browser) t)
  ;; Remove xwidget duplicate — "view in browser" covers it
  (setq mu4e-view-actions
        (cl-remove-if (lambda (a) (equal (car a) "xview in xwidget"))
                       mu4e-view-actions))

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
    (interactive)
    (let* ((path (mu4e-message-field msg :path))
           (header  mu4e-action-tags-header)
           (sep     (cond ((string= header "Keywords") ", ")
                          ((string= header "X-Label") " ")
                          ((string= header "X-Keywords") ", ")
                          (t ", ")))
           )
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

  ;; tag or untag one or many emails in header view. Can use '*' to act on multiple
  (add-to-list 'mu4e-marks
               '(tag
                 :char       "g"
                 :prompt     "gtag"
                 :ask-target nil
                 :action     (lambda (docid msg target)
                               (mu4e-action-retag-message msg target))))

  (add-to-list 'mu4e-marks
               '(tag
                 :char       "G"
                 :prompt     "Guntag"
                 :action     (lambda (docid msg target)
                               (my-mu4e-remove-all-tags msg))))

  ;; this function will remove all tags from a msg by erasing the mu4e-actions-tags-header-line
  ;; based on mu4e-action-retag-message
  (defun my-mu4e-remove-all-tags-message (msg)
    "Remove all tags from MSG."
    (interactive)
    (let* ((path (mu4e-message-field msg :path))
           (header  mu4e-action-tags-header)
           (sep     (cond ((string= header "Keywords") ", ")
                          ((string= header "X-Label") " ")
                          ((string= header "X-Keywords") ", ")
                          (t ", ")))
           )
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

  (add-to-list 'mu4e-view-actions
	           '("Rremove all tags" . my-mu4e-remove-all-tags-message) t)

  (add-to-list 'mu4e-headers-actions
	           '("Rremove all tags" . my-mu4e-remove-all-tags-message) t)

  (defun my-mu4e-copy-message-path (msg)
    "Copy the file path of MSG to the kill ring."
    (let ((path (mu4e-message-field msg :path)))
      (kill-new path)
      (message "Copied: %s" path)))

  (add-to-list 'mu4e-view-actions
               '("yank path" . my-mu4e-copy-message-path) t)

  (add-to-list 'mu4e-headers-actions
               '("yank path" . my-mu4e-copy-message-path) t)

  (add-to-list 'mu4e-view-actions
	           '("retag message" . mu4e-action-retag-message) t)

  (add-to-list 'mu4e-headers-actions
	           '("retag message" . mu4e-action-retag-message) t)


  (defun my-mu4e-last-month ()
    "Return the last year-month as a string for mu searches.

Note: this function is actually not necessary because I learned how to use mu find better"
    (interactive)
    (let* ((cur-time (decode-time))
           (cur-year (nth 5 cur-time))
           (cur-month (nth 4 cur-time))
           ;; (cur-month 1) ; testing if dates roll over elegantly
           (last-month (if (= cur-month 1) 12 (- cur-month 1)))
           (last-year (if (= cur-month 1) (- cur-year 1) cur-year)))
      (format "%d-%02d" last-year last-month)))

  ;; See https://github.com/panjie/mu4e-goodies
  ;; view the mails sent by the sender of current mail
  (defun mu4e-msgv-action-sender-related-mails (msg)
    "Search all mails sent by current message's sender."
    (mu4e-headers-search
     (concat "from:" (cdar (mu4e-message-field msg :from)))))

  ;; create org link to message
  (require 'mu4e-org)
  (add-to-list 'mu4e-view-actions
               '("org link" . org-store-link) t)
  (add-to-list 'mu4e-headers-actions
               '("org link" . org-store-link) t)

  ;; add sender to org-contacts (press 'a' then 'o' in view/headers)
  ;; NOTE: org-contacts can aggressively override mu4e's native contact
  ;; completion. If that happens, set `org-contacts-enable-completion' to
  ;; nil in the org-contacts use-package (in my-setup-notes.el).
  (setq mu4e-org-contacts-file (concat org-directory "contacts.org"))
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)

  ;; TODO: implement this function to open mu4e links from org in another frame
  ;; I need to modify how the org-link behavior works in org
  (defun mu4e-org-open-new-frame (link)
    "Open the org LINK in another frame.
Open the mu4e message (for links starting with \"msgid:\") or run
the query (for links starting with \"query:\")."
    (require 'mu4e)
    (cond
     ((string-match "^msgid:\\(.+\\)" link)
      (select-frame-set-input-focus (make-frame))
      (mu4e-view-message-with-message-id (match-string 1 link)))
     ((string-match "^query:\\(.+\\)" link)
      (select-frame-set-input-focus (make-frame))
      (mu4e-search (match-string 1 link) current-prefix-arg))
     (t (mu4e-error "Unrecognized link type '%s'" link))))

;;;; Composing Email
  (add-hook 'mu4e-compose-mode-hook
            (defun my-mu4e-compose-settings ()
              "My initial settings for message comoposition"
              (my-mu4e-add-cc-bcc)
              (my-swap-email-from-field)
              ;; Check spelling
              (flyspell-mode 1)
              (olivetti-mode 1)
              (olivetti-set-width 0.8)
              (auto-fill-mode -1)
              (hl-line-mode -1)))

  ;; Use mu4e system-wide
  (setq mail-user-agent 'mu4e-user-agent)

  (set-variable 'read-mail-command 'mu4e)
  ;; List of your email adresses:
  (setq mu4e-user-mail-address-list '("REDACTED@example.com"
                                      "REDACTED@example.com"))

  ;; Compose in new buffer (can make 'window for new window)
  (setq mu4e-compose-switch nil)

  ;; Give compose the full frame.  We use `mu4e-compose-switch' = nil
  ;; (current window), so compose already replaces whatever buffer was
  ;; in that window.  If mu4e split to show headers + view before
  ;; composing, delete the *other* window so compose is alone.
  ;;
  ;; IMPORTANT: mu4e saves the window configuration BEFORE
  ;; `mu4e-compose-mode-hook' runs, so modifying windows here is safe —
  ;; the restore after send/kill will recreate the original layout.
  (defun my-mu4e-hide-headers-on-compose ()
    "Give compose the full frame by deleting other windows."
    (delete-other-windows))
  (add-hook 'mu4e-compose-mode-hook #'my-mu4e-hide-headers-on-compose)

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

  ;; Only ask if a context hasn't been previously picked
  (setq mu4e-compose-context-policy 'ask-if-none)

  ;; Possible fix for outlook client reading problems in inline messages
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#How-can-I-avoid-Outlook-display-issues_003f
  (setq  message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote...")

  ;; Filtering function to remove annoying composition auto-completes
  ;; https://emacs.stackexchange.com/questions/47789/how-to-remove-email-address-from-local-database-in-mu4e
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Contact-functions.html
  (defun my-mu4e-contact-filter (addr)
    "Remove annoying completions from the email autocomplete. Uses regex to identify offending emails."
    (cond
     ;; remove unwanted auto-completes
     ((string-match-p "REDACTED@example.com" addr) nil) ;; unwanted gmail
     ((string-match-p "REDACTED@example.com" addr) nil) ;; unwanted alt email
     ((string-match-p "no-reply" addr) nil)
     ((string-match-p "noreply" addr) nil)
     ((string-match-p "@finkelteinlab" addr) nil)
     ((string-match-p "@fortelabs" addr) nil)
     ((string-match-p "noreply@box.com" addr) nil) ;; random Box emails
     ((string-match-p "REDACTED@example.com" addr) nil)
     ((string-match-p "REDACTED@example.com" addr) nil)
     ((string-match-p "REDACTED@example.com" addr) nil) ;; alt email do not use
     ((string-match-p "REDACTED@example.com" addr) nil)
     ((string-match-p "via Canvas" addr) nil)
     ((string-match-p "REDACTED@example.com" addr) nil) ;; old email
     ((string-match-p "unwanted.user@somedomain.com)" addr) nil)
     ((string-match-p "REDACTED@example.com)" addr) nil) ;; alt email do not use
     ;; auto-corrects -->
     ;; jonh smiht --> John Smith
     ;; ((string-match "jonh smiht" contact)
     ;;  (replace-regexp-in-string "jonh smiht" "John Smith" contact))
     (t addr)))

  (setq mu4e-contact-process-function 'my-mu4e-contact-filter)

  ;;tweak the composition window to include CC and BCC
  (defun my-mu4e-add-cc-bcc ()
    "Add CC and BCC to all new e-mails"
    (interactive)
    "Add a Cc: & Bcc: header."
    (save-excursion (message-add-header "Cc: \nBcc: \n")))

  (defun my-swap-email-from-field ()
    "Swap from field in emails to REDACTED@example.com to redirect e-mails to that account"
    (interactive)
    (save-excursion
      (message-remove-header "From" nil 'FIRST) ;;remove only first instance of header to deal with replies
      (goto-char (point-min))
      (insert "From: " (message-make-from "REDACTED" "REDACTED@example.com") "\n")))

  ;; a few helper functions to navigate & copy message fields quickly
  (defun my-kill-email-address-at-point ()
    "If there is one, copy the e-mail address at point to the kill-ring.
Strips surrounding angle brackets if present."
    (interactive)
    (when-let* ((addr (thing-at-point 'email))
                (clean (string-trim addr "<" ">")))
      (kill-new clean)
      (message "Copied: %s" clean)))

  ;; (defun my-msg-goto-to-field ()
  ;;   "Move point to the beginning of the To field."
  ;;   (interactive)
  ;;   (goto-char (point-min))
  ;;   (search-forward "To: " nil t)
  ;;   (goto-char (match-end 0)))

  ;; (defun my-msg-goto-cc-field ()
  ;;   "Move point to the beginning of the To field."
  ;;   (interactive)
  ;;   (goto-char (point-min))
  ;;   (search-forward "Cc: " nil t)
  ;;   (goto-char (match-end 0)))

;;;; Sending Mail
  ;; Configure the function to use for sending mail
  ;; use msmtp to manage sending mail from different e-mail accounts
  ;; reference: https://tushartyagi.com/blog/configure-mu4e-and-msmtp
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        ;; NB: use backquote + expand-file-name because Emacs calls
        ;; msmtp via call-process-region (no shell), so ~ is NOT expanded.
        message-sendmail-extra-arguments `("--read-envelope-from"
                                           "-C" ,(expand-file-name ".msmtprc" "~/.config/"))
        message-send-mail-function 'message-send-mail-with-sendmail)

  ;; I am not sure what the line below is supposed to do.
  (setq smtpmail-queue-dir (concat mu4e-maildir "/queued-mail/"))


  ;;** Contexts
  (setq mu4e-contexts
        (list
         ;; Work account(s). First one is considered default for picking policies
         (make-mu4e-context
          :name "Lab"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Lab" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address   . "REDACTED@example.com")
                  (user-full-name      . "REDACTED")
                  (mu4e-drafts-folder  . "/Lab/Drafts")
                  (mu4e-sent-folder    . "/Lab/Sent")
                  (mu4e-refile-folder  . "/Lab/Archive")
                  (mu4e-trash-folder   . "/Lab/Trash")
                  (smtpmail-smtp-user  . "REDACTED@example.com")))

         (make-mu4e-context
          :name "UT"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/UT" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address   . "REDACTED@example.com")
                  (user-full-name      . "REDACTED")
                  (mu4e-drafts-folder  . "/UT/Drafts")
                  (mu4e-sent-folder    . "/UT/Sent")
                  (mu4e-refile-folder  . "/UT/Archive")
                  (mu4e-trash-folder   . "/UT/Trash")
                  (smtpmail-smtp-user  . "REDACTED@example.com")))

         (make-mu4e-context
          :name "Test"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/gmail-test" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address   . "REDACTED@example.com")
                  (user-full-name      . "Test account")
                  (mu4e-drafts-folder  . "/gmail-test/[Gmail]/Drafts")
                  (mu4e-sent-folder    . "/gmail-test/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/gmail-test/Archive")
                  (mu4e-trash-folder   . "/gmail-test/[Gmail]/Trash")
                  (smtpmail-smtp-user  . "REDACTED@example.com")))
))

  ;; Ask for context if none is set
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'pick-first)

  ;;** Quick Actions & helper functions
  ;; Helpful discussion at
  ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-05.org

  ;;*** LLM-enhanced TODO capture from email
  ;; Uses pi + haiku to analyze the email, then opens org-capture
  ;; with title, deadline, actions, context pre-populated.
  ;; Template "m" in `org-capture-templates' uses %(sexp) to pull
  ;; pending deadline and body from these variables.

  (defvar my-mu4e-todo--pending-deadline nil
    "Pending deadline string for the next org-capture, or nil.")

  (defvar my-mu4e-todo--pending-body nil
    "Pending body string for the next org-capture, or nil.")

  (defun my-mu4e-todo--deadline-string ()
    "Return DEADLINE line if pending, empty string otherwise.
Consumes the pending value.  Called by %(sexp) in the \"m\" template."
    (prog1 (if my-mu4e-todo--pending-deadline
                (format "DEADLINE: <%s>\n" my-mu4e-todo--pending-deadline)
              "")
      (setq my-mu4e-todo--pending-deadline nil)))

  (defun my-mu4e-todo--body-string ()
    "Return body content if pending, empty string otherwise.
Consumes the pending value.  Called by %(sexp) in the \"m\" template."
    (prog1 (or my-mu4e-todo--pending-body "")
      (setq my-mu4e-todo--pending-body nil)))

  (declare-function my-todo-from-email--build-body "private")
  (declare-function my-remove-triple-ticks "my-setup-ai")

  (defun my-mu4e-capture-mail-gtd (msg)
    "Capture email MSG as a TODO with LLM-generated summary.
Stores the mu4e link, sends the email body to pi CLI (haiku) for
analysis, then opens `org-capture' with the heading, deadline,
action items, and context pre-populated.  The user can edit
everything in the capture buffer before confirming.

Falls back to a plain capture buffer if the LLM fails."
    (interactive)
    (org-store-link nil t)
    (let* ((pi-program (or my-todo-from-email-pi-program
                           (executable-find "pi")))
           (subject    (mu4e-message-field msg :subject))
           (email-body (mu4e-view-message-text msg))
           (tmpfile    (make-temp-file "mu4e-todo-" nil ".txt"))
           (proc-buf   (generate-new-buffer " *todo-from-email*"))
           (prompt     (concat
                        "Analyze this email for " user-full-name ".  "
                        "Subject: " subject "\n\n"
                        "Return ONLY a valid JSON object:\n"
                        "{\"title\": \"brief imperative action (5-8 words, use names)\","
                        " \"actions\": [\"specific next step 1\", \"next step 2\"],"
                        " \"deadline\": \"YYYY-MM-DD or null if none mentioned\","
                        " \"context\": \"1-2 sentences of essential context\","
                        " \"refs\": [\"bare URLs (no labels) or plain email addresses\"]}\n\n"
                        "Focus on what " user-full-name " must DO.  Be terse.  "
                        "Only include deadline if explicitly stated.")))
      (unless pi-program
        (user-error "Cannot find `pi' binary on PATH"))
      (with-temp-file tmpfile (insert email-body))
      (message "[todo] Analyzing: %s..." subject)
      (let ((proc
             (make-process
              :name "todo-from-email"
              :buffer proc-buf
              :command (list pi-program
                             "--print" "--mode" "text"
                             "--no-tools" "--no-session"
                             "--no-extensions" "--no-skills"
                             "--no-prompt-templates" "--no-themes"
                             "--provider" "anthropic"
                             "--model" "claude-haiku-4-5"
                             "--system-prompt"
                             "Respond with valid JSON only. No markdown fences."
                             (concat "@" tmpfile)
                             prompt)
              :sentinel
              (lambda (process _signal)
                (when (eq (process-status process) 'exit)
                  (unwind-protect
                      (let ((title nil)
                            (ok (zerop (process-exit-status process))))
                        (when ok
                          (condition-case err
                              (let* ((output (with-current-buffer proc-buf
                                               (buffer-substring-no-properties
                                                (point-min) (point-max))))
                                     (cleaned (my-remove-triple-ticks output))
                                     (result (json-parse-string
                                              cleaned
                                              :object-type 'plist
                                              :array-type 'list
                                              :null-object nil
                                              :false-object nil)))
                                (setq title (or (plist-get result :title) "Process email"))
                                (setq my-mu4e-todo--pending-deadline
                                      (plist-get result :deadline))
                                (setq my-mu4e-todo--pending-body
                                      (let ((body (my-todo-from-email--build-body result)))
                                        (unless (string-empty-p body)
                                          (concat body "\n")))))
                            (error
                             (message "[todo] LLM parse error: %s"
                                      (error-message-string err)))))
                        ;; Open capture — with or without LLM pre-population.
                        (if title
                            (org-capture-string title "m")
                          (org-capture nil "m")))
                    (when (file-exists-p tmpfile) (delete-file tmpfile))
                    (when (buffer-live-p proc-buf) (kill-buffer proc-buf))))))))
        (set-process-query-on-exit-flag proc nil))))

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions
               '("todo" . my-mu4e-capture-mail-gtd) t)
  (add-to-list 'mu4e-view-actions
               '("todo" . my-mu4e-capture-mail-gtd) t)

  
  ;; copy fields to kill ring
  (defun my-copy-email-field-to-kill-ring (FIELD)
    "Copy FIELD from the open org-msg buffer to the kill-ring"
    (interactive "P")
    (kill-new
     (message-fetch-field FIELD)))

  
  ;;disable threading in search results
  (setq mu4e-headers-show-threads nil)

  ;; Note that :hide-unread is implied when the query is not a string; this for the common case where the query function involves some user input, which would be disruptive in this case.
  (setq mu4e-bookmarks
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
          (:name "Home"
                 :query ,(mu4e-make-query
                          '(from "REDACTED@example.com"))
                 :key ?h)
          (:name "HR"
                 :query ,(mu4e-make-query
                          '(from "REDACTED@example.com"))
                 :key ?H)
          (:name "From Contact"
                 :query ,(mu4e-make-query
                          '(from "REDACTED"))
                 :key ?j)
          (:name "Running"
                 :query ,(mu4e-make-query
                          '(or (from "REDACTED@example.com") ("REDACTED")))
                 :key ?R)
          (:name "Price Watch"
                 :query ,(mu4e-make-query
                          '(or (from "REDACTED@example.com") (from "REDACTED@example.com")))
                 :key ?P)))
  ;; (setq mu4e-bookmarks
  ;;       '((:name "Unread"
  ;;                :query (lambda ()
  ;;                         (concat "flag:unread AND NOT flag:trashed AND " my-mu4e-inboxes))
  ;;                :key ?u)
  ;;         ;; (:name "UT Staff" :query "from:REDACTED" :key ?M)

  ;;         (:name "Search"
  ;;                :query "flag:unread AND (from:jobalerts-noreply@linkedin.com OR from:noreply@jobmail.naturecareers.com OR from:noreply@jobrxiv.org OR from:reply@sciencecareers.org OR from:support@academicjobsonline.org OR from:jobseeker@higheredjobs.com OR from:alert@mail.jobs.chronicle.com OR from:no-reply@postmaster.cell.com OR jobalert@higheredjobs.com)"
  ;;                :key ?J)
  ;;         ))



;;;;; Maildirs
  ;; maildirs used frequently; access them with 'j' ('jump')
  (setq mu4e-maildir-shortcuts
	    '(("/UT/Inbox"               . ?u)
	      ("/Lab/Inbox"              . ?l)
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

  (define-key mu4e-compose-minor-mode-map (kbd "R")
              #'my-mu4e-compose-reply-ask-wide)

  ;; TODO: need to check if this works
  (defun my-email-message-to-kill-ring ()
    "Yank the current mu4e message body text into the kill ring.
Must be in mu4e-view-mode major mode."
    (interactive)
    (if (eq major-mode 'mu4e-view-mode)
        (progn
          (save-excursion
            (let ((start (message-goto-body))
                  (end (point-max)))
              (kill-ring-save start end)
              (message "E-mail copied to the kill ring"))))
      (message "Not a mu4e-view-mode buffer")))

  (defun my-mu4e-yank-message ()
    "Yank the current message text into the kill ring"
    ;; TODO: make sure cursor is in mu4e-view mode
    (interactive)

    (save-excursion
      (let ((start (message-goto-body))
            (end (point-max)))
        (kill-ring-save start end))))

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

  ;; Use completing-read
  (setq mu4e-completing-read-function 'completing-read)

  ;; Store link to message if in header view, not to header query
  (setq mu4e-org-link-query-in-headers-mode nil)

  ;; Quickly store links for search queries
  (defun cpm/store-link-to-mu4e-query ()
    (interactive)
    (let ((org-mu4e-link-query-in-headers-mode t))
      (call-interactively 'org-store-link)))

  ;; Go to unread. Only show unread mail from main inbox.
  (defvar my-mu4e-unread-query "flag:unread AND NOT flag:trashed AND (maildir:/UT/Inbox OR maildir:/Lab/Inbox OR maildir:/gmail-test/Inbox)")

  (defun my-go-to-unread-mail ()
    (interactive)
    (if (member "Email" (tabspaces--list-tabspaces))
        (progn
          (tab-bar-switch-to-tab "Email")
          (mu4e-headers-search my-mu4e-unread-query))
      (progn
        (tabspaces-create-workspace)
        (tab-bar-rename-tab "Email")
        (find-file (concat org-directory "inbox.org"))
        (mu4e)
        (mu4e-headers-search my-mu4e-unread-query))))
  ;; search email inbox
  (defun my-search-all-mail ()
    (interactive)
    (if (member "Email" (tabspaces--list-tabspaces))
        (progn
          (tab-bar-switch-to-tab "Email")
          (mu4e-headers-search))
      (progn
        (tabspaces-create-workspace)
        (tab-bar-rename-tab "Email")
        (mu4e)
        (mu4e-headers-search))))
  ) ;; use-package mu4e
;;** End Mu4e

;;* mu4e-goodies (AI chat + spam check)
(use-package mu4e-goodies
  :load-path "~/projects/elisp/mu4e-goodies"
  :after mu4e
  :config
  (mu4e-goodies-setup))

;;* mu4e-query for building searches
(use-package mu4e-query
  :vc (:url "https://github.com/mickeynp/mu4e-query")
  :commands (mu4e-make-query)
  ;; :config
  )
;;* Using Org & HTML (Org-Msg)
(use-package org-msg
  :after (mu4e)
  :ensure t
  :vc (:url "https://github.com/jeremy-compostella/org-msg" :branch "main")
  ;; :disabled t
  ;; avoid pesky org ASCII Export buffer
  ;; https://github.com/jeremy-compostella/org-msg/issues/169
  :preface
  (defun org-msg-no-temp-buffer (orig-fun &rest args)
    "Advice to set `org-export-show-temporary-export-buffer' to `nil'."
    (let ((org-export-show-temporary-export-buffer nil))
      (apply orig-fun args)) )
  :bind (:map org-msg-edit-mode-map
              ("s-<return>" . org-msg-goto-body))  ;; Firefox tridactyl-like bindings
  :hook ((mu4e-compose-pre . org-msg-mode))
  :config
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
     [("ra" "Contact A" (lambda ()
                      (interactive)
                      (insert "REDACTED@example.com")))
      ("rc" "Contact C" (lambda ()
                      (interactive)
                      (insert "REDACTED@example.com")))
      ("rj" "Contact J" (lambda ()
                         (interactive)
                         (insert "REDACTED@example.com")))
      ("rh" "HR" (lambda ()
                   (interactive)
                   (insert "REDACTED@example.com")))]
     [("l" "llm expand" my-mail-llm-expand)]
     ])
  ;; (define-key message-mode-map (kbd "M-o") 'my-transient-email-compose)
  (define-key org-msg-edit-mode-map (kbd "M-o") 'my-transient-email-compose)



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
              ;; Kill stale article buffer so it doesn't confuse mu4e's
              ;; window restoration (do NOT call mu4e-view-quit here —
              ;; that navigates to headers and races with mu4e's own
              ;; post-compose window-config restore, producing duplicate
              ;; headers windows).
              (when-let* ((buf (get-buffer "*mu4e-article*")))
                (kill-buffer buf))
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
        org-msg-convert-citation nil
        ;;         org-msg-signature "
        ;;  Regards,
        ;; #+begin_signature
        ;; Ilya
        ;;  #+end_signature"
        ;; 	    org-msg-greeting-fmt "\nHi%s,\n\n"
        )
  ;; org-msg checks for attachments via a regex search for keywords.
  ;; this elisp regex expression always evaluates as a null hit on all strings
  ;; ref: https://stackoverflow.com/questions/1723182/a-regex-that-will-never-be-matched-by-anything
  ;; note: i need to use emacs-specific regex here.
  (setq org-msg-attached-file-reference "\`\b\'")

  (defun cpm/org-msg-hooks ()
    "Hooks for org-msg"
    (auto-fill-mode -1)
    (hl-line-mode -1)  ;; disable highlight line when composing emails
    (diff-hl-mode -1) ;; disable diff gutter
    ;; Shrink org-meta-line (#+begin_src, #+PROPERTIES, etc.) in compose buffers only
    (face-remap-add-relative 'org-meta-line :height 0.5 :foreground "gray60")
    ;; FIXME: Try remove auto-save hook *locally* to avoid multiple saved drafts
    (remove-hook 'auto-save-hook #'cpm/full-auto-save t))
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
  ;; (define-key org-msg-edit-mode-map (kbd "C-c f") 'my-hydra-jump-to-email-fields/body)
  ;; (define-key org-msg-edit-mode-map (kbd "C-c w") 'my-hydra-copy-email-fields-to-kill-RING/BODY)

  ) ;; org-msg

;;* Org Heading to Email (org-heading-mail)
;; Convert org headings to mu4e/org-msg emails with auto-attachment
(use-package org-heading-mail
  :load-path "~/projects/elisp/org-heading-mail"
  :after org-msg
  :commands (org-heading-mail-send))

(defun my-mu4e-attach-png-from-clipboard ()
  "Save a PNG image from the clipboard to a temp file and attach it. MacOS or linux only, for now."
  (interactive)
  (unless (or (derived-mode-p 'mu4e-compose-mode)
              (derived-mode-p 'org-msg-edit-mode))
    (error "Not in a mu4e-compose-mode or org-msg-edit-mode buffer"))

  (let* ((paste-tool
          (cond
           ((executable-find "pngpaste") '("pngpaste"))
           ((executable-find "xclip")    '("xclip" "-selection" "clipboard" "-t" "image/png" "-o"))
           (t (error "Please install 'pngpaste' (macOS) or 'xclip' (Linux)"))))
         (tmp-file (make-temp-file "org-msg-clipboard-" nil ".png")))

    ;; Run the clipboard tool. The invocation depends on the tool used.
    (let ((command (car paste-tool))
          (args (cdr paste-tool)))
      (cond ((string= command "pngpaste")
             ;; pngpaste takes the output file as a command-line argument.
             ;; (message "Running: %s %s" command tmp-file)
             (call-process command nil nil nil tmp-file))
            ((string= command "xclip")
             ;; xclip writes to standard output, which we redirect to the file.
             ;; (message "Running: %s %s > %s" command (mapconcat #'identity args " ") tmp-file)
             (apply #'call-process command nil tmp-file nil args))
            (t (error "Unsupported paste tool: %s" command))))
    
    (if (> (file-attribute-size (file-attributes tmp-file)) 0)
        (progn
          ;; `mml-attach-file` is the standard Emacs function for this.
          (mml-attach-file tmp-file)
          ;; (message "Attached image from clipboard: %s" (file-name-nondirectory tmp-file)))
          (delete-file tmp-file)
          (error "Command '%S' produced no output or failed. Make sure that an image is in the clipboard." paste-tool)))))

(defun my-mu4e-queries-help ()
  "Look up search syntax"
  (interactive)
  (split-window-vertically)
  (woman "mu-query")
  (search-forward "FIELDS"))

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
             "\n\nSign off with:\nKind regards,\nIlya"
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


(defun my-daily-email-progress ()
  "Plot my daily, weekly, and monthly email progress.
A shell script queries mu every five minutes via the xbar app."
  (interactive "P")
  (let (file "~/Work/01-09 meta/01 productivity/email-inbox-count-log.csv")
    ))

;;; Email Addressing
;; Return first name of email recipients
;; inspired by
;;http://blog.binchen.org/posts/how-to-use-yasnippets-to-produce-email-templates-in-emacs.html
;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/
(defun my-mu4e-get-names-of-recipients ()
  "Return comma separated string of names for an email"
  (interactive)
  (let ((email-name "") str email-string email-list email-name2 tmpname)
    (save-excursion
      (goto-char (point-min))
      ;; first line in email could be some hidden line containing NO to field
      (setq str (buffer-substring-no-properties (point-min) (point-max))))
    ;; take name from TO field - match series of names
    (when (string-match "^To: \"?\\(.+\\)" str)
      (setq email-string (match-string 1 str)))
    ;;split to list by comma
    (setq email-list (split-string email-string " *, *"))
    ;;loop over emails
    (dolist (tmpstr email-list)
      ;;get first word of email string
      (setq tmpname (car (split-string tmpstr " ")))
      ;;remove whitespace or ""
      (setq tmpname (replace-regexp-in-string "[ \"]" "" tmpname))
      ;;join to string
      (setq email-name
            (concat email-name ", " tmpname)))
    ;;remove initial comma
    (setq email-name (replace-regexp-in-string "^, " "" email-name))

    ;;see if we want to use the name in the FROM field
    ;;get name in FROM field if available, but only if there is only
    ;;one name in TO field
    (if (< (length email-list) 2)
        (when (string-match "^\\([^ ,\n]+\\).+writes:$" str)
          (progn (setq email-name2 (match-string 1 str))
                 ;;prefer name in FROM field if TO field has "@"
                 (when (string-match "@" email-name)
                   (setq email-name email-name2))
                 )))
    email-name))


;;* Colors for header view
(use-package mu4e-column-faces
  :after mu4e
  :custom-face
  (mu4e-column-faces-date ((t (:foreground unspecified :background unspecified :inherit lambda-green))))
  (gnus-header-name ((t (:foreground unspecified :background unspecified :inherit lambda-green))))
  (mu4e-column-faces-to-from ((t (:foreground unspecified :background unspecified :inherit lambda-focus))))
  :config
  (mu4e-column-faces-mode))

;;* mu4e-helpers and lifestyle improvements
(use-package mu4e-contrib
  :after mu4e
  :ensure nil
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e")

;;* provide my-setup-mail
(provide 'my-setup-mail)
;;* End Setup Email
