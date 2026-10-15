;;; my-setup-mail.el -*- lexical-binding: t -*-

;; I use mbsync and mu4e
;;; Mu4e
;; Note: since 1.12, mu4e composes and displays messages via Gnus.
;;* mu4e
(use-package mu4e
  :ensure nil
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
  :commands (mu4e mu4e-compose-new mu4e-update-mail-and-index
                  mu4e-transient-menu)
  ;; Change keybindings to look more like elfeed
  ;; in headers view, "r" for read messages, "u" for unread messages
  :bind (:map mu4e-view-mode-map
              ("w" . mu4e-copy-thing-at-point) ; "c" is taken by mu4e-search-minor-mode-map (mu4e-search-query)
              ("f" . link-hint-open-link)
              ("r" . mu4e-headers-mark-for-read)
              ("u" . mu4e-headers-mark-for-unread)
              ("!" . mu4e-headers-mark-for-refile)
              ("?" . mu4e-headers-mark-for-unmark)
              ("L" . org-store-link)
              ("M-o" . mu4e-transient-menu)

              :map mu4e-headers-mode-map
              ("r" . mu4e-headers-mark-for-read)
              ("u" . mu4e-headers-mark-for-unread)
              ("!" . mu4e-headers-mark-for-refile)
              ("?" . mu4e-headers-mark-for-unmark)
              ("L" . org-store-link)
              ("M-o" . mu4e-transient-menu)
              :map mu4e-main-mode-map
              ("u" . mu4e-update-index )
              ("M-o" . mu4e-transient-menu)
              :map mu4e-compose-minor-mode-map
              ("R" . my-mu4e-compose-reply-ask-wide))

  :config
  ;; M-x only, deliberately unbound: marks all visible unread as read.
  (autoload 'mu4e-headers-flag-all-read "mu4e-contrib" nil t)
  ;; Fix a problem with some escape characters
  ;; https://github.com/djcb/mu/issues/2662
  (setq rfc2047-quote-decoded-words-containing-tspecials t)

  ;; Keep the current split when quitting the headers buffer.
  ;; `mu4e~headers-quit-buffer' calls `quit-window', and
  ;; `quit-restore-window' resizes the window back to the absolute
  ;; width it had when headers was first shown (stored as the 4th
  ;; element of the quit-restore "quad").  After a frame shrink (e.g.
  ;; external monitor -> laptop), `balance-windows', or a manual
  ;; resize, that stale width squeezes the neighbouring window: seen
  ;; as main 105 cols / other window 17 cols on a 124-col frame.
  ;; Dropping the stored size makes quit restore the buffer only.
  ;; Same mutation window.el itself uses for this (Bug#78835).
  (defun my-mu4e--drop-quit-restore-size (window)
    "Remove the stored window size from WINDOW's quit-restore parameters."
    (dolist (param '(quit-restore quit-restore-prev))
      (when-let* ((quad (nth 1 (window-parameter window param)))
                  ((consp quad))
                  ((integerp (nth 3 quad))))
        (setf (nth 3 quad) nil))))

  (define-advice mu4e~headers-quit-buffer (:before () keep-split)
    "Stop `quit-window' from snapping headers back to a stale width."
    (my-mu4e--drop-quit-restore-size (selected-window)))

  ;; Charsets.  Emacs' default overrides already decode Outlook's
  ;; "iso-8859-1" as its windows-1252 superset (smart quotes, dashes)
  ;; and GB2312 as GBK; add UTF-8 mail mislabeled as us-ascii.
  (with-eval-after-load 'mm-util
    (add-to-list 'mm-charset-override-alist '(us-ascii . utf-8)))
  ;; Encode outgoing mail as UTF-8 when it isn't plain ASCII.
  (setq mm-coding-system-priorities '(utf-8))

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
  ;; Trash = a plain move to the trash folder.  With the Maildir T flag,
  ;; mbsync's `Expunge Both' deletes the message on the server at once
  ;; instead of leaving it in the server's (recoverable) trash.
  (setq mu4e-trash-without-flag t)
  ;; Refresh mail using mbsync every 5 minutes
  (setq mu4e-update-interval (* 5 60))

  ;;** Sleep/Wake Recovery for mu4e
  ;;
  ;; macOS sleep breaks an in-flight sync: mbsync's connection is dead
  ;; after wake but the process lives on, so every later
  ;; `mu4e-update-mail-and-index' says "already running".  And Wi-Fi
  ;; needs a few seconds after wake, so an immediate sync fails on DNS.
  ;; `system-sleep' (Emacs 31) delivers real pre-sleep/post-wake events:
  ;;
  ;;   pre-sleep : kill a running mbsync so Emacs sees it exit.
  ;;   post-wake : a few seconds later, kill any leftover mbsync, wait
  ;;               for the network without blocking, then sync.
  ;;
  ;; The repeating update timer needs no help (timer.el catches up
  ;; after a clock jump), and mu4e restarts a dead mu server on its
  ;; next request.  Hook functions must not signal (that halts the
  ;; remaining hook functions), so the work runs from a timer.

  (require 'system-sleep)

  (defvar my-mu4e--wake-timer nil
    "Pending recovery timer scheduled by the last post-wake event.")

  (defvar my-mu4e--network-timer nil
    "Pending network-wait timer, so a new wake can cancel an old wait.")

  (defvar my-mu4e--network-attempts 10
    "How many times to probe the network after wake, 3s apart.")

  (defun my-mu4e--when-network-up (fn &optional attempts)
    "Call FN once the network is reachable, probing up to ATTEMPTS times.
Probes with a single ICMP ping to 1.1.1.1 (2s timeout) run
asynchronously, so Emacs never blocks.  If `ping' is missing,
assume the network is up.  Gives up silently after ATTEMPTS
\(default `my-mu4e--network-attempts') probes 3s apart."
    (let ((attempts (or attempts my-mu4e--network-attempts)))
      (when (timerp my-mu4e--network-timer)
        (cancel-timer my-mu4e--network-timer)
        (setq my-mu4e--network-timer nil))
      (cond
       ((<= attempts 0)
        (mu4e-message "Network not available after wake — skipping immediate sync"))
       ((not (executable-find "ping"))
        (funcall fn))
       (t
        (make-process
         :name "my-mu4e-ping" :noquery t :buffer nil
         :command '("ping" "-c1" "-W2000" "1.1.1.1")
         :sentinel
         (lambda (proc _msg)
           (when (memq (process-status proc) '(exit signal))
             (if (and (eq (process-status proc) 'exit)
                      (zerop (process-exit-status proc)))
                 (funcall fn)
               (setq my-mu4e--network-timer
                     (run-at-time 3 nil #'my-mu4e--when-network-up
                                  fn (1- attempts)))))))))))

  (defun my-mu4e-sleep-wake-recover ()
    "Kill a hung mbsync and sync once the network is up.
Does nothing unless mu4e's periodic update is active, so a wake
never restarts mu4e after you quit it.  Safe to call by hand."
    (interactive)
    ;; `mu4e--stop' cancels and clears the timer; a dead mu server
    ;; leaves it alone, and the sync below restarts the server.
    (when (timerp mu4e--update-timer)
      (mu4e-message "Wake detected — syncing once the network is up")
      (mu4e-kill-update-mail)
      (my-mu4e--when-network-up
       (lambda ()
         (condition-case err
             (mu4e-update-mail-and-index t)
           (error (message "mu4e wake recovery: sync failed: %s" err)))))))

  (defun my-mu4e--on-sleep-event (event)
    "Handle a `system-sleep' EVENT for mu4e.  Never signals."
    (condition-case err
        (pcase (sleep-event-state event)
          ('pre-sleep
           ;; Let Emacs see mbsync exit instead of the OS killing it.
           (mu4e-kill-update-mail))
          ('post-wake
           (when (timerp my-mu4e--wake-timer)
             (cancel-timer my-mu4e--wake-timer))
           (setq my-mu4e--wake-timer
                 (run-at-time 3 nil #'my-mu4e-sleep-wake-recover))))
      (error (message "mu4e sleep hook error: %s" err))))

  (add-hook 'system-sleep-event-functions #'my-mu4e--on-sleep-event)

  ;;** Attachments
  ;; Set default attachment dir (create if missing).  mu4e's built-in
  ;; `mu4e-view-save-attachments' (bound to `e' in view mode) saves all
  ;; attachments when none are selected.
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads/_Mail"))
  (make-directory mu4e-attachment-dir t)
  ;; File-type icons on attachments and MIME parts in the message view.
  (setq mu4e-file-name-to-icon-function #'nerd-icons-icon-for-file)

  ;;** searching
  (defun my-mu4e-search-for-sender (&optional msg)
    "Find mails sent from the sender of MSG."
    (mu4e-search (concat "from:"
                         (plist-get (car (mu4e-message-field msg :from)) :email))))

  (defun my-mu4e-capture-mail-todo (_msg)
    "Capture the message at point as a TODO with an org-link to it."
    (org-capture nil "M"))

  ;;* mu4e actions (headers & view)
  ;; Shortcut key is the first character of the action name.
  ;;   a → add contact          e → export pdf
  ;;   r → retag message        R → Remove all tags
  ;;   t → todo from email      v → view in browser
  ;;   y → yank path            z → search for sender
  (dolist (action '(("add contact"       . mu4e-action-add-org-contact)
                    ("export pdf"         . my-mu4e-export-message-to-pdf)
                    ("retag message"      . mu4e-action-retag-message)
                    ("Remove all tags"    . my-mu4e-remove-all-tags)
                    ("todo from email"   . my-mu4e-capture-mail-todo)
                    ("view in browser"   . mu4e-action-view-in-browser)
                    ("yank path"         . mu4e-action-copy-message-file-path)
                    ("zsearch for sender" . my-mu4e-search-for-sender)))
    (setf (alist-get (car action) mu4e-headers-actions nil nil #'equal) (cdr action))
    (setf (alist-get (car action) mu4e-view-actions nil nil #'equal) (cdr action)))
  ;; Remove xwidget duplicate — "view in browser" covers it
  (setf (alist-get "xview in xwidget" mu4e-view-actions nil 'remove #'equal) nil)

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

  (setq mu4e-use-fancy-chars t)

  ;; Show only matching messages, not the rest of their threads.
  ;; https://groups.google.com/g/mu-discuss/c/yWvilXfLunE
  (setq mu4e-search-include-related nil)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

  ;;** Useful functions and actions
  (defun my-mu4e-remove-all-tags (msg)
    "Remove all tags from MSG."
    (if-let* ((tags (mu4e-message-field msg :tags)))
        (mu4e-action-retag-message
         msg (mapconcat (lambda (tag) (concat "-" tag)) tags ","))
      (mu4e-message "No tags to remove")))

  ;; tag or untag one or many emails in header view. Can use '*' to act on multiple.
  ;; Use setf/alist-get (not add-to-list) so config reloads don't stack duplicate
  ;; entries.  The two marks must use distinct keys (tag/untag) -- assq lookup in
  ;; mark execution returns the first match, so a shared key shadows the other.
  ;; :ask-target runs once per batch, so marking many messages asks once.
  (setf (alist-get 'tag mu4e-marks)
        '(:char       "g"
          :prompt     "gtag"
          :ask-target (lambda ()
                        (string-join (completing-read-multiple
                                      "Tags (tag, +add, -remove): "
                                      mu4e-action-tags-completion-list)
                                     ","))
          :action     (lambda (_docid msg target)
                        (mu4e-action-retag-message msg target))))

  (setf (alist-get 'untag mu4e-marks)
        '(:char       "G"
          :prompt     "Guntag"
          :action     (lambda (_docid msg _target)
                        (my-mu4e-remove-all-tags msg))))

  ;; org-contacts file for "add contact" action
  ;; NOTE: org-contacts can aggressively override mu4e's native contact
  ;; completion. If that happens, set `org-contacts-enable-completion' to
  ;; nil in the org-contacts use-package (in my-setup-notes.el).
  (setq mu4e-org-contacts-file my-org-contacts-file)

;;;; Composing Email
  ;; Give compose the full frame.  `mu4e-compose-switch' is nil (the
  ;; default: current window), so compose already replaces whatever buffer was
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
              (olivetti-set-width 0.8)))

  ;; Use mu4e system-wide
  (setq mail-user-agent 'mu4e-user-agent)

  (setopt read-mail-command 'mu4e)

  ;; Inline forwarding of multipart/alternative messages (e.g. from
  ;; Outlook) leaves the body empty between the forward markers —
  ;; the content lands in a sibling MIME part that Outlook ignores.
  ;; Forwarding as message/rfc822 attachment avoids this.
  (setq message-forward-as-mime t)

  ;; Don't keep message compose buffers around after sending:
  (setq message-kill-buffer-on-exit t)

  ;; where reply should be positioned
  (setopt message-cite-reply-position 'above)

  ;; TODO: remove mime part buttons. Appears impossible for now
  ;; https://github.com/djcb/mu/issues/2639

  ;; Drop junk from address completion: addresses in
  ;; `my-mu4e-suppressed-addresses' (private.el) plus no-reply and
  ;; similar patterns.  Replaces mu4e's default filter, which only
  ;; dropped "reply".
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Contact-functions.html
  (let ((junk (regexp-opt (append my-mu4e-suppressed-addresses
                                  '("reply" "@finkelsteinlab" "@fortelabs"
                                    "via Canvas")))))
    (setq mu4e-contact-process-function
          (lambda (addr) (unless (string-match-p junk addr) addr))))

  ;;tweak the composition window to include CC and BCC
  (defun my-mu4e-add-cc-bcc ()
    "Add empty Cc: and Bcc: headers."
    (interactive)
    ;; One header per argument: `message-add-header' skips a whole
    ;; string when its first header (Cc on wide replies) exists.
    (save-excursion (message-add-header "Cc: \n" "Bcc: \n")))

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
               (mu4e-trash-folder   . ,(plist-get account :trash))))))

  ;; Non-PII shared/lab accounts kept in this tracked file; personal
  ;; accounts live in `my-mu4e-context-accounts' (private.el).  The
  ;; hisserlab app password is in the macOS Keychain, not here.
  ;; Mail is always sent as the Lab address (`my-swap-email-from-field'),
  ;; so drafts and sent copies go to Lab.  All Mail isn't synced:
  ;; refiling removes the message from INBOX on Gmail (= archive).
  (defvar my-mu4e-extra-context-accounts
    '((:name "Hisser"
       :maildir-prefix "/Hisser"
       :email "hisserlab@gmail.com"
       :full-name "Hisser Lab"
       :drafts "/Lab/Drafts"
       :sent "/Lab/[Gmail]/Sent Mail"
       :refile "/Hisser/[Gmail]/All Mail"
       :trash "/Hisser/[Gmail]/Trash"))
    "Shared account definitions (no PII) merged into `mu4e-contexts'.")

  (setq mu4e-contexts
        (mapcar #'my--build-mu4e-context
                (append my-mu4e-context-accounts
                        my-mu4e-extra-context-accounts)))

  ;; Use the first context when none matches; don't ask
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

;;;; Header flag icons
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
    "Reply to the message at point, asking whether to reply to all.
Only asks when the message has more than one To/Cc recipient and
was not sent by me."
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

  ;;** Export message to PDF (Gmail-print style, via lualatex)
  (defvar my-mu4e-pdf-font "Helvetica Neue"
    "Main font for message PDF export.  Must be visible to fontspec.")

  (defvar my-mu4e-pdf-directory nil
    "Directory for exported message PDFs.
Nil means use `mu4e-attachment-dir'.")

  (defun my-mu4e-pdf--escape (str)
    "Escape LaTeX special characters in STR.
Also drop CR and the control characters LuaTeX treats as invalid,
which would abort the export under -halt-on-error."
    (replace-regexp-in-string
     "[\\{}$&#^_%~]"
     (lambda (m)
       (pcase m
         ("\\" "\\textbackslash{}")
         ("^"  "\\textasciicircum{}")
         ("~"  "\\textasciitilde{}")
         (_    (concat "\\" m))))
     (replace-regexp-in-string
      (rx (any "\r" (0 . 8) ?\v (#x0e . #x1f) #x7f)) "" str)
     t t))

  (defun my-mu4e-pdf--strip-header-block (text)
    "Return TEXT without its leading header block.
The block runs through the first blank line; if there is no blank
line, TEXT is all headers and the result is empty."
    (if (string-match "\n[ \t]*\n" text)
        (string-trim (substring text (match-end 0)))
      ""))

  (defun my-mu4e-pdf--body (msg)
    "Return the plain-text body of MSG.
Tries `mu view' on the message file first; for HTML-only messages
that yields nothing, so fall back to the rendered text of the
current `mu4e-view-mode' buffer."
    (let* ((path (mu4e-message-field msg :path))
           (raw (and path (file-exists-p path)
                     (my-mu4e--extract-body path)))
           (body (and raw (my-mu4e-pdf--strip-header-block raw))))
      (cond
       ((and body (not (string-blank-p body))) body)
       ((derived-mode-p 'mu4e-view-mode)
        (my-mu4e-pdf--strip-header-block
         (buffer-substring-no-properties (point-min) (point-max))))
       (t (user-error "No text body found; open the message first and retry")))))

  (defun my-mu4e-pdf--format-body (text)
    "Convert plain-text TEXT to LaTeX, preserving line and paragraph breaks."
    (let ((paras (split-string (my-mu4e-pdf--escape text)
                               "\n[ \t]*\n+" t "[ \t\n]+")))
      (if (null paras)
          "(no message body)"
        (mapconcat
         (lambda (p)
           (mapconcat #'string-trim-right (split-string p "\n") "\\newline\n"))
         paras "\n\n"))))

  (defun my-mu4e-pdf--slug (subject)
    "Sanitize SUBJECT for use as a file name component."
    (let ((s (string-trim
              (replace-regexp-in-string
               "[ \t]+" " "
               (replace-regexp-in-string "[/:*?\"<>|\\\\]+" " " subject)))))
      (truncate-string-to-width (if (string-empty-p s) "message" s) 60)))

  (defun my-mu4e-pdf--tex (msg body)
    "Return LaTeX source for MSG with LaTeX-formatted BODY."
    (let* ((subject (my-mu4e-pdf--escape
                     (or (mu4e-message-field msg :subject) "(no subject)")))
           (from (car (mu4e-message-field msg :from)))
           (from-name (my-mu4e-pdf--escape
                       (or (plist-get from :name) (plist-get from :email) "Unknown")))
           (from-email (my-mu4e-pdf--escape (or (plist-get from :email) "")))
           (to (my-mu4e-pdf--escape
                (mapconcat #'mu4e-contact-full (mu4e-message-field msg :to) ", ")))
           (cc (my-mu4e-pdf--escape
                (mapconcat #'mu4e-contact-full (mu4e-message-field msg :cc) ", ")))
           (date (format-time-string "%a, %b %-d, %Y at %-I:%M %p"
                                     (mu4e-message-field msg :date)))
           (account (my-mu4e-pdf--escape
                     (if (and (bound-and-true-p my-email-primary-name)
                              (bound-and-true-p my-email-primary-address))
                         (format "%s <%s>" my-email-primary-name
                                 my-email-primary-address)
                       (or user-mail-address "")))))
      (concat
       "\\documentclass[11pt]{article}\n"
       "\\usepackage[margin=2.2cm]{geometry}\n"
       "\\usepackage{fontspec}\n"
       "\\setmainfont{" my-mu4e-pdf-font "}\n"
       "\\usepackage[parfill]{parskip}\n"
       "\\usepackage{xcolor}\n"
       "\\definecolor{metagray}{HTML}{5F6368}\n"
       "\\definecolor{rulegray}{HTML}{DADCE0}\n"
       "\\usepackage{fancyhdr}\n"
       "\\pagestyle{fancy}\n"
       "\\fancyhf{}\n"
       "\\renewcommand{\\headrulewidth}{0pt}\n"
       "\\fancyfoot[C]{\\footnotesize\\color{metagray}\\thepage}\n"
       "\\setlength{\\emergencystretch}{3em}\n"
       "\\sloppy\n"
       "\\begin{document}\n"
       "{\\footnotesize\\color{metagray}" account "}\\par\n"
       "\\vspace{1mm}\n"
       "{\\color{rulegray}\\hrule height 0.8pt}\n"
       "\\vspace{6mm}\n"
       "{\\LARGE\\bfseries " subject "\\par}\n"
       "\\vspace{4mm}\n"
       "{\\color{rulegray}\\hrule height 0.4pt}\n"
       "\\vspace{6mm}\n"
       "\\noindent\\textbf{" from-name "} {\\color{metagray}<" from-email ">}"
       "\\hfill {\\color{metagray}\\footnotesize " date "}\\par\n"
       "\\vspace{0.5mm}\n"
       (unless (string-empty-p to)
         (concat "{\\color{metagray}\\footnotesize To: " to "\\par}\n"))
       (unless (string-empty-p cc)
         (concat "{\\color{metagray}\\footnotesize Cc: " cc "\\par}\n"))
       "\\vspace{6mm}\n"
       "\\begin{flushleft}\n"
       body
       "\n\\end{flushleft}\n"
       "\\end{document}\n")))

  (defun my-mu4e-export-message-to-pdf (&optional msg)
    "Export MSG (default: message at point) to a formatted PDF via lualatex.
The layout mimics Gmail's print view: account line, subject rule,
sender/date header, then the plain-text body.  The PDF lands in
`my-mu4e-pdf-directory' (default `mu4e-attachment-dir') and opens
when compilation finishes.  Compilation runs asynchronously."
    (interactive)
    (unless (executable-find "lualatex")
      (user-error "Cannot find lualatex in PATH"))
    (let* ((msg (or msg (mu4e-message-at-point 'noerror)
                    (user-error "No message at point")))
           (body (my-mu4e-pdf--format-body (my-mu4e-pdf--body msg)))
           (dir (file-name-as-directory
                 (or my-mu4e-pdf-directory mu4e-attachment-dir)))
           (target (expand-file-name
                    (format "%s %s.pdf"
                            (format-time-string
                             "%Y-%m-%d" (mu4e-message-field msg :date))
                            (my-mu4e-pdf--slug
                             (or (mu4e-message-field msg :subject) "")))
                    dir))
           (tmpdir (make-temp-file "mu4e-pdf-" t))
           (texfile (expand-file-name "message.tex" tmpdir))
           (buf (generate-new-buffer " *mu4e-pdf-lualatex*")))
      (make-directory dir t)
      (with-temp-file texfile (insert (my-mu4e-pdf--tex msg body)))
      (message "Exporting message to PDF…")
      (let* ((default-directory tmpdir)
             (proc (start-process "mu4e-pdf-lualatex" buf "lualatex"
                                  "-interaction=nonstopmode" "-halt-on-error"
                                  "message.tex")))
        (set-process-sentinel
         proc
         (lambda (process _signal)
           (when (memq (process-status process) '(exit signal))
             (let ((pdf (expand-file-name "message.pdf" tmpdir)))
               (if (and (zerop (process-exit-status process))
                        (file-exists-p pdf))
                   (progn
                     (copy-file pdf target t)
                     (delete-directory tmpdir t)
                     (when (buffer-live-p buf) (kill-buffer buf))
                     (call-process "open" nil 0 nil target)
                     (message "Exported: %s" target))
                 (message "lualatex failed (exit %s) — log in %s"
                          (process-exit-status process) tmpdir)))))))))

  (defun my-email-to-kill-ring ()
    "Pick an address from mu4e contacts and copy it to the kill ring.
Free input is accepted when nothing matches."
    (interactive)
    (let* ((contact (completing-read "Email address: " mu4e--contacts-set))
           (email (string-trim contact ".*<" ">*")))
      (message "Copied %s to the clipboard" email)
      (kill-new email)))

  ;;** Menu (M-o, leader M): mu4e's own transient, opening mail in the
  ;; "Email" workspace, plus a few extras.
  (transient-replace-suffix 'mu4e-transient-menu "M"
    '("M" "Main screen" my-open-email-in-workspace))
  (transient-replace-suffix 'mu4e-transient-menu "C"
    '("C" "New message" (lambda ()
                          (interactive)
                          (my-open-email-in-workspace)
                          (mu4e-compose-new))))
  (transient-replace-suffix 'mu4e-transient-menu "R"
    '("R" "Reply" my-mu4e-compose-reply-ask-wide :if mu4e-message-p))
  (unless (ignore-errors (transient-get-suffix 'mu4e-transient-menu "e"))
    (transient-append-suffix 'mu4e-transient-menu '(0 -1)
      ["Extras"
       ("e" "Email to kill ring" my-email-to-kill-ring)
       ("H" "Send org heading" org-heading-send-dwim)]))
  ) ;; use-package mu4e
;;** End Mu4e

;; Use Dired to attach files via C-c <RET> C-a (gnus-dired feature).
;; Top-level add-hook so first dired doesn't pull in the whole mail stack.
;; https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html
(add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)
;; Attaching with no draft open starts a new message; make it mu4e's
;; (the default `gnus-user-agent' starts Gnus).
(setq gnus-dired-mail-mode 'mu4e-user-agent)

;;* mu4e-goodies (Claude Code chat about a message)
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
  :demand t
  :bind (:map org-msg-edit-mode-map
              ("s-<return>" . org-msg-goto-body)
              ("M-o" . my-transient-email-compose))
  :config
  ;; Global mode, so it covers forwards too: those come out as
  ;; text+html with the original attached as message/rfc822.
  (org-msg-mode)

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
     ])
  ;; Inject quick-insert contacts from private.el (chained after "eS")
  (when (bound-and-true-p my-email-quick-contacts)
    (let ((prev-key "eS"))
      (dolist (entry my-email-quick-contacts)
        (let ((key   (nth 0 entry))
              (label (nth 1 entry))
              (email (nth 2 entry)))
          (transient-append-suffix 'my-transient-email-compose prev-key
            `(,key ,label (lambda () (interactive) (insert ,email))))
          (setq prev-key key)))))

  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	    org-msg-startup "hidestars indent inlineimages"
	    org-msg-default-alternatives '((new		        . (text html))
				                       (reply-to-html	. (text html))
				                       (reply-to-text	. (text html))))
  ;; org-msg checks for attachments via a regex search for keywords.
  ;; `regexp-unmatchable' never matches, disabling that nag.
  (setq org-msg-attached-file-reference regexp-unmatchable)

  (defun cpm/org-msg-hooks ()
    "Hooks for org-msg"
    ;; org-msg already put point in the body, past its property
    ;; drawer; mu4e's later "reasonable place" jump would move it back
    ;; onto the #+OPTIONS line.
    (setq-local mu4e-compose-jump-to-a-reasonable-place nil)
    (auto-fill-mode -1)
    (hl-line-mode -1)  ;; disable highlight line when composing emails
    (diff-hl-mode -1) ;; disable diff gutter
    ;; Shrink org-meta-line (#+begin_src, #+PROPERTIES, etc.) in compose buffers only
    (face-remap-add-relative 'org-meta-line :height 0.5 :foreground "gray60"))
  (add-hook 'org-msg-edit-mode-hook #'cpm/org-msg-hooks)

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

  (defun my-mu4e-reset-attachment-icons (&rest _)
    "Redraw attachment icons after org-msg attaches or deletes a file."
    (my-mu4e-remove-attachment-icons)
    (my-mu4e-add-attachment-icons))

  ;; Dired attaching goes through `org-msg-attach-attach' too.
  (advice-add 'org-msg-attach-attach :after #'my-mu4e-reset-attachment-icons)
  (advice-add 'org-msg-attach-delete :after #'my-mu4e-reset-attachment-icons)
  ) ;; org-msg

;;* Org Heading to Email/Slack (org-heading-send)
;; Convert org headings to mu4e/org-msg emails or Slack messages.
;; Single merged package; mail deps (org-msg/mu4e) are lazy-required
;; inside the mail backend, so loading this never pulls the mail stack.
(use-package org-heading-send
  :load-path "~/projects/elisp/org-heading-send"
  :commands (org-heading-send-dwim
             org-heading-send-now
             org-heading-mail-send
             org-heading-mail-check-scheduled
             org-heading-slack-send)
  :custom
  (org-heading-mail-scheduled-file "~/Work/org/drafts.org"))

;;* Colors for header view
(use-package mu4e-column-faces
  :after mu4e
  :custom-face
  (mu4e-column-faces-date ((t (:foreground unspecified :background unspecified :inherit lambda-green))))
  (gnus-header-name ((t (:foreground unspecified :background unspecified :inherit lambda-green))))
  (mu4e-column-faces-to-from ((t (:foreground unspecified :background unspecified :inherit lambda-focus))))
  :config
  (mu4e-column-faces-mode))

;;* gcal-add (calendar events from text, context-aware; M-x only)
(use-package gcal-add
  :load-path "~/projects/elisp/gcal-add"
  :commands (gcal-add))

;;* provide my-setup-mail
(provide 'my-setup-mail)
;;* End Setup Email
