;;; my-setup-mail.el -*- lexical-binding: t -*-

;; I use mbsync and mu4e
;;; Mu4e
;; Note: mu4e 1.12.* uses gnus for how it processes messages.
;;* mu4e
(use-package mu4e
  :ensure nil
  :after (org org-contacts)
  :load-path "/opt/homebrew/Cellar/mu/1.12.2/share/emacs/site-lisp/mu/mu4e"
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
  ;; Set default attachment dir
  (setq mu4e-attachment-dir (concat (getenv "HOME") "/Downloads/_Mail"))
  ;; TODO: Fix the function below so all attachements work
  ;; (bind-key "e" #'mu4e-views-mu4e-save-all-atachments mu4e-headers-mode-map)
  
  (defun my-mu4e-action-save-import-ics-file (msg)
    ;;   "Save the text/calendar file(s)  of a message in
    ;;   `mu4e-attachment-dir'. Open with calendar app."
    (let* ((parts (mu4e-view-mime-parts))
           (cal-parts (seq-filter
                       (lambda (el) (equal (plist-get el :mime-type) "text/calendar"))
                       parts))
           (handle (plist-get (car cal-parts) :handle))
           (file (expand-file-name (concat (format-time-string "%Y%m%d_%H-%M-%S_cal") ".ics") mu4e-attachment-dir)))
      (when handle
        ;; save ics file with date-time-stamp in filename in mu4e-attachment-dir
        (mm-save-part-to-file handle file)
        ;; assuming I'm on a Mac (see xah-open-in-external-app for a better implementation)
        ;; open this ics file with open app
        (shell-command (concat "open " (shell-quote-argument file))))))

  (add-to-list 'mu4e-view-actions
               '("Cal import" . my-mu4e-action-save-import-ics-file) t)

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
                       (path (mu4e--uniqify-file-name
                              (mu4e-join-paths
                               (or custom-dir (plist-get part :target-dir))
                               (plist-get part :filename)))))
                  (mm-save-part-to-file (plist-get part :handle) path)))
              candidates)) )

  ;; (add-to-list 'mu4e-view-actions
  ;;              '("export file(s)" . mu4e-view-save-attachments) t)
  (add-to-list 'mu4e-view-actions
               '("export all" . my-mu4e-view-save-all-attachments) t)



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
  ;; View in browser
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser) t)

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

  (add-to-list 'mu4e-view-actions
	           '("Ttodo with LLM" . my-gptel-todo-from-mu4e-message) t)

  (add-to-list 'mu4e-headers-actions
	           '("Ttodo with LLM" . my-gptel-todo-from-mu4e-message) t)


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

  ;; define 'x' as the shortcut
  (add-to-list 'mu4e-view-actions
               '("Xsearch for sender" . my-mu4e-search-for-sender) t)


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
  (setq mu4e-user-mail-address-list '("ilya@finkelsteinlab.org"
                                      "ifinkelstein@cm.utexas.edu"))

  ;; Compose in new buffer (can make 'window for new window)
  (setq mu4e-compose-switch nil)

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
     ((string-match-p "claus.o.wilke@gmail.com" addr) nil) ;; Claus's gmail, which he doesn't use apparently
     ((string-match-p "ellingtontx@gmail.com" addr) nil) ;; random email from Andy Ellington
     ((string-match-p "no-reply" addr) nil)
     ((string-match-p "noreply" addr) nil)
     ((string-match-p "@finkelteinlab" addr) nil)
     ((string-match-p "@fortelabs" addr) nil)
     ((string-match-p "noreply@box.com" addr) nil) ;; random Box emails
     ((string-match-p "elvirasmail2@gmail.com" addr) nil)
     ((string-match-p "ccenik@stanford.edu" addr) nil)
     ((string-match-p "bxhem13@gmail.com" addr) nil) ;; Blerta' alt email do not use
     ((string-match-p "cmazzalin@medicina.ulisboa.pt" addr) nil)
     ((string-match-p "via Canvas" addr) nil)
     ((string-match-p "dleahy@jhmi.edu" addr) nil) ;; Dan Leahy's old email
     ((string-match-p "unwanted.user@somedomain.com)" addr) nil)
     ((string-match-p "dml822@eid.utexas.edu)" addr) nil) ;; Desiree Ledet alt email do not use
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
    "Swap from field in emails to ilya@finkelsteinlab.org to redirect e-mails to that account"
    (interactive)
    (save-excursion
      (message-remove-header "From" nil 'FIRST) ;;remove only first instance of header to deal with replies
      (goto-char (point-min))
      (insert "From: " (message-make-from "Ilya Finkelstein" "ilya@finkelsteinlab.org") "\n")))

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
  (setq sendmail-program "/opt/homebrew/bin/msmtp"
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from"
                                           "-C" "/Users/ilya/.config/.msmtprc")
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
          :vars '((user-mail-address   . "ilya@finkelsteinlab.org")
                  (user-full-name      . "Ilya Finkelstein")
                  (mu4e-drafts-folder  . "/Lab/Drafts")
                  (mu4e-sent-folder    . "/Lab/Sent")
                  (mu4e-refile-folder  . "/Lab/Archive")
                  (mu4e-trash-folder   . "/Lab/Trash")
                  (smtpmail-smtp-user  . "ilya@finkelsteinlab.org")))

         (make-mu4e-context
          :name "UT"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/UT" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address   . "ifinkelstein@cm.utexas.edu")
                  (user-full-name      . "Ilya Finkelstein")
                  (mu4e-drafts-folder  . "/UT/Drafts")
                  (mu4e-sent-folder    . "/UT/Sent")
                  (mu4e-refile-folder  . "/UT/Archive")
                  (mu4e-trash-folder   . "/UT/Trash")
                  (smtpmail-smtp-user  . "ifinkelstein@cm.utexas.edu")))

         (make-mu4e-context
          :name "Test"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/gmail-test" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address   . "test@finkelsteinlab.org")
                  (user-full-name      . "Test account")
                  (mu4e-drafts-folder  . "/gmail-test/[Gmail]/Drafts")
                  (mu4e-sent-folder    . "/gmail-test/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/gmail-test/Archive")
                  (mu4e-trash-folder   . "/gmail-test/[Gmail]/Trash")
                  (smtpmail-smtp-user  . "test@finkelsteinlab.org")))))

  ;; Ask for context if none is set
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'pick-first)

  ;;** Quick Actions & helper functions
  ;; Helpful discussion at
  ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-05.org
  (defun my-mu4e-capture-mail-gtd (msg)
    "Capture message as a TODO item"
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "m"))

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions
               '("todo" . my-mu4e-capture-mail-gtd ) t)
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
                          '(from "ABOR@mlsmatrix.com"))
                 :key ?h)
          (:name "HR"
                 :query ,(mu4e-make-query
                          '(from "MBS_HR@austin.utexas.edu"))
                 :key ?H)
          (:name "From Julie"
                 :query ,(mu4e-make-query
                          '(from "Julie Glasser"))
                 :key ?j)
          (:name "Running"
                 :query ,(mu4e-make-query
                          '(or (from "info@gilbertsgazelles.com") ("Neely Gracey")))
                 :key ?R)
          (:name "Price Watch"
                 :query ,(mu4e-make-query
                          '(or (from "alerts@alerts.craigslist.org") (from "noreply@camelcamelcamel.com")))
                 :key ?P)))
  ;; (setq mu4e-bookmarks
  ;;       '((:name "Unread"
  ;;                :query (lambda ()
  ;;                         (concat "flag:unread AND NOT flag:trashed AND " my-mu4e-inboxes))
  ;;                :key ?u)
  ;;         ;; (:name "UT Staff" :query "from:Desiree OR from:linda OR from:Amanda" :key ?M)

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

;;* mu4e-query for building searches
(use-package mu4e-query
  :vc (:url "https://github.com/mickeynp/mu4e-query")
  :commands (mu4e-make-query)
  ;; :config
  )
;;* Using Org & HTML (Org-Msg)
(use-package org-msg
  :after (mu4e)
  :ensure nil
  :load-path "/Users/ilya/.config/.emacs/org-msg-patch"
  ;; :disabled t
  ;; avoid pesky org ASCII Export buffer
  ;; https://github.com/jeremy-compostella/org-msg/issues/169
  :custom-face
  (org-meta-line ((t (:height 0.65))))

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
     [("rj" "Jeremiah" (lambda ()
                         (interactive)
                         (insert "jdpaulus@utexas.edu")))
      ("rh" "HR" (lambda ()
                   (interactive)
                   (insert "MBS_HR@austin.utexas.edu")))]
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
              (switch-to-buffer "*mu4e-article*")
              (mu4e-view-quit)
              (kill-buffer "*Org ASCII Export*")))

  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	    org-msg-startup "hidestars indent inlineimages"
	    org-msg-greeting-fmt nil
	    org-msg-recipient-names nil
	    org-msg-greeting-name-limit 3
	    org-msg-default-alternatives '((new		        . (text html))
				                       (reply-to-html	. (text html))
				                       (reply-to-text	. (text html)))
        org-msg-convert-citation t
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
    (progn
      (auto-fill-mode -1)
      (hl-line-mode -1)  ;; disable highlight line when composing emails
      (diff-hl-mode -1) ;; disable diff gutter
      ;; (company-mode 1)
      ;; FIXME: Try remove auto-save hook *locally* to avoid multiple saved drafts
      (remove-hook 'auto-save-hook #'cpm/full-auto-save t)))
  (add-hook 'org-msg-edit-mode-hook #'cpm/org-msg-hooks)


  ;; advise mu4e to move cursor to start of message body
  ;; when replying or forwarding messages in org-msg
  (defun my-reply-advice (orig &rest args)
    "Move cursor to the start of the reply in org-msg mode to avoid the properties.
Compatible with mu4e 1.12.xx"
    ;; first run the mu4e function
    (apply orig args)
    ;; then, move the cursor to the body if its a reply (but not new/fwd composition)
    ;; NOTE: first element in "args" holds the type of email that's being composed.
    ;; default behavior for new and forwarded email is to start in the "to" field
    (if (member (car args) '(reply edit))
        (org-msg-goto-body)))
  (advice-add 'mu4e--compose-setup-post :around #'my-reply-advice)


  (defun my-mu4e-add-attachment-icons ()
    "Add icons to the properties drawer to indicate the type of attachments
Requires all-the-icons as a dependency"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let* ((attachments (org-msg-get-prop "attachment"))
             (properties-pos (search-forward ":PROPERTIES:"))
             (properties-start-pos (match-beginning 0)) ;; find the start of the ":PROPERTIES: tag"
             (overlay-icons nil))
        (when (and attachments properties-pos)
          (dolist (element attachments)
            (let ((overlay (make-overlay properties-start-pos 1)))
              (overlay-put overlay 'attachment-icon t)
              (overlay-put overlay 'after-string (all-the-icons-icon-for-file element)))

            ;; (setq overlay-icons (concat (all-the-icons-icon-for-file element)))
            ;; (message overlay-icons)
            )))))

  (defun my-mu4e-remove-attachment-icons ()
    "Remove all icon overlays."
    (save-restriction
      (widen)
      (mapc #'delete-overlay
            (cl-remove-if-not
             (lambda (ov)
               (overlay-get ov 'attachment-icon))
             (overlays-in (point-min) (point-max))
             ))))

  (defun my-mu4e-reset-attachment-icons (orig-fun &rest args)
    "Reset overlays for attachment icons"
    (apply orig-fun args)
    (my-mu4e-remove-attachment-icons)
    (my-mu4e-add-attachment-icons))

  (advice-add 'org-msg-attach-attach :around #'my-mu4e-reset-attachment-icons)
  (advice-add 'org-msg-dired-attach :around #'my-mu4e-reset-attachment-icons)
  (advice-add 'org-msg-attach-delete :around #'my-mu4e-reset-attachment-icons)
  ;; (define-key org-msg-edit-mode-map (kbd "C-c f") 'my-hydra-jump-to-email-fields/body)
  ;; (define-key org-msg-edit-mode-map (kbd "C-c w") 'my-hydra-copy-email-fields-to-kill-RING/BODY)

  ) ;; org-msg

;;* Helper functions
(defvar my--org-heading-email-data nil
  "Temporary storage for email data from org heading.")

(defun my--org-heading-to-email-populate ()
  "Populate email fields after composition buffer is ready.
Called from `org-msg-edit-mode-hook'."
  (when my--org-heading-email-data
    (message "[org-heading-to-email] Populating fields in composition buffer...")
    (let-alist my--org-heading-email-data
      ;; Populate To field
      (message "[org-heading-to-email] Populating To: %s" .to)
      (message-goto-to)
      (insert .to)
      ;; Populate Cc if present
      (when .cc
        (message "[org-heading-to-email] Populating Cc: %s" .cc)
        (message-goto-cc)
        (insert .cc))
      ;; Populate Bcc if present
      (when .bcc
        (message "[org-heading-to-email] Populating Bcc: %s" .bcc)
        (message-goto-bcc)
        (insert .bcc))
      ;; Populate Subject if present
      (when .subject
        (message "[org-heading-to-email] Populating Subject: %s" .subject)
        (message-goto-subject)
        (insert .subject))
      ;; Populate body if present
      (when (and .body (not (string-empty-p .body)))
        (message "[org-heading-to-email] Populating body...")
        (org-msg-goto-body)
        (insert .body))
      ;; Position cursor at body and save to drafts. Deferred because
      ;; org-msg-goto-body fails when called directly from org-msg-edit-mode-hook
      ;; due to the buffer not being fully initialized yet. Auto-save also
      ;; prevents mu4e from prompting to create a draft record during send.
      (run-at-time 0 nil (lambda ()
                          (org-msg-goto-body)
                          (save-buffer)))
      (message "[org-heading-to-email] Done!"))
    ;; Clear the data and remove hook
    (setq my--org-heading-email-data nil)
    (remove-hook 'org-msg-edit-mode-hook #'my--org-heading-to-email-populate)))

(defun my--org-heading-to-email-parse-subheading-format ()
  "Parse email data from subheading format.
Returns alist with to, cc, bcc, subject, body or nil if not in subheading format.

Expected format:
* Heading title (ignored)
** To
recipient@example.com, another@example.com
** Cc
cc@example.com
** Subject
Email subject
** Body
Body text starts here..."
  (save-excursion
    (org-back-to-heading t)
    (let ((parent-level (org-current-level))
          (bound (save-excursion (org-end-of-subtree t) (point)))
          to-addrs cc-addrs bcc-addrs subject body
          found-subheading)
      ;; Look for subheadings
      (while (re-search-forward org-heading-regexp bound t)
        (let* ((heading-text (org-get-heading t t t t))
               (heading-level (org-current-level)))
          (when (= heading-level (1+ parent-level))
            (setq found-subheading t)
            (let* ((element (org-element-at-point))
                   (content-begin (org-element-property :contents-begin element))
                   (content-end (org-element-property :contents-end element))
                   (content (when (and content-begin content-end)
                              (string-trim
                               (buffer-substring-no-properties content-begin content-end)))))
              (pcase (downcase heading-text)
                ("to" (setq to-addrs content))
                ("cc" (setq cc-addrs content))
                ("bcc" (setq bcc-addrs content))
                ("subject" (setq subject content))
                ("body" (setq body content)))))))
      (when found-subheading
        `((to . ,to-addrs)
          (cc . ,cc-addrs)
          (bcc . ,bcc-addrs)
          (subject . ,subject)
          (body . ,body))))))

(defun my--org-heading-to-email-parse-inline-format ()
  "Parse email data from inline header format.
Returns alist with to, cc, bcc, subject, body.

Expected format:
* Heading title (ignored)
<optional timestamp>

To: recipient@example.com, another@example.com
Cc: cc@example.com (optional)
Subject: Email subject (optional)

Body text starts here..."
  (save-excursion
    (org-back-to-heading t)
    (let* ((element (org-element-at-point))
           (content-begin (org-element-property :contents-begin element))
           (content-end (org-element-property :contents-end element))
           (content (when (and content-begin content-end)
                      (buffer-substring-no-properties content-begin content-end)))
           to-addrs cc-addrs bcc-addrs subject body)
      (when content
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          ;; Skip property drawer if present
          (when (looking-at "^:PROPERTIES:")
            (when (re-search-forward "^:END:" nil t)
              (forward-line 1)))
          ;; Skip timestamp line if present
          (when (looking-at "^<[^>]+>")
            (forward-line 1))
          ;; Skip blank lines
          (while (and (not (eobp)) (looking-at "^\\s-*$"))
            (forward-line 1))
          ;; Parse header fields
          (while (looking-at "^\\(To\\|Cc\\|Bcc\\|Subject\\):\\s-*\\(.*\\)$")
            (let ((field (match-string 1))
                  (value (string-trim (match-string 2))))
              (pcase field
                ("To" (push value to-addrs))
                ("Cc" (push value cc-addrs))
                ("Bcc" (push value bcc-addrs))
                ("Subject" (setq subject value))))
            (forward-line 1))
          ;; Skip blank lines before body
          (while (and (not (eobp)) (looking-at "^\\s-*$"))
            (forward-line 1))
          ;; Rest is body
          (setq body (string-trim (buffer-substring-no-properties (point) (point-max))))))
      ;; Combine multiple addresses
      (setq to-addrs (and to-addrs (string-join (nreverse to-addrs) ", ")))
      (setq cc-addrs (and cc-addrs (string-join (nreverse cc-addrs) ", ")))
      (setq bcc-addrs (and bcc-addrs (string-join (nreverse bcc-addrs) ", ")))
      `((to . ,to-addrs)
        (cc . ,cc-addrs)
        (bcc . ,bcc-addrs)
        (subject . ,subject)
        (body . ,body)))))

(defun my-org-heading-to-email ()
  "Convert org heading at point to a new mu4e email.

Supports two formats:

Format 1 - Inline headers:
* Heading title (ignored)
<optional timestamp>

To: recipient@example.com, another@example.com
Cc: cc@example.com (optional, multiple allowed)
Bcc: bcc@example.com (optional, multiple allowed)
Subject: Email subject (optional)

Body text starts here...

Format 2 - Subheadings:
* Heading title (ignored)
** To
recipient@example.com, another@example.com
** Cc
cc@example.com
** Subject
Email subject
** Body
Body text starts here..."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (save-excursion
    (org-back-to-heading t)
    (message "[org-heading-to-email] Found heading at point %d" (point))
    ;; Try subheading format first, fall back to inline format
    (let ((email-data (or (my--org-heading-to-email-parse-subheading-format)
                          (my--org-heading-to-email-parse-inline-format))))
      (let-alist email-data
        (message "[org-heading-to-email] Parsed - To: %s, Cc: %s, Bcc: %s, Subject: %s"
                 .to .cc .bcc .subject)
        (message "[org-heading-to-email] Body:\n%s" .body)
        ;; Validate required fields
        (unless .to
          (user-error "Missing To: field"))
        ;; Store data for the hook to use
        (setq my--org-heading-email-data email-data)
        ;; Add hook to populate after buffer is ready
        (add-hook 'org-msg-edit-mode-hook #'my--org-heading-to-email-populate)
        ;; Create the email in a new window - buffer population happens in the hook
        (message "[org-heading-to-email] Creating new mu4e composition buffer...")
        (let ((mu4e-compose-switch 'window))
          (mu4e-compose-new))))))

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
  "Find @llm directive in org-msg buffer and use LLM to draft email.

Place cursor anywhere in the buffer. The function searches for a line
starting with @llm followed by instructions. It sends those instructions
along with the entire email as context to the LLM, then replaces the
@llm line with the generated response.

Example usage in an org-msg reply buffer:
  @llm politely decline, too busy. commend her son."
  (interactive)
  (unless (derived-mode-p 'org-msg-edit-mode)
    (user-error "Not in an org-msg-edit-mode buffer"))
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^@llm\\s-+\\(.+\\)$" nil t)
      (user-error "No @llm directive found"))
    (let* ((instructions (match-string 1))
           (llm-line-beg (match-beginning 0))
           (llm-line-end (match-end 0))
           (email-content (buffer-substring-no-properties (point-min) (point-max))))
      (message "[org-msg-llm] Instructions: %s" instructions)
      (message "[org-msg-llm] Sending to LLM...")
      ;; Delete the @llm line
      (delete-region llm-line-beg llm-line-end)
      ;; Position for insertion
      (goto-char llm-line-beg)
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
                  (goto-char insert-pos)
                  (insert response)))
              (message "[org-msg-llm] Draft inserted:\n%s" response))))))))


(defun my-daily-email-progress ()
  "Plot my daily, weekly, and monthly email progress.
A shell script queries mu every five minutes via the xbar app."
  (interactive "P")
  (let (file "/Users/ilya/Work/01-09 meta/01 productivity/email-inbox-count-log.csv")
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
  :load-path "/opt/homebrew/Cellar/mu/1.12.2/share/emacs/site-lisp/mu/mu4e")

;;* provide my-setup-mail
(provide 'my-setup-mail)
;;* End Setup Email
