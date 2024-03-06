;;; my-setup-mail.el ---Setup for mu4e   -*- lexical-binding: t -*-
;; TODO: Make this look like the other module
;; Assembled from many sources
;; I use mbsync and mu4e

;;; Code

;;;; Mu4e
(use-package mu4e
  :ensure nil
  :load-path "/opt/homebrew/Cellar/mu/1.10.8/share/emacs/site-lisp/mu/mu4e"
  ;; :straight (:type nil :local-repo "/opt/homebrew/Cellar/mu/1.10.8/share/emacs/site-lisp/mu/mu4e" :pre-build ())
  :commands (mu4e mu4e-compose-new mu4e-update-mail-and-index)
  :bind (:map mu4e-view-mode-map
         ("f" . link-hint-open-link))  ;; Firefox tridactyl-like bindings
  :config
  ;; remove icalendar integration so I can save ICS files directly to my Calendar app
  ;; from mu4e manual: https://www.djcbsoftware.nl/code/mu/mu4e/iCalendar.html
  ;; (require 'mu4e-icalendar)
  ;; (mu4e-icalendar-setup)

  ;; Finding the binary (installed w/homebrew)
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq  message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote...") ;; fix bug in some Outlook clients
  (mu4e-toggle-logging) ;;turn on for error logging buffer

  ;;change keybindings to look more like elfeed
  ;; in headers view, "r" for read messages, "u" for unread messages
  ;; over-ridden mu4e keybinds map to old positions
  ;; bind-key* macro is defined in use-package
  (bind-keys :map mu4e-headers-mode-map
    ("r" . mu4e-headers-mark-for-read)
    ("u" . mu4e-headers-mark-for-unread)
    ("!" . mu4e-headers-mark-for-refile)
    ("?" . mu4e-headers-mark-for-unmark))

  (bind-keys :map mu4e-view-mode-map
    ("r" . mu4e-view-mark-for-read)
    ("u" . mu4e-view-mark-for-unread)
    ("!" . mu4e-view-mark-for-refile)
    ("?" . mu4e-view-mark-for-unmark))
;;;; Syncing
  ;; Maildir
  (setq mu4e-maildir "~/Mail")
  ;; Sync imap servers w/mbsync (via isync installed w/homebrew):
  ;; this command redirects STDERR to STDOUT to a log file
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

;;;; Attachments
  ;; Set default dir
  (setq mu4e-attachment-dir (concat (getenv "HOME") "/Downloads/_Mail"))
  ;; Save all attachments to specified dir without asking about each one
  (setq mu4e-save-multiple-attachments-without-asking t)
  (bind-key "e" #'mu4e-views-mu4e-save-all-attachments mu4e-headers-mode-map)
  ;; use Dired to attach files via C-c <RET> C-a
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
  (add-hook 'mu4e-compose-mode-hook
            (defun ijf-mu4e-compose-settings ()
              "My settings for message comoposition"
              (ijf-mu4e-add-cc-bcc)
              (ijf-swap-email-from-field)
              (auto-fill-mode -1)
              (hl-line-mode -1)
              ;; (org-msg-goto-body)
              ))

  (defun ijf-mu4e-view-import-ics-file (&optional arg)
    "Save the text/calendar file(s)  of a message in
    `mu4e-attachment-dir'. Open with calendar app."
    (interactive)
    (when (and (eq major-mode 'mu4e-view-mode)
               (derived-mode-p 'gnus-article-mode))
      (let ((parts (mu4e--view-gather-mime-parts))
            file ;; initialize with nil
            )
        ;; body of let
        (dolist (part parts) ;; cycle through every element in parts alist
          ;; body of dolist
          (when (assoc "text/calendar" (cdr part))
            ;; save ics file with date-time-stamp in filename in mu4e-attachment-dir
            (setq file (expand-file-name (concat (format-time-string "%Y%m%d_%H-%M-%S_cal") ".ics") mu4e-attachment-dir))
            (mm-save-part-to-file (cdr part) file)
            ;; (debug)
            ;; assuming I'm on a Mac (see xah-open-in-external-app for a better implementation)
            ;; open this ics file with open app
            (shell-command (concat "open " (shell-quote-argument file)))
            )
          ))
      ))

  (defun ed/mu4e-view-save-all-attachments (&optional arg)
    "Save all attachments of a given message.
    If ARG is nil, all attachments will be saved in
    `mu4e-attachment-dir'. When non-nil, user will be prompted to
    choose a specific directory where to save all the files.
    If the directory doesn't exist, user will be prompted to create it and parents."
    (interactive "P")
    (when (and (eq major-mode 'mu4e-view-mode)
               (derived-mode-p 'gnus-article-mode))
      (let ((parts (mu4e--view-gather-mime-parts))
            (handles '())
            (files '())
            (directory (if arg
                           (read-directory-name "Save: ")
                         mu4e-attachment-dir)))
        ;; create directory if one doesn't exist
        (if (not (file-directory-p directory))
            (make-directory directory t))
        (dolist (part parts)
          (let ((fname (or (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                           (cl-loop for item in part
                                    for name = (and (listp item) (assoc-default 'name item))
                                    thereis (and (stringp name) name)))))
            (when fname
              (push `(,fname . ,(cdr part)) handles)
              (push fname files))))
        (if files
            (progn
              (cl-loop for (f . h) in handles
                       when (member f files)
                       do (mm-save-part-to-file
                           h (let ((file (expand-file-name f directory)))
                               (if (file-exists-p file)
                                   (let (newname (count 1))
                                     (while (and
                                             (setq newname
                                                   (format "%s-%s%s"
                                                           (file-name-sans-extension file)
                                                           count
                                                           (file-name-extension file t)))
                                             (file-exists-p newname))
                                       (cl-incf count))
                                     newname)
                                 file)))))
          (mu4e-message "No attached files found")))))

  (defun ijf-mu4e-search-for-sender (&optional msg)
    "Find mails sent from SENDER."
    (mu4e-search (concat "from:" (nth 1 (flatten-list (mu4e-message-field msg :from))))))

  ;; define 'z' as the shortcut
  (add-to-list 'mu4e-view-actions
               '("zsearch for sender" . ijf-mu4e-search-for-sender) t)

  (add-to-list 'mu4e-headers-actions
               '("zsearch for sender" . ijf-mu4e-search-for-sender) t)

  ;; (define-key mu4e-view-mode-map "a" #'ed/mu4e-view-save-all-attachments)
  (add-to-list 'mu4e-headers-actions
               '("export" . ed/mu4e-view-save-all-attachments) t)
  (add-to-list 'mu4e-view-actions
               '("export" . ed/mu4e-view-save-all-attachments) t)

  (add-to-list 'mu4e-headers-actions
               '("Cal" . ijf-mu4e-view-import-ics-file) t)
  (add-to-list 'mu4e-view-actions
               '("Cal" . ijf-mu4e-view-import-ics-file) t)
  (add-to-list 'mu4e-view-actions
               '("quit" . (lambda (msg)
                            (interactive)
                            (message "Do Nothing"))) t)

;;;; Viewing
;;;;; Header View Functions
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

  (defun mu4e-headers-tag (text tag face help query)
    "Make a clickable button with specified FACE displaying TEXT.
    When hovered, HELP is displayed. When clicked, mu4e QUERY is executed."
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map 'mu4e-headers-mode-map)
      (define-key map [mouse-1] `(lambda ()
                                   (interactive) (mu4e-headers-search ,query)))
      (concat
       (propertize text
                   'display tag
                   'face face
                   'mouse-face `(:foreground ,bespoke-salient)
                   'local-map map
                   'help-echo `(lambda (window _object _point)
                                 (let (message-log-max) (message ,help))))
       " ")))

  ;; Buttons
  (defun mu4e-headers-button (text face help query)
    "Make a clickable button with specified FACE displaying TEXT.
    When hovered, HELP is displayed. When clicked, mu4e QUERY is executed."
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map 'mu4e-headers-mode-map)
      (define-key map [mouse-1] `(lambda ()
                                   (interactive) (mu4e-headers-search ,query)))
      (propertize text
                  'face face
                  'mouse-face `(:foreground ,bespoke-background
                                :background ,bespoke-faded)
                  'local-map map
                  'help-echo `(lambda (window _object _point)
                                (let (message-log-max) (message ,help))))))

  (defun mu4e-headers-date-button (date face)
    (concat
     (mu4e-headers-button (format-time-string "%d" date)
                          face
                          (format-time-string "Mails from %d %B %Y" date)
                          (format-time-string "date:%Y%m%d" date))
     (propertize "/" 'face face)
     (mu4e-headers-button (format-time-string "%m" date)
                          face
                          (format-time-string "Mails from %B %Y" date)
                          (format-time-string "date:%Y%m" date))
     (propertize "/" 'face face)
     (mu4e-headers-button (format-time-string "%Y" date)
                          face
                          (format-time-string "Mails from %Y" date)
                          (format-time-string "date:%Y" date))))
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

;;;;; Headers
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
  ;; IF: modified this setting to
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

  ;; Other options for rendering
  ;; NOTE: superseded by xwidget support -- see mu4e-views below
  ;; (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  ;; (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

  ;; other display settings
  (setq mu4e-speedbar-support t)
  (setq mu4e-use-fancy-chars t)

  ;; disable seeing related messages in a thread. can be toggled with "W" in headers view
  (setq mu4e-headers-include-related nil)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)


;;;;; Useful functions and actions

  (defun ijf-mu4e-last-month ()
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
               '("Xsearch for sender" . ijf-mu4e-search-for-sender) t)


  ;; create org link to message
  (require 'mu4e-org)
  (add-to-list 'mu4e-view-actions
               '("org link" . org-store-link) t)
  (add-to-list 'mu4e-headers-actions
               '("org link" . org-store-link) t)

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

;;;;; Composing Email

  ;; Use mu4e system-wide
  (setq mail-user-agent 'mu4e-user-agent)

  (set-variable 'read-mail-command 'mu4e)
  ;; List of your email adresses:
  (setq mu4e-user-mail-address-list '("ilya@finkelsteinlab.org"
                                      "ifinkelstein@cm.utexas.edu"))

  ;; Disable Compose in new frame
  (setq mu4e-compose-in-new-frame nil)

  ;; Don't keep message compose buffers around after sending:
  (setq message-kill-buffer-on-exit t)
  ;;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; Only ask if a context hasn't been previously picked
  (setq mu4e-compose-context-policy 'ask-if-none)

  ;; Possible fix for outlook client reading problems in inline messages
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#How-can-I-avoid-Outlook-display-issues_003f
  (setq  message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote...")

  ;; Check spelling
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

  ;; Filtering function to remove annoying composition auto-completes
  ;; https://emacs.stackexchange.com/questions/47789/how-to-remove-email-address-from-local-database-in-mu4e
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Contact-functions.html
  (defun ijf-mu4e-contact-filter (addr)
    "Remove annoying completions from the email autocomplete. Uses regex to identify offending emails."
    (cond
     ;; remove unwanted auto-completes
     ((string-match-p "no-reply" addr) nil)
     ((string-match-p "@finkelteinlab" addr) nil)
     ((string-match-p "@fortelabs" addr) nil)
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

  (setq mu4e-contact-process-function 'ijf-mu4e-contact-filter)

  ;;tweak the composition window to include CC and BCC
  (defun ijf-mu4e-add-cc-bcc ()
    "Add CC and BCC to all new e-mails"
    (interactive)
    "Add a Cc: & Bcc: header."
    (save-excursion (message-add-header "Cc: \nBcc: \n")))

  (defun ijf-swap-email-from-field ()
    "Swap from field in emails to ilya@finkelsteinlab.org to redirect e-mails to that account"
    (interactive)
    (save-excursion
      (message-remove-header "From" nil 'FIRST) ;;remove only first instance of header to deal with replies
      (goto-char (point-min))
      (insert "From: " (message-make-from "Ilya Finkelstein" "ilya@finkelsteinlab.org") "\n")))

  ;; a few helper functions to navigate & copy message fields quickly
  (defun ijf-copy-email-address-at-point ()
    "If there is one, copy the e-mail address at point to the kill-ring."
    (interactive)
    (when-let ((addr (thing-at-point 'email)))
      (kill-new addr)))

  ;; (defun ijf-msg-goto-to-field ()
  ;;   "Move point to the beginning of the To field."
  ;;   (interactive)
  ;;   (goto-char (point-min))
  ;;   (search-forward "To: " nil t)
  ;;   (goto-char (match-end 0)))

  ;; (defun ijf-msg-goto-cc-field ()
  ;;   "Move point to the beginning of the To field."
  ;;   (interactive)
  ;;   (goto-char (point-min))
  ;;   (search-forward "Cc: " nil t)
  ;;   (goto-char (match-end 0)))

;;;; Sending Mail
;;;;; Send Settings

  ;; Configure the function to use for sending mail
  ;; use msmtp to manage sending mail from different e-mail accounts
  ;; reference: https://tushartyagi.com/blog/configure-mu4e-and-msmtp
  (setq sendmail-program "/opt/homebrew/bin/msmtp"
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from"
                                           "-C" "/Users/ilya/.config/.msmtprc")
        message-send-mail-function 'message-send-mail-with-sendmail)

  ;; IF: I am not sure what the line below is supposed to do. I don't have it in my config
  (setq smtpmail-queue-dir (concat mu4e-maildir "/queued-mail/"))

  ;; add Cc to all messages by default

  ;; add more useful keybindings to org-msg sparse map

  ;; NOTE: Only use this if you have set up a GPG key!
  ;; Automatically sign all outgoing mails
  ;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  ;; Move messages to trash
  ;; See https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
  ;; FIXME: This wont work since I have two email accounts
  ;; (fset 'cpm-ijf-swap-email-from-field-email-move-to-trash "mTrash")
  ;; (define-key mu4e-headers-mode-map (kbd "d") 'cpm--email-move-to-trash)
  ;; (define-key mu4e-view-mode-map (kbd "d") 'cpm--email-move-to-trash)

;;;;; Check Attachments
  ;; See https://github.com/panjie/mu4e-goodies
  (require 'hi-lock)
  (defvar cpm-mail-rule-func
    '((check-attach . cpm-mail-draft-attach-p)
      (check-cc . cpm-mail-draft-cc-p)))

  (defvar cpm-mail-keywords
    '(("[aA]ttachment" . check-attach)
      ("[aA]ttached" . check-attach)
      ("[cC]c'd" . check-cc)
      ("C[cC]'d" . check-cc)
      ("CCd" . check-cc))
    "Keywords to be alerted. An alist like:
    \( (regexp-of-keywords . rules-for-keywords) ... )")

  (defun cpm-mail-draft-attach-p ()
    "Check if current email draft has at least one attachment."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "\<#part .*filename=.*" (point-max) t)))

  (defun cpm-mail-draft-cc-p ()
    "Check if current email draft has cc field."
    (message-fetch-field "Cc"))

  (defun cpm-mail-search-body-subject (keyword &optional start)
    "Search for keyword in the current mail's subject and body. Return
    the pos of the keyword which is a cons cell, nil if not found."
    ;; check for subject
    (save-excursion
      (if (and start (<= start (point-max)))
          (goto-char start)
        (message-goto-subject))
      (if (re-search-forward keyword (point-max) t)
          ;; check if the keyword is found in a cited line
          (let ((current-pos (point)))
            (beginning-of-line)
            (if (or (search-forward message-yank-prefix
                                    (+ (point) (length message-yank-prefix))
                                    t)
                    (search-forward message-yank-cited-prefix
                                    (+ (point) (length message-yank-cited-prefix))
                                    t))
                (cpm-mail-search-body-subject keyword current-pos)
              (cons (match-beginning 0) (match-end 0))))
        nil)))
  ;; IF: disabled attachment reminders for now. Let's see if I miss it
  ;; (add-hook 'message-send-hook
  ;;           (defun cpm-mail-check-keywords ()
  ;;             (interactive "P")
  ;;             (let ((it (car cpm-mail-keywords))
  ;;                   (list (cdr cpm-mail-keywords))
  ;;                   (key-pos)
  ;;                   (msg))
  ;;               (while (and (not key-pos) it)
  ;;                 (unless (and (setq key-pos (cpm-mail-search-body-subject (car it)))
  ;;                              (not (funcall (cdr (assoc (cdr it) cpm-mail-rule-func)))))
  ;;                   (setq key-pos nil)
  ;;                   (setq it (car list)
  ;;                         list (cdr list))))
  ;;               (when key-pos
  ;;                 (goto-char (car key-pos))
  ;;                 (overlay-put (make-overlay (car key-pos) (cdr key-pos)) 'face 'hi-yellow)
  ;;                 (cond
  ;;                  ((eq (cdr it) 'check-attach) (setq msg "You may forget your attachment!"))
  ;;                  ((eq (cdr it) 'check-cc) (setq msg "You may forget your Cc!")))
  ;;                 (setq msg (concat msg " Really send message?"))
  ;;                 (or (y-or-n-p msg)
  ;;                     (keyboard-quit))))))

  (defun cpm-mail-check-keywords ()
    (interactive "P")
    (let ((it (car cpm-mail-keywords))
          (list (cdr cpm-mail-keywords))
          (key-pos)
          (msg))
      (while (and (not key-pos) it)
        (unless (and (setq key-pos (cpm-mail-search-body-subject (car it)))
                     (not (funcall (cdr (assoc (cdr it) cpm-mail-rule-func)))))
          (setq key-pos nil)
          (setq it (car list)
                list (cdr list))))
      (when key-pos
        (goto-char (car key-pos))
        (overlay-put (make-overlay (car key-pos) (cdr key-pos)) 'face 'hi-yellow)
        (cond
         ((eq (cdr it) 'check-attach) (setq msg "You may have forgotten your attachment!"))
         ((eq (cdr it) 'check-cc) (setq msg "You may have forgotten your Cc!")))
        (setq msg (concat msg " Really send message?"))
        (or (y-or-n-p msg)
            (keyboard-quit)))))


;;;; Contexts
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

;;;;; Quick Actions & helper functions

  ;; Helpful discussion at
  ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-05.org
  (defun ijf/capture-mail-gtd (msg)
    "Capture message as a TODO item"
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "m"))

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions
               '("todo" . ijf/capture-mail-gtd ) t)
  (add-to-list 'mu4e-view-actions
               '("todo" . ijf/capture-mail-gtd) t)
  ;; (add-to-list 'mu4e-headers-actions
  ;;              '("respond" . cpm/capture-mail-respond) t)
  ;; (add-to-list 'mu4e-view-actions
  ;;              '("respond" . cpm/capture-mail-respond) t)
  ;; (add-to-list 'mu4e-headers-actions
  ;;              '("Remind" . cpm/capture-mail-remind) t)
  ;; (add-to-list 'mu4e-view-actions
  ;;              '("Remind" . cpm/capture-mail-remind) t)
  ;; hydra to improve rapid navigation between composition fields

  (defhydra ijf-hydra-jump-to-email-fields (:color blue)
    "[t]o [f]rom [c]c [b]cc [s]ubject [m]essage [a]ttach [d]ired"
    ("t" message-goto-to)
    ("f" message-goto-from)
    ("c" message-goto-cc)
    ("b" message-goto-bcc)
    ("s" message-goto-subject)
    ("m" message-goto-body)
    ("a" mml-attach-file)
    ("d" dired-other-window))
  (define-key message-mode-map (kbd "C-c f") 'ijf-hydra-jump-to-email-fields/body)

  ;; copy fields to kill ring
  (defun ijf-copy-email-field-to-kill-ring (FIELD)
    "Copy FIELD from the open org-msg buffer to the kill-ring"
    (interactive "P")
    (kill-new
     (message-fetch-field FIELD)))

  (defhydra ijf-hydra-copy-email-fields-to-kill-ring (:color blue)
    "[t]o [f]rom [c]c [b]cc [s]ubject"
    ("t" (ijf-copy-email-field-to-kill-ring "To"))
    ("f" (ijf-copy-email-field-to-kill-ring "From"))
    ("c" (ijf-copy-email-field-to-kill-ring "Cc"))
    ("b" (ijf-copy-email-field-to-kill-ring "Bcc"))
    ("s" (ijf-copy-email-field-to-kill-ring "Subject")))
  (define-key message-mode-map (kbd "C-c w") 'ijf-hydra-copy-email-fields-to-kill-ring/body)

;;;;; Mail Custom Bookmarks/Searches
  (setq ijf-mu4e-inboxes "(maildir:/UT/Inbox OR maildir:/Lab/Inbox OR maildir:/gmail-test/Inbox)")

  ;;disable threading in search results
  (setq mu4e-headers-show-threads nil)

  ;; Note that :hide-unread is implied when the query is not a string; this for the common case where the query function involves some user input, which would be disruptive in this case.
  (setq mu4e-bookmarks
        '((:name "Unread"
           :query (lambda ()
                    (concat "flag:unread AND NOT flag:trashed AND " ijf-mu4e-inboxes))
           :key ?u)
          (:name "Today"
           :query (lambda ()
                    (concat "NOT flag:trashed AND date:today..now AND " ijf-mu4e-inboxes))
           :key ?t)
          (:name "New This Week"
           :query (lambda ()
                    (concat "flag:unread NOT flag:trashed AND date:1w..now AND " ijf-mu4e-inboxes))
           :key ?w)
          (:name "New This Month"
           :query (lambda ()
                    (concat "flag:unread NOT flag:trashed AND date:1m..now AND " ijf-mu4e-inboxes))
           :key ?m)
          (:name "New Last Month"
           :query (lambda ()
                    (concat "flag:unread NOT flag:trashed AND date:2m..1m AND " ijf-mu4e-inboxes))
           :key ?M)
          (:name "From Julie"
           :query "from:Julie Glasser"
           :key ?j)
          ;; (:name "UT Staff" :query "from:Desiree OR from:linda OR from:Amanda" :key ?M)
          (:name "Price Watch"
           :query "woom or f:noreply@camelcamelcamel.com"
           :key ?P)
          (:name "Search"
           :query "flag:unread AND (from:jobalerts-noreply@linkedin.com OR from:noreply@jobmail.naturecareers.com OR from:noreply@jobrxiv.org OR from:reply@sciencecareers.org OR from:support@academicjobsonline.org OR from:jobseeker@higheredjobs.com)"
           :key ?J)
          ))

  ;; (defun ijf-mu4e-bookmark-num-days-old-query (days-old)
  ;;   (interactive (list (read-number "Show days old messages: " 7)))
  ;;   (let ((start-date (subtract-time (current-time) (days-to-time days-old))))
  ;;     (concat "date:"
  ;;             (format-time-string "%Y%m%d" start-date) " AND " ijf-mu4e-inboxes)))

  ;; (add-to-list 'mu4e-bookmarks
  ;;              `(:name  "Inbox messages in the last 7 days"
  ;;                :query ,(lambda () (call-interactively 'ijf-mu4e-bookmark-num-days-old-query))
  ;;                :key  ?o) t)

;;;; Maildirs
  ;; NOTE: Use maildir-extensions for now
  ;; Eventually this will be incorporated into mu, but right now it doesn't show mail counts for some reason

  ;; ;; the maildirs you use frequently; access them with 'j' ('jump')
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

;;;;; Add SVG tags
  ;; Don't show refile target; use svg instead
  (setq mu4e-headers-show-target nil)

  ;; FIXME: unmarking doesn't remove SVG tags
  (defun mu4e-mark-at-point-advice (mark target)
    (interactive)
    (require 'svg-tag-mode)
    (let* ((msg (mu4e-message-at-point))
           (docid (mu4e-message-field msg :docid))
           (overlay (make-overlay (- (line-end-position) 10)
                                  (- (line-end-position) 0))))
      (save-excursion
        ;; (remove-overlays (line-beginning-position) (line-end-position))
        (delete-overlay (make-overlay (line-beginning-position) (line-end-position)))
        (if (eql mark 'unmark)
            (delete-overlay overlay)
          (cond ((eql mark 'refile)
                 (overlay-put overlay 'display (svg-tag-make "ARCHIVE" 'success 3 0)))
                ((eql mark 'trash)
                 (overlay-put overlay 'display (svg-tag-make "TRASH" 'error 5 0)))
                ((eql mark 'untrash)
                 (overlay-put overlay 'display (svg-tag-make "UNTRASH" 3 0)))
                ((eql mark 'delete)
                 (overlay-put overlay 'display (svg-tag-make "DELETE" 'error 4 0)))
                ((eql mark 'unread)
                 (overlay-put overlay 'display (svg-tag-make "UNREAD" 4 0)))
                ((eql mark 'flag)
                 (overlay-put overlay 'display (svg-tag-make "FLAG" 'warning 6 0)))
                ((eql mark 'unflag)
                 (overlay-put overlay 'display (svg-tag-make "UNFLAG" 4 0)))
                ((eql mark 'move)
                 (overlay-put overlay 'display (svg-tag-make "MOVE" 'success 6 0)))
                ((eql mark 'tag)
                 (overlay-put overlay 'display (svg-tag-make "TAG" 'shadow 7 0))))))))

  (advice-add 'mu4e-mark-at-point :after #'mu4e-mark-at-point-advice)


;;;; Miscellaneous helper functions
  (defun my-email-message-to-kill-ring ()
    "Yank the current message text into the kill ring"
    (interactive)
    (when (mu4e-is-mode-or-derived-p 'gnus-article-mode)
      (save-excursion
        (let ((start (message-goto-body))
              (end (point-max)))
          (kill-ring-save start end)))))

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

  (defun ijf-mu4e-view-overlay-attachment-icons (&optional msg)
    "Add file-specific icon overlay for each attachment type in mu4e-view mode."
    (when (and (eq major-mode 'mu4e-view-mode)
               (derived-mode-p 'gnus-article-mode))
      (goto-char (point-min))
      (let ((parts (mu4e--view-gather-mime-parts))
            (file nil);; initialize with nil
            (overlay-pos (re-search-forward "Attachments?:" nil t nil))
            (overlay nil))
        ;; body of let
        (message "overlapy position: %S" (- overlay-pos 1))
        (when overlay-pos
          (dolist (part parts) ;; cycle through every element in parts alist
            ;; body of dolist
            (let ((fname (or (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                             (cl-loop for item in part
                                      for name = (and (listp item)
                                                      (assoc-default 'name item))
                                      thereis (and (stringp name) name)))))
              (when fname
                ;; (message (file-name-extension fname))
                (setq overlay (make-overlay (- overlay-pos 1) 1))
                (overlay-put overlay 'attachment-icon t)
                (overlay-put overlay 'after-string (all-the-icons-icon-for-file fname)))
              ))))))
  (advice-add 'mu4e-view :after #'ijf-mu4e-view-overlay-attachment-icons)

  ;; Updating
  (bind-key "u" #'mu4e-update-index mu4e-main-mode-map)

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
  (defvar ijf-mu4e-unread-query "flag:unread AND NOT flag:trashed AND (maildir:/UT/Inbox OR maildir:/Lab/Inbox OR maildir:/gmail-test/Inbox)")

  (defun ijf-go-to-unread-mail ()
    (interactive)
    (if (member "Email" (tabspaces--list-tabspaces))
        (progn
          (tab-bar-switch-to-tab "Email")
          (mu4e-headers-search ijf-mu4e-unread-query))
      (progn
        (tabspaces-create-workspace)
        (tab-bar-rename-tab "Email")
        (find-file (concat org-directory "inbox.org"))
        (mu4e)
        (mu4e-headers-search ijf-mu4e-unread-query))))
  ;; Go to inbox
  (defvar cpm-mu4e-inbox-query
    "(maildir:/UT/Inbox OR maildir:/Lab/Inbox OR maildir:/gmail-test/Inbox)")
  (defun cpm/go-to-mail-inbox ()
    (interactive)
    (if (member "Email" (tabspaces--list-tabspaces))
        (progn
          (tab-bar-switch-to-tab "Email")
          (mu4e-headers-search cpm-mu4e-inbox-query))
      (progn
        (tabspaces-create-workspace)
        (tab-bar-rename-tab "Email")
        (find-file (concat org-directory "mail.org"))
        (mu4e)
        (mu4e-headers-search cpm-mu4e-inbox-query))))
  ;; search email inbox
  (defun ijf/search-all-mail ()
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
  )
;;;; End Mu4e

;;;; Using Org & HTML (Org-Msg)
(use-package org-msg
  :after (mu4e)
  ;; avoid pesky org ASCII Export buffer
  ;; https://github.com/jeremy-compostella/org-msg/issues/169
  :preface
  (defun org-msg-no-temp-buffer (orig-fun &rest args)
    "Advice to set `org-export-show-temporary-export-buffer' to `nil'."
    (let ((org-export-show-temporary-export-buffer nil))
      (apply orig-fun args)))

  :hook ((mu4e-compose-pre . org-msg-mode))
  :config
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

  ;; advise the mu4e core function mu4e~compose-handler so that it doesn't move cursor to start of message body when replying or forwarding messages
  (defun ijf-reply-fwd-advice (orig &rest args)
    "Move cursor to the start of the reply in org-msg mode"
    ;; first run the mu4e function
    (apply orig args)
    ;; then, move the cursor to the body if its a reply (but not new composition)
    ;; NOTE: first element in "args" holds the type of email that's being composed.
    ;; default behavior for new and forwarded email is to start in the "to" field
    (if (member (car args) '(reply edit))
        (org-msg-goto-body)))

  (defun ijf-mu4e-add-attachment-icons ()
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

  (defun ijf-mu4e-remove-attachment-icons ()
    "Remove all icon overlays."
    (save-restriction
      (widen)
      (mapc #'delete-overlay
            (cl-remove-if-not
             (lambda (ov)
               (overlay-get ov 'attachment-icon))
             (overlays-in (point-min) (point-max))
             ))))

  (defun ijf-mu4e-reset-attachment-icons (orig-fun &rest args)
    "Reset overlays for attachment icons"
    (apply orig-fun args)
    (ijf-mu4e-remove-attachment-icons)
    (ijf-mu4e-add-attachment-icons))

  (advice-add 'org-msg-attach-attach :around #'ijf-mu4e-reset-attachment-icons)
  (advice-add 'org-msg-dired-attach :around #'ijf-mu4e-reset-attachment-icons)
  (advice-add 'org-msg-attach-delete :around #'ijf-mu4e-reset-attachment-icons)

  (advice-add 'mu4e~compose-handler :around #'ijf-reply-fwd-advice)
  (define-key org-msg-edit-mode-map (kbd "C-c f") 'ijf-hydra-jump-to-email-fields/body)
  (define-key org-msg-edit-mode-map (kbd "C-c w") 'ijf-hydra-copy-email-fields-to-kill-RING/BODY)
  (define-key org-msg-edit-mode-map (kbd "C-c w") 'ijf-hydra-copy-email-fields-to-kill-RING/BODY)

  )

;;;;;; Helper functions
(defun ijf-mu4e-queries-help ()
  "Look up search syntax"
  (interactive)
  (split-window-vertically)
  (woman "mu-query")
  (search-forward "FIELDS"))

;; TODO: incomplete. need to think of what I really want here
(defun ijf-daily-email-progress ()
  "Plot my daily, weekly, and monthly email progress.
A shell script queries mu every five minutes via the xbar app."
  (interactive "P")
  (let (file "/Users/ilya/Work/01-09 meta/01 productivity/email-inbox-count-log.csv")
    ))

;;;; Email Addressing
;; function to return first name of email recipients
;; used by yasnippet
;; inspired by
;;http://blog.binchen.org/posts/how-to-use-yasnippets-to-produce-email-templates-in-emacs.html
;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/
(defun cpm/mu4e-get-names-for-yasnippet ()
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
;;;; Colors for header view
(use-package mu4e-column-faces
  :after mu4e
  :custom-face
  (mu4e-column-faces-date ((t (:foreground unspecified :background unspecified :inherit lambda-green))))
  (gnus-header-name ((t (:foreground unspecified :background unspecified :inherit lambda-green))))
  (mu4e-column-faces-to-from ((t (:foreground unspecified :background unspecified :inherit lambda-focus))))
  :config
  (mu4e-column-faces-mode))

;;; End Setup Email
(provide 'my-setup-mail)
