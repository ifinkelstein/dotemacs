;; my-setup-slack.el -*- lexical-binding: t -*-

;; Slack Management
(message "Setting up Slack...")

;;* Slack
(use-package emacs-slack
  :vc (:url "https://github.com/emacs-slack/emacs-slack")
  :commands (slack-start slack-register-team)
  :hook ((slack-mode . variable-pitch-mode)
         (slack-mode . emojify-mode)
         (slack-message-buffer-mode . olivetti-mode)
         (slack-message-buffer-mode . abbrev-mode))
  :custom-face
  (slack-message-output-header ((t (:foreground "DarkBlue"))))
  (slack-message-mention-me-face ((t (:foreground "Blue" :background "LightGray"))))
  (slack-message-mention-face ((t (:foreground "Blue" :background "LightGray"))))

  :bind ( (:map slack-mode-map
                (("@" . slack-message-embed-mention)
                 ("#" . slack-message-embed-channel)
                 ("M-o" . casual-slack-dispatch)
                 ("C-." . flyspell-auto-correct-word)))
          (:map slack-thread-message-buffer-mode-map
                (("C-c '" . slack-message-write-another-buffer)
                 ("@" . slack-message-embed-mention)
                 ("#" . slack-message-embed-channel)
                 ("M-o" . casual-slack-dispatch)
                 ("C-." . flyspell-auto-correct-word)))
          (:map slack-message-buffer-mode-map
                (("C-c '" . slack-message-write-another-buffer)
                 ("M-o" . casual-slack-dispatch)
                 ("C-." . flyspell-auto-correct-word)))
          (:map slack-message-edit-buffer-mode-map
                (("@" . slack-message-embed-mention)
                 ("#" . slack-message-embed-channel)
                 ("M-o" . casual-slack-dispatch)
                 ("C-." . flyspell-auto-correct-word)))
          (:map slack-message-compose-buffer-mode-map
                (("@" . slack-message-embed-mention)
                 ("#" . slack-message-embed-channel)
                 ("C-c '" . slack-message-send-from-buffer)
                 ("M-o" . casual-slack-dispatch)
                 ("C-." . flyspell-auto-correct-word))))
  :config
  (setq slack-extra-subscribed-channels (mapcar 'intern (list "some-channel")))
  (setq slack-buffer-emojify t) ;; uses emojify as a dependency
  (setq slack-prefer-current-team t) ;; default to current team


  ;; copied from slack-file-upload
  ;; goal is to avoid any extraneous prompts or messages when uploading a file
  ;; todo: finish fixing this 
  (defun my-slack-file-upload (file filetype filename &optional msg)
    "Uploads FILE with FILETYPE and FILENAME to the current slack buffer and team. Avoid questions. Include an optional message. "
    (interactive
     (let ((file (expand-file-name (car (find-file-read-args "Select File: " t)))))
       (list file
             (slack-file-select-filetype (file-name-extension file))
             (read-from-minibuffer "Filename: " (file-name-nondirectory file)))))

    (slack-if-let*
        ((buffer slack-current-buffer)
         (team (slack-buffer-team buffer))
         (initial-comment msg))
        (cl-labels
            ((on-file-upload (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-file-upload"))))

          (slack-request
           (slack-request-create
            slack-file-upload-url
            team
            :type "POST"
            :params (append (slack-file-upload-params buffer)
                            (list
                             (cons "filename" filename)
                             (cons "filetype" filetype)
                             (if initial-comment
                                 (cons "initial_comment" initial-comment))))
            :files (list (cons "file" file))
            :headers (list (cons "Content-Type" "multipart/form-data"))
            :success #'on-file-upload)))
      (error "Call from message buffer or thread buffer")))
  )

;;** add emoji support
;; to integrate with emacs-slack, the display style has to be 'image
;; other styles crash the channel display and control
;; may be an underlying bug in the circe library that emacs-slack uses
(use-package emojify
  :hook (slack-message-buffer . emojify-mode)
  :config
  (when (member "Apple Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Apple Emoji") nil 'prepend))
  (setq emojify-display-style 'image) ;;switching to unicode crashes slack
  (setq emojify-emoji-styles '(unicode github)))

;;** Slack Workspace (tabspaces)

(defun my-open-slack-in-workspace ()
  "Open Slack in its own tab-bar workspace.
If the workspace already exists, switch to it."
  (interactive)
  (if (member "Slack" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "Slack")
    (tab-bar-new-tab)
    (tab-bar-rename-tab "Slack")
    (delete-other-windows)
    (slack-select-unread-rooms)))

;;** Helpers

(defun my-slack-message-add-reaction (str)
  "Add reaction STR to message at point.
STR should be the emoji name without colons, e.g. \"tent\" not \":tent:\"."
  (interactive "sEmoji name: ")
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf))
                  (str-emoji (string-trim str ":"  ":")))
      (slack-buffer-add-reaction-to-message buf str-emoji (slack-get-ts))))

(defun my-slack-attach-dired-to-buffer (files)
  "Attach FILES marked in dired to the Slack conversation in other window."
  (interactive
   (list (dired-get-marked-files)))
  (unless (eq major-mode 'dired-mode)
    (user-error "This command must be triggered in a dired buffer"))
  (let ((start-win (selected-window))
        (other-win
         (get-window-with-predicate
          (lambda (window)
            (with-current-buffer (window-buffer window)
              (derived-mode-p 'slack-mode))))))
    (unless other-win
      (user-error "No window displaying a Slack buffer"))
    (select-window other-win)
    (dolist (file files)
      (slack-file-upload file
                         (slack-file-select-filetype (file-name-extension file))
                         (file-name-nondirectory file)))
    (select-window start-win)))

(defun my-slack-thumbsup ()
  "Add 👍 reaction or insert emoji depending on context."
  (interactive)
  (if (get-text-property (point) 'read-only)
      (progn
        (my-slack-message-add-reaction "thumbsup")
        (goto-char (point-max)))
    (insert "👍")))

(defun my-slack-eyes ()
  "Add 👀 reaction to message at point."
  (interactive)
  (my-slack-message-add-reaction "eyes"))

(defun my-slack-check ()
  "Add ✅ reaction to message at point."
  (interactive)
  (my-slack-message-add-reaction "white_check_mark"))

;;** Context-aware transients (casual-style)

;; Global — available from anywhere, the main entry point
(transient-define-prefix casual-slack ()
  "Slack."
  ["Slack"
   ["Navigate"
    ("c" "Channel"       slack-select-rooms)
    ("u" "Unread"        slack-select-unread-rooms)
    ("i" "DM"            slack-im-select)
    ("g" "Group"         slack-group-select)
    ("a" "Activity Feed" slack-activity-feed-show)
    ("T" "All Threads"   slack-all-threads)]
   ["Search"
    ("s" "Messages"  slack-search-from-messages)
    ("S" "Files"     slack-search-from-files)]
   ["Users"
    ("p" "Profile"  slack-user-select)
    ("P" "Status"   slack-user-set-status)]
   ["Connection"
    ("o" "Start"  slack-start)
    ("K" "Stop"   slack-stop)
    ("R" "Refresh Token" slack-refresh-token)]])

;; Message buffer — reading a channel or DM
(transient-define-prefix casual-slack-message-buffer ()
  "Slack › Channel"
  :transient-suffix 'transient--do-stay
  ["Navigate"
   ["Move"
    ("n" "Next msg"  slack-buffer-goto-next-message :transient t)
    ("p" "Prev msg"  slack-buffer-goto-prev-message :transient t)
    ("M-<" "First"   slack-buffer-goto-first-message :transient t)
    ("M->" "Last"    slack-buffer-goto-last-message :transient t)
    ("m" "Load more" slack-load-more-message :transient t)]
   ["Open"
    ("t"   "Thread"     slack-thread-show-or-create :transient nil)
    ("u"   "Unread"     slack-select-unread-rooms :transient nil)
    ("c"   "Channel"    slack-select-rooms :transient nil)
    ("j"   "App"        slack-jump-to-app :transient nil)
    ("J"   "Browser"    slack-jump-to-browser :transient nil)
    ("RET" "Open link"  slack-open-link :transient nil)]]
  ["Act"
   ["Message"
    ("e" "Edit"         slack-message-edit :transient nil)
    ("d" "Delete"       slack-message-delete :transient nil)
    ("y" "Copy link"    slack-message-copy-link :transient nil)
    ("s" "Share"        slack-message-share :transient nil)
    ("g" "Redisplay"    slack-message-redisplay :transient t)]
   ["React"
    ("r"  "Add reaction"    slack-message-add-reaction :transient nil)
    ("R"  "Remove reaction" slack-message-remove-reaction :transient nil)
    ("1"  "👍"  my-slack-thumbsup :transient t)
    ("2"  "👀"  my-slack-eyes :transient t)
    ("3"  "✅"  my-slack-check :transient t)]
   ["Star & Pin"
    ("*"  "Star"   slack-message-add-star :transient nil)
    ("8"  "Unstar" slack-message-remove-star :transient nil)
    ("+"  "Pin"    slack-message-pins-add :transient nil)
    ("-"  "Unpin"  slack-message-pins-remove :transient nil)]
   ["Follow"
    ("f"  "Follow"   slack-message-follow :transient nil)
    ("F"  "Unfollow" slack-message-unfollow :transient nil)]]
  ["Compose & Files"
   ["Write"
    ("w"  "Write in buffer" slack-message-write-another-buffer :transient nil)
    ("q"  "Quote reply"     slack-quote-and-reply :transient nil)
    ("Q"  "Quote w/ link"   slack-quote-and-reply-with-link :transient nil)
    ("@"  "Mention"         slack-message-embed-mention :transient nil)
    ("#"  "Channel link"    slack-message-embed-channel :transient nil)
    ("l"  "Insert link"     slack-insert-link :transient nil)]
   ["Emoji"
    ("E"  "Insert emoji" slack-insert-emoji :transient nil)]
   ["Files"
    ("U"  "Upload file"      slack-file-upload :transient nil)
    ("V"  "Upload clipboard" slack-clipboard-image-upload :transient nil)
    ("C"  "Upload snippet"   slack-file-upload-snippet :transient nil)
    ("D"  "Dired → Slack"    my-slack-attach-dired-to-buffer :transient nil)]]
  ["Channel"
   ["Info"
    ("b" "Bookmarks" slack-show-channel-bookmarks :transient nil)
    ("P" "Pins"      slack-room-pins-list :transient nil)
    ("T" "Threads"   slack-room-unread-threads :transient nil)
    ("I" "Inspect"   slack-message-inspect :transient nil)]
   ["Manage"
    ("G"  "Refresh list" slack-conversations-list-update-quick :transient nil)
    ("M"  "Mark read"    slack-message-update-mark :transient nil)]])

;; Thread buffer
(transient-define-prefix casual-slack-thread ()
  "Slack › Thread"
  ["Thread"
   ["Navigate"
    ("n" "Next msg"  slack-buffer-goto-next-message)
    ("p" "Prev msg"  slack-buffer-goto-prev-message)
    ("C" "↑ Channel" slack-thread-message-buffer-jump-to-channel-buffer)]
   ["Compose"
    ("w" "Write in buffer" slack-message-write-another-buffer)
    ("@" "Mention"         slack-message-embed-mention)
    ("#" "Channel link"    slack-message-embed-channel)
    ("l" "Insert link"     slack-insert-link)
    ("E" "Insert emoji"    slack-insert-emoji)]
   ["Message"
    ("e" "Edit"     slack-message-edit)
    ("d" "Delete"   slack-message-delete)
    ("y" "Copy link" slack-message-copy-link)
    ("s" "Share"    slack-message-share)
    ("g" "Redisplay" slack-message-redisplay)]
   ["React"
    ("r" "Add reaction"    slack-message-add-reaction)
    ("R" "Remove reaction" slack-message-remove-reaction)
    ("1" "👍" my-slack-thumbsup)
    ("2" "👀" my-slack-eyes)
    ("3" "✅" my-slack-check)]
   ["Files"
    ("U" "Upload"    slack-file-upload)
    ("V" "Clipboard" slack-clipboard-image-upload)]])

;; Compose / edit buffer
(transient-define-prefix casual-slack-compose ()
  "Slack › Compose"
  ["Compose Message"
   ["Send"
    ("C-c" "Send" slack-message-send-from-buffer)
    ("C-k" "Cancel" slack-message-cancel-edit)]
   ["Embed"
    ("@" "Mention" slack-message-embed-mention)
    ("#" "Channel" slack-message-embed-channel)
    ("l" "Link"    slack-insert-link)
    ("E" "Emoji"   slack-insert-emoji)]
   ["Attach"
    ("U" "Upload file" slack-file-upload)
    ("V" "Clipboard"   slack-clipboard-image-upload)
    ("C" "Snippet"     slack-file-upload-snippet)]])

;; Search results
(transient-define-prefix casual-slack-search ()
  "Slack › Search Results"
  ["Search Results"
   ["Navigate"
    ("RET" "Open message" slack-search-result-open-message)
    ("n"   "Next msg"     slack-buffer-goto-next-message)
    ("p"   "Prev msg"     slack-buffer-goto-prev-message)]
   ["Search"
    ("s" "Search messages" slack-search-from-messages)
    ("S" "Search files"    slack-search-from-files)]
   ["Go"
    ("c" "Select rooms"   slack-select-rooms)
    ("u" "Unread rooms"   slack-select-unread-rooms)]])

;; Stars
(transient-define-prefix casual-slack-stars ()
  "Slack › Starred Items"
  ["Starred Items"
   ["Navigate"
    ("n" "Next"  slack-buffer-goto-next-message)
    ("p" "Prev"  slack-buffer-goto-prev-message)]
   ["Actions"
    ("RET" "Open" slack-pinned-items-open-message)
    ("8"   "Unstar" slack-message-remove-star)]])

;; Activity feed
(transient-define-prefix casual-slack-activity ()
  "Slack › Activity Feed"
  ["Activity Feed"
   ["Navigate"
    ("n"   "Next"   slack-buffer-goto-next-message)
    ("p"   "Prev"   slack-buffer-goto-prev-message)
    ("RET" "Open"   slack-activity-feed-open-message)]
   ["Go"
    ("c" "Channel" slack-select-rooms)
    ("u" "Unread"  slack-select-unread-rooms)]])

;; Smart dispatcher — picks the right transient for current context
(defun casual-slack-dispatch ()
  "Context-aware Slack transient dispatch.
Selects the appropriate transient based on the current major mode."
  (interactive)
  (cond
   ((derived-mode-p 'slack-message-compose-buffer-mode)  (casual-slack-compose))
   ((derived-mode-p 'slack-message-edit-buffer-mode)     (casual-slack-compose))
   ((derived-mode-p 'slack-thread-message-buffer-mode)   (casual-slack-thread))
   ((derived-mode-p 'slack-message-buffer-mode)          (casual-slack-message-buffer))
   ((derived-mode-p 'slack-search-result-buffer-mode)    (casual-slack-search))
   ((derived-mode-p 'slack-stars-buffer-mode)            (casual-slack-stars))
   ((derived-mode-p 'slack-activity-feed-buffer-mode)    (casual-slack-activity))
   ((derived-mode-p 'slack-all-threads-buffer-mode)      (casual-slack-thread))
   ((derived-mode-p 'slack-mode)                         (casual-slack-message-buffer))
   ((derived-mode-p 'slack-buffer-mode)                  (casual-slack))
   (t                                                    (casual-slack))))

(provide 'my-setup-slack)
;;; my-setup-slack.el ends here
