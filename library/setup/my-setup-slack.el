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
                 ("M-o" . my-transient-slack)
                 ("C-." . flyspell-auto-correct-word)))
          (:map slack-thread-message-buffer-mode-map
                (("C-c '" . slack-message-write-another-buffer)
                 ("@" . slack-message-embed-mention)
                 ("#" . slack-message-embed-channel)
                 ("C-." . flyspell-auto-correct-word)))
          (:map slack-message-buffer-mode-map
                (("C-c '" . slack-message-write-another-buffer)
                 ("C-." . flyspell-auto-correct-word)))
          (:map slack-message-compose-buffer-mode-map
                (("C-c '" . slack-message-send-from-buffer)
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

;;** transient for dealing with emacs-slack

(defun my-slack-message-add-reaction (str)
  "Modified slack add-reaction that takes an emoji as input.
  The str var has to be 'emoji-name,' without the :...:
  For example, pass 'tent' but not ':tent:'"
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf))
                  ;; later, add some error correction to remove :...: colons
                  (str-emoji str))
      (slack-buffer-add-reaction-to-message buf
                                            str-emoji
                                            (slack-get-ts))))

(defclass slack-prefix (transient-prefix) ()
  "Prefix class for Slack commands.")

(defclass slack-suffix (transient-suffix) ()
  "Suffix class for Slack commands.")

(defun my-slack-attach-dired-to-buffer (files)
  "Attach FILES marked or current file in `dired' to slack converration in other window.
Precondition: Point must be in a `dired' buffer.
Idea taken from `org-attach-dired-to-subtree'."
  (interactive
   (list (dired-get-marked-files)))
  (unless (eq major-mode 'dired-mode)
    (user-error "This command must be triggered in a `dired' buffer"))
  (let ((start-win (selected-window))
        (other-win
         (get-window-with-predicate
          (lambda (window)
            (with-current-buffer (window-buffer window)
              (derived-mode-p 'slack-mode))))))
    (unless other-win
      (user-error
       "Can't upload to Slack.  No window displaying a Slack buffer"))
    (select-window other-win)
    (dolist (file files)
      ;; this is where the slack attach has to happen
      (slack-file-upload file
                         (slack-file-select-filetype (file-name-extension file))
                         (file-name-nondirectory file)))
    (select-window start-win)
    ))

(transient-define-prefix my-transient-slack ()
  "Slack command menu."
  [["Basic Operations"
    ("K" "Stop Slack" slack-stop)
    ("c" "Select Rooms" slack-select-rooms)
    ("u" "Select Unread Rooms" slack-select-unread-rooms)
    ("U" "Select User" slack-user-select)]
   ["Messages"
    ("s" "Search Messages" slack-search-from-messages)
    ("E" "Edit Message" slack-message-edit)
    ("r" "Add Reaction" slack-message-add-reaction)
    ("t" "Thread Show/Create" slack-thread-show-or-create)
    ("g" "Redisplay Message" slack-message-redisplay)]
   ["Navigation"
    ("J" "Jump to Browser" slack-jump-to-browser)
    ("j" "Jump to App" slack-jump-to-app)]]
  [["Composition"
    ("@" "Embed Mention" slack-message-embed-mention)
    ("#" "Embed Channel" slack-message-embed-channel)
    ("'" "Write in Buffer" slack-message-write-another-buffer)
    ("RET" "Send from Buffer" slack-message-send-from-buffer)]
   ["Files"
    ;; maybe force opening a dired window if not open?
    ("D" "via dired" my-slack-attach-dired-to-buffer) 
    ]]

  
  [["Quoting"
    ("q" "Quote and Reply" slack-quote-and-reply)
    ("Q" "Quote and Reply with Link" slack-quote-and-reply-with-link)]
   ["Updates"
    ("G" "Update Conversations List" slack-conversations-list-update-quick)]
   ["Emoji"
    ("ee" "Insert Emoji" slack-insert-emoji)
    ("et" "👍" (lambda ()
                 (interactive)
                 (let ((read-only (get-text-property (point) 'read-only)))
                   (if read-only
                       (progn
                         (my-slack-message-add-reaction "thumbsup")
                         (end-of-buffer))
                     (insert "👍")))))    ]])

;; TODO: fix this function
;; currently, its just a copy/paste from gnus
(defun my-slack-dired-attach (files-to-attach)
  "Attach Dired's marked files to an emacs-slack channel.
If called non-interactively, FILES-TO-ATTACH should be a list of
filenames."
  (interactive
   (list
    (delq nil
	      (mapcar
	       ;; don't attach directories
	       (lambda (f) (if (file-directory-p f) nil f))
	       (nreverse (dired-map-over-marks (dired-get-filename) nil)))))
   dired-mode)
  (let ((destination nil)
	    (files-str nil)
	    (bufs nil))
    ;; warn if user tries to attach without any files marked
    (if (null files-to-attach)
	    (error "No files to attach")
      (setq files-str
	        (mapconcat
	         (lambda (f) (file-name-nondirectory f))
	         files-to-attach ", "))
      (setq bufs (gnus-dired-mail-buffers))

      ;; set up destination mail composition buffer
      (if (and bufs
	           (y-or-n-p "Attach files to existing mail composition buffer? "))
	      (setq destination
		        (if (= (length bufs) 1)
		            (get-buffer (car bufs))
		          (gnus-completing-read "Attach to buffer"
                                        bufs t nil nil (car bufs))))
	    ;; setup a new mail composition buffer
	    (let ((mail-user-agent gnus-dired-mail-mode)
	          ;; A workaround to prevent Gnus from displaying the Gnus
	          ;; logo when invoking this command without loading Gnus.
	          ;; Gnus demonstrates it when gnus.elc is being loaded if
	          ;; a command of which the name is prefixed with "gnus"
	          ;; causes that autoloading.  See the code in question,
	          ;; that is the one first found in gnus.el by performing
	          ;; `C-s this-command'.
	          (this-command (if (eq gnus-dired-mail-mode 'gnus-user-agent)
				                'gnoose-dired-attach
			                  this-command)))
	      (compose-mail))
	    (setq destination (current-buffer)))

      ;; set buffer to destination buffer, and attach files
      (set-buffer destination)
      (when gnus-dired-attach-at-end
        (goto-char (point-max)))		;attach at end of buffer
      (while files-to-attach
	    (mml-attach-file (car files-to-attach)
			             (or (mm-default-file-type (car files-to-attach))
			                 "application/octet-stream")
			             nil)
	    (setq files-to-attach (cdr files-to-attach)))
      (message "Attached file(s) %s" files-str))))

(provide 'my-setup-slack)
;;; my-setup-slack.el ends here
