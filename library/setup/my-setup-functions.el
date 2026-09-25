;; my-setup-functions.el  -*- lexical-binding: t -*-

(message "Setting up my helper functions...")



;;* Config Helper Functions
;;** Goto Functions
(defun my-goto-init.el ()
  "Open init.el."
  (interactive)
  (find-file user-init-file))

(defun my-goto-emacs-dir ()
  "Go to Emacs dir."
  (interactive)
  (find-file user-emacs-directory))

(defvar my-files-sources-data
  `(("Init Files"      ?i ,my-emacs-dir)
    ("Setup Files"     ?s ,my-setup-dir)
    ("User Files"      ?u ,my-user-dir))
  "Define titles, quick-keys, and directories to be searched for files.")

(defun my--files-make-source (name char dir)
  "Return a source list suitable for `consult--multi'.
  NAME is the source name, CHAR is the narrowing character,
  and DIR is the directory to find files. "
  (let ((idir (propertize (file-name-as-directory dir) 'invisible t)))
    `(:name     ,name
      :narrow   ,char
      :category file
      :face     consult-file
      :items    ,(lambda () (mapcar (lambda (f) (concat idir f))
				               ;; filter files that glob *.*
				               (directory-files dir nil "[^.].*[.].+")))
      :action   ,(lambda (f) (find-file f)))))

(defun my-find-emacs-file ()
  "Find a file from list of Emacs configuration files."
  (interactive)
  (require 'consult)
  (consult--multi (mapcar #'(lambda (s) (apply 'my--files-make-source s))
			              my-files-sources-data)
		          :prompt "files: "
		          :history 'file-name-history))

(defun my-search-emacs-files ()
  "Search all configuration files with consult-ripgrep."
  (interactive)
  (require 'consult)
  (let ((consult-ripgrep-args
         "rg --null --line-buffered --max-columns=1000 --path-separator /\
   --smart-case --no-heading --line-number --hidden --glob=library/** --glob=!straight --glob=!var --glob=!.git/ ."))
    (if (executable-find "rg")
        (consult-ripgrep my-emacs-dir)
      (message "Please install `rg' first."))))

;; Load init file
(defun my-load-init-file ()
  "Load the base init file."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;;* Built-in Functions
;; These are useful built-in functions, but you have to enable them
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Not going to use these commands
(put 'ns-print-buffer 'disabled t)
(put 'suspend-frame 'disabled t)

;;* CRUX
;; A Collection of Ridiculously Useful eXtensions for Emacs. Crux bundles many
;; useful interactive commands to enhance your overall Emacs experience. Most of
;; the crux commands are related to the editing experience, but there are also a
;; bunch of utility commands that are just very useful to have (e.g.
;; crux-open-with and crux-reopen-as-root). Originally part of Emacs Prelude.
(use-package crux
  :defer 1
  :bind
  ;; C-a is left to mwim (remap of move-beginning-of-line)
  ("C-k"   . crux-smart-kill-line))

;;* Search Functions
;;** Search given directory
(defun my-search-in-input-dir ()
  "Call `consult-ripgrep' with a prefix arg of 4 to search in input directory."
  (interactive)
  (let ((current-prefix-arg '(4))) (call-interactively #'consult-ripgrep)))


;;* Frame Functions
;;** Delete Frame or Quit
(defun my-delete-frame-or-quit ()
  "Delete the selected frame & kill terminal buffers. If the last frame, kill Emacs."
  (interactive)
  (when (condition-case nil (delete-frame)
          (error (save-buffers-kill-emacs))))
  (select-frame-set-input-focus (selected-frame)))

;;* Window Functions
;; Exchange Windows
;; Swap buffers in windows and leave the cursor in the original window. Courtesy of
;; Mike Zamansky's video.
;; http://cestlaz.github.io/posts/using-emacs-36-touch-of-elisp/#.WX5Wg0czpcx

(defun my-window-exchange-buffer ()
  "Swap buffer in windows and leave focus in original window"
  (interactive)
  (ace-swap-window)
  (aw-flip-window))

;; Focus Window Split
;; Easy split and move functions
(defun my-split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (require 'windmove)
  (split-window-right)
  (windmove-right))

(defun my-split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (require 'windmove)
  (split-window-below)
  (windmove-down))

(defun my-cycle-window-split-ratio ()
  "Cycle the selected window through 1/2 → 2/3 → 1/3 size.
Works with exactly two windows in any split direction."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "Need exactly 2 windows"))
  (let* ((horiz (window-combined-p nil t))
         ;; Total-size units throughout: `window-resize' deltas are in
         ;; total lines/columns, and `window-width' (body) would
         ;; undercount by fringe/divider columns.
         (size (lambda (w) (if horiz (window-total-width w) (window-total-height w))))
         (total (funcall size (frame-root-window)))
         (current (funcall size (selected-window)))
         (ratio (/ (float current) total))
         (target (cond
                  ((< ratio 0.4)  0.5)    ; ~1/3 → 1/2
                  ((< ratio 0.58) 0.667)  ; ~1/2 → 2/3
                  (t              0.333))) ; ~2/3 → 1/3
         (delta (- (round (* target total)) current)))
    (window-resize nil delta horiz)))

;; Jump to Minibuffer Window
(defun my-goto-minibuffer-window ()
  "locate point to minibuffer window if it is active."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

;;** Split With Agent Terminal
;; Left: the current text buffer.  Right: a terminal session running in that
;; buffer's own directory.  Sessions are classified by their process command
;; line rather than by buffer name, because ghostel renames its buffers from
;; the terminal title (a live Claude session is "*ghostel: <session title>*",
;; not "*claude:...*").

(defvar ghostel-identity)
(declare-function ghostel-buffer-list "ghostel")
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel-send-string "ghostel" (string))

(defvar my-agent-split-preference '(claude codex shell)
  "Kinds of terminal session to reuse, most preferred first.
Also the order the right window cycles through on repeat calls.")

(defun my-agent--directory ()
  "Directory the agent should run in.
The current file's directory, or `default-directory' in a non-file buffer."
  (file-truename (if buffer-file-name
                     (file-name-directory buffer-file-name)
                   default-directory)))

(defun my-agent--session-kind (buffer)
  "Classify ghostel BUFFER as `claude', `codex' or `shell'.
Reads the exec'd program from `ghostel-identity'; with the native PTY
there is no Emacs process object to inspect."
  (let ((cmd (string-join
              (or (alist-get 'command (buffer-local-value 'ghostel-identity buffer))
                  (and-let* ((proc (get-buffer-process buffer)))
                    (process-command proc)))
              " ")))
    (cond ((string-match-p "\\bclaude\\b" cmd) 'claude)
          ((string-match-p "\\bcodex\\b" cmd) 'codex)
          (t 'shell))))

(defun my-agent--sessions (dir)
  "Live ghostel buffers running in DIR, ordered by `my-agent-split-preference'."
  (let ((bufs (seq-filter
               (lambda (buf)
                 (string= dir (file-truename
                               (buffer-local-value 'default-directory buf))))
               (and (fboundp 'ghostel-buffer-list) (ghostel-buffer-list)))))
    (mapcan (lambda (kind)
              (seq-filter (lambda (buf) (eq kind (my-agent--session-kind buf)))
                          bufs))
            my-agent-split-preference)))

(defun my-agent--start-claude (dir window)
  "Start a Claude Code session in DIR, displayed in WINDOW."
  (require 'cl-lib)
  (require 'claude-code)
  ;; `claude-code--directory' prefers the project root; we want the file's
  ;; own directory, so shadow it for the duration of the call the same way
  ;; `claude-code-start-in-directory' does.
  (let ((claude-code-display-window-fn
         (lambda (buffer) (set-window-buffer window buffer) window)))
    (cl-letf (((symbol-function 'claude-code--directory) (lambda () dir)))
      (claude-code))))

(defun my-split-with-agent (&optional new)
  "Put this buffer on the left and an agent terminal on the right.

The terminal runs in the current file's directory.  An existing session
for that directory is reused, preferring Claude Code, then Codex, then any
other ghostel terminal; if none exists, a new Claude Code session is
started there.  Repeat calls cycle the right window through the remaining
sessions for that directory.  With prefix arg NEW, always start a fresh
Claude Code session."
  (interactive "P")
  (let* ((dir (my-agent--directory))
         (sessions (unless new (my-agent--sessions dir)))
         (right (window-in-direction 'right)))
    ;; Rebuild the two-window layout unless the right window already holds
    ;; one of the candidate sessions (that is the repeat-call case).
    (unless (and right (memq (window-buffer right) sessions))
      (delete-other-windows)
      (setq right (split-window-right)))
    (if (null sessions)
        (my-agent--start-claude dir right)
      (let* ((pos (seq-position sessions (window-buffer right)))
             (next (nth (if pos (mod (1+ pos) (length sessions)) 0) sessions)))
        (set-window-buffer right next)))
    (select-window right)))

;;** Agent Edit DWIM
;; Hand the section/sentence/paragraph/region at point, or the mu4e
;; message at point, to the live Claude Code or Codex session next door,
;; with a one-line instruction typed at the minibuffer.  The agent gets
;; the file, line range, quoted text and a standing brief: edit the file
;; in place when asked to rewrite, otherwise act on the text (fact-check,
;; explain, verify) and report.  For mail it gets the maildir path and
;; headers and reads the message itself.

(defvar my-agent-edit-brief
  "The instruction applies to the quoted passage, or to the whole section \
at the lines given when only its heading is quoted.  If it asks for a \
rewrite, edit the file in place: change only that passage unless a \
correct edit needs more, keep markup, citations and labels intact, and \
match the surrounding voice.  If it asks a question or for a check \
(fact-check a claim, verify a citation, explain), do that and reply \
briefly; edit only if the instruction implies a fix.  Do not reflow or \
reformat unrelated text."
  "Standing instructions appended to every `my-agent-edit-dwim' request.")

(defvar my-agent-edit-history nil
  "Minibuffer history for `my-agent-edit-dwim' instructions.")

(defun my-agent--edit-target (dir)
  "Agent buffer to send to: one visible in this frame, else one running in DIR.
Only `claude' and `codex' sessions qualify.  Among visible ones a session
in DIR wins; otherwise any visible agent does, since the agent for a
project is often started at the repo root rather than the file's folder."
  (let* ((agent-p (lambda (buf)
                    (and (eq (buffer-local-value 'major-mode buf) 'ghostel-mode)
                         (memq (my-agent--session-kind buf) '(claude codex)))))
         (visible (seq-filter agent-p (mapcar #'window-buffer (window-list nil 'no-mini))))
         (in-dir (lambda (buf)
                   (string= dir (file-truename
                                 (buffer-local-value 'default-directory buf))))))
    (or (seq-find in-dir visible)
        (car visible)
        (seq-find agent-p (my-agent--sessions dir))
        (user-error "No Claude Code or Codex session visible or running in %s"
                    (abbreviate-file-name dir)))))

(defun my-agent--heading-regexp ()
  "Regexp for lines that start a section, or nil outside Org and LaTeX.
In LaTeX only the sectioning commands count: AUCTeX's `outline-regexp'
also matches \\begin{document}, \\end{document} and the `TeX-outline-extra'
comment markers, whose subtrees can run to the end of the file."
  (cond ((derived-mode-p 'org-mode) outline-regexp)
        ((derived-mode-p 'LaTeX-mode 'latex-mode)
         "[ \t]*\\\\\\(?:part\\|chapter\\|\\(?:sub\\)*section\\|\\(?:sub\\)?paragraph\\)\\b")))

(defun my-agent--on-heading-p ()
  "Non-nil when point is on a line matching `my-agent--heading-regexp'."
  (when-let* ((regexp (my-agent--heading-regexp)))
    (save-excursion
      (forward-line 0)
      (looking-at regexp))))

(defun my-agent--edit-unit (arg)
  "Unit of text to act on for prefix ARG.
The active region wins; then, on an Org or LaTeX heading line, the
section; else the paragraph with ARG and the sentence without."
  (cond ((use-region-p) 'region)
        ((my-agent--on-heading-p) 'section)
        (arg 'paragraph)
        (t 'sentence)))

(defun my-agent--section-bounds ()
  "Bounds of the heading on this line and its subtree.
The subtree ends before the next heading of the same or a higher level,
or before \\end{document} in the last LaTeX section."
  (let ((outline-regexp (my-agent--heading-regexp)))
    (save-excursion
      (forward-line 0)
      (let ((beg (point))
            (end (progn (outline-end-of-subtree) (point))))
        (goto-char beg)
        (cons beg (if (re-search-forward "^[ \t]*\\\\end *{document}" end t)
                      (match-beginning 0)
                    end))))))

(defun my-agent--edit-bounds (unit)
  "Bounds of UNIT: `region', `section', `sentence' or `paragraph'."
  (pcase unit
    ('region (cons (region-beginning) (region-end)))
    ('section (my-agent--section-bounds))
    (_ (or (bounds-of-thing-at-point unit)
           (user-error "No %s at point" unit)))))

(defun my-agent--read-instruction (target unit)
  "Read a non-empty instruction for TARGET about UNIT from the minibuffer."
  (let ((instruction (string-trim
                      (read-string
                       (format "Agent (%s), %s: " (my-agent--session-kind target) unit)
                       nil 'my-agent-edit-history))))
    (when (string-empty-p instruction)
      (user-error "Empty instruction"))
    instruction))

(defun my-agent--send (target request)
  "Submit REQUEST to the ghostel buffer TARGET and make sure it is shown."
  (with-current-buffer target
    (ghostel-paste-string request)
    (sit-for 0.1)
    (ghostel-send-string "\r"))
  (unless (get-buffer-window target)
    (display-buffer target))
  (deactivate-mark)
  (message "Sent to %s" (buffer-name target)))

(defun my-agent--text-request (target arg)
  "Request about the text at point in this file buffer, for TARGET.
ARG is the raw prefix argument, interpreted by `my-agent--edit-unit'."
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (pcase-let* ((unit (my-agent--edit-unit arg))
               (`(,beg . ,end) (my-agent--edit-bounds unit))
               (text (string-trim (buffer-substring-no-properties beg end)))
               (line1 (line-number-at-pos beg t))
               (line2 (line-number-at-pos (max beg (1- end)) t))
               (instruction (my-agent--read-instruction target unit)))
    (when (buffer-modified-p)
      (save-buffer))
    (format "In %s, line%s%s:\n\n\"\"\"\n%s\n\"\"\"\n\nInstruction: %s\n\n%s"
            (file-relative-name buffer-file-name
                                (buffer-local-value 'default-directory target))
            (if (= line1 line2) (format " %d" line1) (format "s %d-%d" line1 line2))
            (if (eq unit 'section) ", the section headed" "")
            (if (eq unit 'section) (car (split-string text "\n")) text)
            instruction my-agent-edit-brief)))

(defvar my-agent-mail-brief
  "Act on the message above.  If the instruction asks for a reply or a \
forward, use the draft-email skill: save the draft to drafts.org with \
In-Reply-To (or Forward) set to the Message-Id given, in Ilya's voice, \
and do not send anything.  If it asks a question, a summary or a check, \
answer briefly here.  Attachments are already saved at the paths listed; \
the Source line is the raw message for anything else (mu view, mu \
extract).  Do not move, edit or delete anything in the maildir."
  "Standing instructions appended to every mail request from `my-agent-edit-dwim'.")

(defvar my-agent-mail-attachment-dir
  (expand-file-name "mu4e-agent/" (bound-and-true-p my-cache-dir))
  "Directory under which mail attachments are saved for the agent.
Each message gets a subdirectory named after its Message-Id.")

(defvar my-agent-mail-attachment-max (* 25 1024 1024)
  "Largest attachment, in bytes, saved for the agent.")

(defvar my-agent-mail-body-max-lines 150
  "Lines of message body quoted in a request before truncating.")

(declare-function mu4e-message-at-point "mu4e-message" (&optional noerror))
(declare-function mu4e-message-field "mu4e-message" (msg field))
(declare-function mu4e-contact-full "mu4e-contacts" (contact))
(eval-when-compile (require 'mm-decode)) ; for the mm-handle-* accessors
(declare-function mm-dissect-buffer "mm-decode" (&optional no-strict-mime loose-mime from))
(declare-function mm-display-inline "mm-decode" (handle))
(declare-function mm-handle-filename "mm-decode" (handle))
(declare-function mm-save-part-to-file "mm-decode" (handle file))
(declare-function mm-destroy-parts "mm-decode" (handles))
(defvar shr-width)
(defvar shr-inhibit-images)
(defvar mm-inline-text-html-with-images)

(defun my-agent--mm-leaves (handle)
  "Flatten the MIME HANDLE tree from `mm-dissect-buffer' into leaf parts."
  (if (stringp (car handle))
      (mapcan #'my-agent--mm-leaves (cdr handle))
    (list handle)))

(defun my-agent--mm-render (part)
  "Plain text of the text/plain or text/html PART, rendered as mu4e would."
  (with-temp-buffer
    (let ((shr-width 80)
          (shr-inhibit-images t)
          (mm-inline-text-html-with-images nil))
      (mm-display-inline part))
    (string-trim
     (replace-regexp-in-string
      "\n\\{3,\\}" "\n\n"
      (replace-regexp-in-string
       "[ \t]+$" "" (buffer-substring-no-properties (point-min) (point-max)))))))

(defun my-agent--mail-parts (path msgid)
  "Render the message file PATH into (BODY . ATTACHMENTS).
BODY is the text of the first text/plain part, else the first text/html
part rendered with shr.  ATTACHMENTS are the named, non-inline-image parts
saved under `my-agent-mail-attachment-dir'/MSGID, as a list of file names;
parts over `my-agent-mail-attachment-max' bytes are skipped."
  (require 'mm-decode)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (let* ((handles (mm-dissect-buffer t))
           (leaves (my-agent--mm-leaves handles))
           (text-p (lambda (type)
                     (lambda (part)
                       (and (equal (mm-handle-media-type part) type)
                            (not (mm-handle-filename part))))))
           (body-part (or (seq-find (funcall text-p "text/plain") leaves)
                          (seq-find (funcall text-p "text/html") leaves)))
           (dir (expand-file-name
                 (replace-regexp-in-string "[^[:alnum:]._-]" "_" msgid)
                 my-agent-mail-attachment-dir))
           (files nil))
      (unwind-protect
          (progn
            (dolist (part leaves)
              (let ((name (mm-handle-filename part)))
                (when (and name
                           (not (eq part body-part))
                           (not (and (string-prefix-p "image/" (mm-handle-media-type part))
                                     (equal (car (mm-handle-disposition part)) "inline")))
                           (<= (buffer-size (mm-handle-buffer part))
                               my-agent-mail-attachment-max))
                  (make-directory dir t)
                  (let ((file (expand-file-name (file-name-nondirectory name) dir)))
                    (mm-save-part-to-file part file)
                    (push file files)))))
            (cons (if body-part (my-agent--mm-render body-part) "[no text body]")
                  (nreverse files)))
        (mm-destroy-parts handles)))))

(defun my-agent--truncate-lines (text max)
  "TEXT cut to its first MAX lines, with a note on how many were dropped."
  (let ((lines (split-string text "\n")))
    (if (<= (length lines) max)
        text
      (format "%s\n[... %d more lines; see Source]"
              (string-join (seq-take lines max) "\n")
              (- (length lines) max)))))

(defun my-agent--mail-request (target)
  "Request about the mu4e message at point, for TARGET.
Works in the headers view and the message view.  The request carries the
envelope, the rendered body, the saved attachments and the maildir path;
in the message view an active region is quoted as the passage the
instruction applies to."
  (pcase-let* ((msg (mu4e-message-at-point))
               (region (and (use-region-p)
                            (string-trim (buffer-substring-no-properties
                                          (region-beginning) (region-end)))))
               (instruction (my-agent--read-instruction
                             target (if region "message region" "message")))
               (path (mu4e-message-field msg :path))
               (msgid (mu4e-message-field msg :message-id))
               (`(,body . ,files) (my-agent--mail-parts path msgid))
               (contacts (lambda (field)
                           (let ((cs (mu4e-message-field msg field)))
                             (and cs (mapconcat #'mu4e-contact-full cs ", "))))))
    (concat
     (format "Email\nFrom: %s\nTo: %s\n" (funcall contacts :from) (funcall contacts :to))
     (and-let* ((cc (funcall contacts :cc))) (format "Cc: %s\n" cc))
     (format "Date: %s\nSubject: %s\nMessage-Id: <%s>\n\n"
             (format-time-string "%F %R" (mu4e-message-field msg :date))
             (mu4e-message-field msg :subject) msgid)
     (my-agent--truncate-lines body my-agent-mail-body-max-lines)
     "\n\n"
     (and files
          (format "Attachments:\n%s\n\n"
                  (mapconcat (lambda (f)
                               (format "- %s (%s)" f
                                       (file-size-human-readable
                                        (file-attribute-size (file-attributes f)))))
                             files "\n")))
     (format "Source: %s\n\n" path)
     (and region
          (format "The instruction applies to this passage:\n\n\"\"\"\n%s\n\"\"\"\n\n" region))
     (format "Instruction: %s\n\n%s" instruction my-agent-mail-brief))))

(defun my-agent-edit-dwim (&optional arg)
  "Send the thing at point with an instruction to the agent.
In a file buffer the thing is the region if active, else the section
when point is on an Org or LaTeX heading line, else the sentence, or the
paragraph with prefix ARG (see `my-agent--edit-unit').  In a mu4e
headers or message view it is the message at point: envelope, rendered
body, saved attachments and maildir path (see `my-agent--mail-request').

The target is the Claude Code or Codex ghostel session visible in this
frame, else one running in this file's directory.  The request names the
file and line range, quotes the text (only the heading line for a
section, since the agent reads the file anyway), adds the instruction
typed at the prompt, and closes with the standing brief.  It is
submitted at once; point stays here."
  (interactive "P")
  (let ((target (my-agent--edit-target (my-agent--directory))))
    (my-agent--send target
                    (if (derived-mode-p 'mu4e-headers-mode 'mu4e-view-mode)
                        (my-agent--mail-request target)
                      (my-agent--text-request target arg)))))

;;* Buffer Functions
(defun my-narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, markdown
  subtree, or defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive "P")
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'markdown-mode)
         (markdown-narrow-to-subtree))
        ((derived-mode-p 'latex-mode)
         (cond ((ignore-errors (LaTeX-narrow-to-environment) t)) ; inside an env
               (t (my-LaTeX-narrow-to-section))))                ; else the section
        (t (narrow-to-defun))))

(defun my-LaTeX-narrow-to-section (&optional no-subsections)
  "Narrow to the current LaTeX section.
With prefix NO-SUBSECTIONS, exclude nested subsections."
  (interactive "P")
  (save-mark-and-excursion
    (LaTeX-mark-section no-subsections)
    (narrow-to-region (region-beginning) (region-end))))

;; Blank Buffer New Frame
;; Make a blank buffer when opening a new frame. From
;; https://stackoverflow.com/a/25792276.

(defun my-new-buffer-new-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))


;; Create new buffer
(defun my-create-new-buffer ()
  "Create a new buffer in the default major mode."
  (interactive)
  (let ((buffer (generate-new-buffer "*new*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (funcall (default-value 'major-mode)))))

;; Clipboard to/from Buffer
;; http://stackoverflow.com/a/10216338/4869
(defun my-copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun my-copy-buffer-file-name (&optional basename)
  "Copy the current buffer's file name to the clipboard.
By default copy the absolute path.  With a prefix arg BASENAME,
copy just the file name without its directory."
  (interactive "P")
  (if-let* ((file (buffer-file-name)))
      (let ((name (if basename (file-name-nondirectory file)
                    (abbreviate-file-name file))))
        (kill-new name)
        (message "Copied: %s" name))
    (user-error "Buffer is not visiting a file")))

;; Useful Buffers

;; TODO: make this respect workspace buffers
(defun my-user-buffer-q ()
  "Return non-nil if the current buffer is a user buffer.
Buffers whose names start with * and Dired buffers are not user
buffers."
  (not (or (string-prefix-p "*" (buffer-name))
           (derived-mode-p 'dired-mode))))

(defun my--cycle-user-buffer (step)
  "Call STEP until a user buffer is current, at most 20 times.
A \"user buffer\" is determined by `my-user-buffer-q'."
  (funcall step)
  (let ((i 0))
    (while (and (not (my-user-buffer-q)) (< i 20))
      (funcall step)
      (setq i (1+ i)))))

(defun my-next-user-buffer ()
  "Switch to the next user buffer (see `my-user-buffer-q')."
  (interactive)
  (my--cycle-user-buffer #'next-buffer))

(defun my-previous-user-buffer ()
  "Switch to the previous user buffer (see `my-user-buffer-q')."
  (interactive)
  (my--cycle-user-buffer #'previous-buffer))

;;* File Functions
;;** Directory Functions
;;;;; Make Parent Directory
;;  Create a directory – or a hierarchy of them – while finding a file in a
;;  nonexistent directory. From mbork
;;  http://mbork.pl/2016-07-25_Making_directories_on_the_fly

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

;;** Scan Document
(defun my-scan-document ()
  "Scan from the ScanSnap ADF to a PDF and open it in a window on the right.
Runs the scansnap CLI asynchronously (all pages in the feeder,
duplex, color, 300 dpi); see scansnap -h for the underlying tool."
  (interactive)
  (let* ((file (expand-file-name (format-time-string "scan-%Y%m%d-%H%M%S.pdf")
                                 "~/Downloads/"))
         (buf-name " *scansnap*")
         (buf (progn (when (get-buffer buf-name) (kill-buffer buf-name))
                     (get-buffer-create buf-name))))
    (message "Scanning to %s..." file)
    (set-process-sentinel
     (start-process "scansnap" buf "scansnap" file)
     (lambda (process _signal)
       (when (eq (process-status process) 'exit)
         (if (zerop (process-exit-status process))
             (progn
               (when (buffer-live-p buf) (kill-buffer buf))
               (select-window (split-window-right))
               (find-file file))
           (message "Scan failed: %s"
                    (if (buffer-live-p buf)
                        (string-trim (with-current-buffer buf (buffer-string)))
                      (format "exit %d" (process-exit-status process))))))))))

;;* Text Functions
;; Narrow/Widen
;; https://github.com/ultronozm/emacsd/blob/main/init-latex.el
(defun my-widen-first (orig-fun &rest args)
  (save-restriction
    (widen)
    (apply orig-fun args)))

;; Move text to bottom of buffer
;; Adapted from palimpsest package
(defun my-move-region-to-dest (start end dest)
  "Move text between START and END to buffer's desired position, otherwise known as DEST."
  (let ((count (count-words-region start end)))
    (save-excursion
      (kill-region start end)
      (goto-char (funcall dest))
      (yank)
      (newline))
    (push-mark (point))
    (message "Moved %s words" count)))

;; Custom move region to bottom
;; Adapted from palimpsest package
(defun my-move-region-to-bottom (start end)
  "Move text between START and END to bottom of buffer."
  (interactive "r")
  (if (use-region-p)
      (my-move-region-to-dest start end 'point-max)
    (message "No region selected")))

(defun my-move-region-to-top (start end)
  "Move text between START and END to top of buffer."
  (interactive "r")
  (if (use-region-p)
      (my-move-region-to-dest start end 'point-min)
    (message "No region selected")))

;; Fill Paragraph
(defun my-fill-paragraph ()
  "if in an org buffer use org-fill-paragraph; else use fill-paragraph"
  (interactive)
  (if (derived-mode-p 'org-mode)
      (call-interactively #'org-fill-paragraph)
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'my-fill-paragraph)

;; Unfill Paragraph
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun my-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(global-set-key (kbd "M-Q") #'my-unfill-paragraph)

;; Smart Yanking
;;Courtesy of Marcin Borkowski http://mbork.pl/2018-07-02_Smart_yanking

(defun has-space-at-boundary-p (string)
  "Check whether STRING has any whitespace on the boundary.
      Return 'left, 'right, 'both or nil."
  (let ((result nil))
    (when (string-match-p "^[[:space:]]+" string)
      (setq result 'left))
    (when (string-match-p "[[:space:]]+$" string)
      (if (eq result 'left)
          (setq result 'both)
        (setq result 'right)))
    result))

(defun is-there-space-around-point-p ()
  "Check whether there is whitespace around point.
      Return 'left, 'right, 'both or nil."
  (let ((result nil))
    (when (< (save-excursion
               (skip-chars-backward "[:space:]"))
             0)
      (setq result 'left))
    (when (> (save-excursion
               (skip-chars-forward "[:space:]"))
             0)
      (if (eq result 'left)
          (setq result 'both)
        (setq result 'right)))
    result))

(defun set-point-before-yanking (string)
  "Put point in the appropriate place before yanking STRING."
  (let ((space-in-yanked-string (has-space-at-boundary-p string))
        (space-at-point (is-there-space-around-point-p)))
    (cond ((and (eq space-in-yanked-string 'left)
                (eq space-at-point 'left))
           (skip-chars-backward "[:space:]"))
          ((and (eq space-in-yanked-string 'right)
                (eq space-at-point 'right))
           (skip-chars-forward "[:space:]")))))

(defun set-point-before-yanking-if-in-text-mode (string)
  "Invoke `set-point-before-yanking' in text modes."
  (when (derived-mode-p 'text-mode)
    (set-point-before-yanking string)))

(advice-add
 'insert-for-yank
 :before
 #'set-point-before-yanking-if-in-text-mode)
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00975.html


;; Jump to sexp
(defun my-forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;;* Scratch Buffer Functions

;;** New scratch buffer with derived mode
(defun my-create-scratch-buffer (&optional nomode)
  "Create a new scratch buffer and switch to it. If the region is active, then
 paste the contents of the region in the new buffer. The new buffer inherits
 the mode of the original buffer unless nomode is set.
 Return the buffer."

  ;; https://gist.github.com/eev2/52edbfdb645e26aefec19226c0ca7ad0
  (interactive "P")
  (let (bufname (mjmode  major-mode) (paste (and (region-active-p) (prog1 (buffer-substring (mark t) (point)) (deactivate-mark)))))
    (if (and (not nomode) (boundp 'ess-dialect) ess-dialect)
        (setq mjmode (intern-soft (concat ess-dialect "-mode"))))
    (setq bufname (generate-new-buffer-name "*scratch*"))
    (switch-to-buffer (get-buffer-create bufname))
    (if paste (insert paste))
    (if (and (not nomode) mjmode) (ignore-errors (funcall mjmode)))
    (get-buffer bufname)))

;;** Bury, don't kill scratch buffer
(defun my--bury-scratch ()
  "Don't kill scratch buffer, bury it."
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil)
    t))
(add-hook 'kill-buffer-query-functions 'my--bury-scratch)

;;** Persist scratch buffer
(defun my--save-persistent-scratch ()
  "Save the contents of *scratch*."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max)
                  (concat my-library-dir "scratch"))))

(defun my--load-persistent-scratch ()
  "Reload the scratch buffer."
  (let ((scratch-file (concat my-library-dir "scratch")))
    (if (file-exists-p scratch-file)
        (with-current-buffer (get-buffer "*scratch*")
          (delete-region (point-min) (point-max))
          (insert-file-contents scratch-file))))
  ;; set default dir for scratch buffer
  (with-current-buffer (get-buffer "*scratch*")
    (setq-local default-directory my-var-dir)))

;; Hooks for loading and saving the scratch buffer
(add-hook 'after-init-hook 'my--load-persistent-scratch)
(add-hook 'kill-emacs-hook 'my--save-persistent-scratch)
;; Save scratch buffer every 5 minutes (300 seconds)
(run-with-idle-timer 300 t 'my--save-persistent-scratch)

;;* UI Functions
;; Toggle markup
(defun my-toggle-display-markup ()
  "Toggle the display of markup in markdown and org modes"
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-toggle-link-display)
    (if (bound-and-true-p markdown-hide-markup)
        (markdown-toggle-markup-hiding 0)
      (markdown-toggle-markup-hiding))))

;;* Tab functions
(defun my-move-tab-to ()
  "Prompt for where to move tab to, and move there.
Tab numbering starts at 1."
  (interactive)
  (tab-move-to (read-number "Move tab to:" 1)))

;;* Quit All the Things!
;; From a great vim migration guide by Juanjo Álvarez
;; https://juanjoalvarez.net/en/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
;; (original code from davvil) https://github.com/davvil/.emacs.d/blob/master/init.el

;; Quit Message Function
(defun my--quit-p (&optional prompt)
  "Return t if this session should be killed. Prompts the user for
      confirmation."
  (or (yes-or-no-p (format "››› %s" (or prompt "Quit Emacs?")))
      (ignore (message "Aborted"))))
(setq confirm-kill-emacs nil)
(defvar my-quit-messages
  '(;; from Doom
    "Let's beat it -- This is turning into a bloodbath!"
    "I wouldn't leave if I were you. DOS is much worse."
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    "Emacs! Emacs!! Emacs!!!"
    "The King is dead, long live the King!"
    "Like you have somewhere better to be..."
    "Don't worry, I won't tell everyone you're a failure"
    "Aus so krummem Holze, als woraus der Mensch gemacht ist, kann nichts ganz Gerades gezimmert werden"
    "Sed omnia praeclara tam difficilia, quam rara sunt"
    "You're leaving? Fine, I didn't like you anyway."
    "Come back soon, I'm running out of RAM!"
    "Why quit Emacs when you can have infinite windows with vim keybindings?"
    "No need to rage quit, just embrace the chaos."
    "Remember, timetables are just a suggestion."
    "Your cat will miss pouncing on all those dangling cables."
    "This is just a pause, Emacs is forever."
    "Quitting Emacs won't make your todo list shorter."
    "Alert: Emacs is always watching, waiting for your return."
    "Technically, you're still plugged in somewhere in the Matrix."
    "I knew you'd go back to notepad at the first sign of trouble!"
    "You're now allowed a very short break to grab coffee."
    "Is this the end, or merely an elaborate buffer switch?"
    "Emacs will remember that."
    "Emacs, Emacs never changes."
    "Hey! Hey, M-x listen!"
    "Okay, look. We've both said a lot of things you're going to regret..."
    "You are *not* prepared!")
  "A list of quit messages, picked randomly by `my-quit'. Taken from
      http://doom.wikia.com/wiki/Quit_messages and elsewhere.")

(defun my--quit (&rest _)
  (my--quit-p
   (format "%s  Quit?"
           (nth (random (length my-quit-messages))
                my-quit-messages))))

(add-hook 'kill-emacs-query-functions #'my--quit)

;;* Org functions

(defun unpackaged/org-element-descendant-of (type element)
  "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
  ;; MAYBE: Use `org-element-lineage'.
  (when-let* ((parent (org-element-property :parent element)))
    (or (eq type (car parent))
        (unpackaged/org-element-descendant-of type parent))))

(defun unpackaged/org-return-dwim (&optional default)
  "A helpful replacement for `org-return'.  With prefix, call `org-return'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
  ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (if default
      (org-return)
    (cond
     ;; Act depending on context around point.

     ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
     ;; followed.

     ;; ((eq 'link (car (org-element-context)))
     ;;  ;; Link: Open it.
     ;;  (org-open-at-point-global))

     ((org-at-heading-p)
      ;; Heading: Move to position after entry content.
      ;; NOTE: This is probably the most interesting feature of this function.
      (let ((heading-start (org-entry-beginning-position)))
        (goto-char (org-entry-end-position))
        (cond ((and (org-at-heading-p)
                    (= heading-start (org-entry-beginning-position)))
               ;; Entry ends on its heading; add newline after
               (end-of-line)
               (insert "\n\n"))
              (t
               ;; Entry ends after its heading; back up
               (forward-line -1)
               (end-of-line)
               (when (org-at-heading-p)
                 ;; At the same heading
                 (forward-line)
                 (insert "\n")
                 (forward-line -1))
               (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))
                                         (line-beginning-position)))
                 (insert "\n"))
               (forward-line -1)))))

     ((org-at-item-checkbox-p)
      ;; Checkbox: Insert new item with checkbox.
      (org-insert-todo-heading nil))

     ((org-in-item-p)
      ;; Plain list.  Yes, this gets a little complicated...
      (let ((context (org-element-context)))
        (if (or (eq 'plain-list (car context))  ; First item in list
                (and (eq 'item (car context))
                     (not (eq (org-element-property :contents-begin context)
                              (org-element-property :contents-end context))))
                (unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
            ;; Non-empty item: Add new item.
            (org-insert-item)
          ;; Empty item: Close the list.
          ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
          (delete-region (line-beginning-position) (line-end-position))
          (insert "\n"))))

     ((when (fboundp 'org-inlinetask-in-task-p)
        (org-inlinetask-in-task-p))
      ;; Inline task: Don't insert a new heading.
      (org-return))

     ((org-at-table-p)
      (cond ((save-excursion
               (beginning-of-line)
               ;; See `org-table-next-field'.
               (cl-loop with end = (line-end-position)
                        for cell = (org-element-table-cell-parser)
                        always (equal (org-element-property :contents-begin cell)
                                      (org-element-property :contents-end cell))
                        while (re-search-forward "|" end t)))
             ;; Empty row: end the table.
             (delete-region (line-beginning-position) (line-end-position))
             (org-return))
            (t
             ;; Non-empty row: call `org-return'.
             (org-return))))
     (t
      ;; All other cases: call `org-return'.
      (org-return)))))

;; search through an org file headings using a simple interface
(defun org-goto-interactive ()
  (interactive)
  (org-goto 'outline))

;;* Programming functions
;;** Lisp Helper Functions
;; Evaluates the current form by searching backward for its beginning.
(defun my-eval-current-form ()
  "Evaluate the current Lisp form by moving backward to its start and executing it."
  (interactive)
  (save-excursion
    (search-backward-regexp "(defun\\|(use-pack\\|(transient-\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

;;* Download helpers
;; I use this function to download MP3s for kids from youtube
(defun my-youtube-dl-urls-in-region (start end)
  "Download links from a selection in org (or any other text mode) into an MP3 output."
  (interactive "r")
  (let ((lines (split-string (buffer-substring-no-properties start end) "\n")))
    (mapc (lambda (line)
            (when (string-match
                   (rx (or "http" "https")
                       "://"
                       (+ (not (any " " "]" "," "\n"))))
                   line)
              (let ((url (match-string 0 line)))
                (when url
                  (let* ((song-name-dirty (string-trim (shell-command-to-string
                                                        (concat "yt-dlp --get-title "
                                                                (shell-quote-argument url)))))
                         ;; sanitize song name by removing characters
                         (song-name (replace-regexp-in-string
                                     (rx (any "!@#$%^&*()[]{}:;,<.>/?\\|'\"`~"))
                                     ""
                                     song-name-dirty)))
                    (when song-name
                      (shell-command (concat "yt-dlp --extract-audio --audio-format mp3 -o "
                                             (shell-quote-argument (concat "~/Downloads/" song-name ".mp3"))
                                             " "
                                             (shell-quote-argument url)))))))))
          lines)))

;;* provide my-setup-functions
(provide 'my-setup-functions)
