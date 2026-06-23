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
  ("C-k"   . crux-smart-kill-line)
  ("C-a"   . crux-move-beginning-of-line))

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
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

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
