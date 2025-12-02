;; my-functions.el  -*- lexical-binding: t -*-

(message "Setting up my helper functions...")



;;* Config Helper Functions
;;** Goto Functions
(defun my-goto-private ()
  "Open `private.el' in `my-user-dir'."
  (interactive)
  (find-file (concat my-user-dir "private.el")))

(defun my-goto-early-init.el ()
  "Open early-init.el."
  (interactive)
  (find-file "~/.emacs.d/early-init.el"))

(defun my-goto-init.el ()
  "Open init.el."
  (interactive)
  (find-file user-init-file))

(defun my-goto-custom.el ()
  "Open custom.el."
  (interactive)
  (find-file custom-file))

(defun my-goto-config ()
  "Open user config."
  (interactive)
  (find-file my-config-file))

(defun my-load-config ()
  "Load config."
  (interactive)
  (load-file user-init-file)
  (load-file my-config-file))

(defun my-goto-emacs-dir ()
  "Go to Emacs dir."
  (interactive)
  (find-file user-emacs-directory))

(defun my-goto-org-files ()
  "Go to org files dir."
  (interactive)
  (find-file org-directory))

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

(defun my-load-config-file ()
  "Load the user config file."
  (interactive)
  (load-file my-config-file))

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

;;* Package management

;; https://emacsredux.com/blog/2020/09/12/reinstalling-emacs-packages/
(defun my-reinstall-package (pkg)
  "Reinstall the Emacs package PKG.

First, prompts the user to select a package to reinstall from the list
of installed packages. Then unloads the currently loaded features of the
selected package, reinstalls the package, and finally requires the package
to ensure it is loaded into the current Emacs session.

PKG is the package symbol selected by the user."

  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

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
  (kill-matching-buffers "*vterm" nil t)
  (when (condition-case nil (delete-frame)
          (error (save-buffers-kill-emacs))))
  (select-frame-set-input-focus (selected-frame)))

;;* Window Functions
;; Toggle Dedicated Window
(defun my-dired-download-split-screen (&optional dir-name)
  "Create a vertical dual-window split with dired buffers.
Left window shows ~/Downloads/, right window shows DIR-NAME or the most
recently visited directory from `dired-recent-directories' if available."
  (interactive)
  (delete-other-windows)
  (let* ((downloads-dir (expand-file-name "~/Downloads/"))
         (default-directory downloads-dir)
         (right-dir (cond
                     (dir-name (expand-file-name dir-name))
                     ((and (boundp 'dired-recent-directories)
                           dired-recent-directories)
                      (expand-file-name (car dired-recent-directories)))
                     (t default-directory))))
    ;; Open Downloads in the left window
    (dired downloads-dir)
    ;; Split and open the right directory
    (split-window-right)
    (other-window 1)
    (dired right-dir)
    ;; Go back to left window
    (other-window 1)))

(defun my-toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Exchange Windows
;; Swap buffers in windows and leave the cursor in the original window. Courtesy of
;; Mike Zamansky's video.
;; http://cestlaz.github.io/posts/using-emacs-36-touch-of-elisp/#.WX5Wg0czpcx

(defun my-window-exchange-buffer ()
  "Swap buffer in windows and leave focus in original window"
  (interactive)
  (ace-swap-window)
  (aw-flip-window))

;; Rotate Windows
;; from magnars modified by ffevotte for dedicated windows support
(defun my-rotate-windows (count)
  "Rotate your windows.
  Dedicated windows are left untouched. Giving a negative prefix
  argument takes the kindows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))

                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun my-rotate-windows-backward (count)
  "Rotate your windows backward."
  (interactive "p")
  (my-rotate-windows (* -1 count)))

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

(defun my-toggle-window-split ()
  "Move from a horizontal to a vertical split and vice versa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Jump to Minibuffer Window
(defun my-goto-minibuffer-window ()
  "locate point to minibuffer window if it is active."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

;;* Buffer Functions
(defun my-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if (< line-spacing 0.2)
      (setq line-spacing 0.5)
    (setq line-spacing 0.1))
  (redraw-frame (selected-frame)))

(defun my-narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, markdown
  subtree, or defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive "P")
  (declare (interactive-only))
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

;; Create New Elisp Buffer
(defun my-create-new-elisp-buffer ()
  "Create a new buffer in `'emacs-lisp-mode'."
  (interactive)
  (let ((buffer (generate-new-buffer "*elisp*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (emacs-lisp-mode))))

;; Make Temp Buffer
(defun my-tmp-buffer()
  "Make a temporary buffer and switch to it."
  (interactive)
  (switch-to-buffer (get-buffer-create (concat "tmp-" (format-time-string "%m.%dT%H.%M.%S"))))
  (delete-other-windows))

;; Revert all buffers
(defun my-revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

;; Clipboard to/from Buffer
;; http://stackoverflow.com/a/10216338/4869
(defun my-copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun my-copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;; Useful Buffers

;; TODO: make this respect workspace buffers
(defun my-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
  Typically, if buffer name starts with *, it's not considered a user buffer.
  This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
  You can override this function to get your idea of “user buffer”.
  version 2016-06-18

Include: gptel, mu4e, and OrgMsg buffers in the user-buffer list"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

(defun my-next-user-buffer ()
  "Switch to the next user buffer.
  “user buffer” is determined by `my-user-buffer-q'.
  URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
  Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (my-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun my-previous-user-buffer ()
  "Switch to the previous user buffer.
  “user buffer” is determined by `my-user-buffer-q'.
  URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
  Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (my-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;; Eval emacs buffer until error

(defun my-eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

;; Kill Current Buffer
;; (kill-this-buffer) is unreliable when not invoked from the menubar. So here's a
;; wrapper on (kill-buffer) to kill the current buffer. This is sometimes better
;; than (evil-delete-buffer) since it keeps the window.

(defun my-kill-this-buffer ()
  (interactive)
  (kill-buffer))

;; Show Filename of Buffer
;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun my-show-and-copy-buffer-full-filename ()
  "Show the full path to the current file in the minibuffer and copy to clipboard."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun my-show-and-copy-buffer-filename ()
  "Show the abbreviated path to the current file in the minibuffer and copy to clipboard."
  (interactive)
  (let ((file-name (abbreviate-file-name buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;; Switch previous buffer

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;* File Functions
;; Delete Current File
(defun my-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; Get string from file
(defun my-get-string-from-file (filePath)
  "Read a file and return the contents as a string"
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; Duplicate file
;; Duplicate a file in dired or deer
(defun my-duplicate-file ()
  (interactive)
  (dired-do-copy-regexp "\\(.*\\)\\.\\(.*\\)" "\\1 (copy).\\2"))

;; Move File
(defun my-move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
    (call-interactively #'write-file)
    (when old-location
      (delete-file old-location))))

;;** Directory Functions
;;;;; Make Parent Directory
;;  Create a directory – or a hierarchy of them – while finding a file in a
;;  nonexistent directory. From mbork
;;  http://mbork.pl/2016-07-25_Making_directories_on_the_fly

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

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
      ;; (insert palimpsest-prefix)
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
      (palimpsest-move-region-to-dest start end 'point-max)
    (message "No region selected")))

(defun my-move-region-to-top (start end)
  "Move text between START and END to top of buffer."
  (interactive "r")
  (if (use-region-p)
      (palimpsest-move-region-to-dest start end 'point-min)
    (message "No region selected")))

(defun palimpsest-move-region-to-dest (start end dest)
  "Move text between START and END to buffer's desired position, otherwise known as DEST."
  (let ((count (count-words-region start end)))
    (save-excursion
      (kill-region start end)
      (goto-char (funcall dest))
      (yank)
      (newline))
    (push-mark (point))
    (message "Moved %s words" count)))

;; Fill Paragraph
(defun my-fill-paragraph ()
  "if in an org buffer use org-fill-paragraph; else use fill-paragraph"
  (interactive)
  (if (derived-mode-p 'org-mode)
      (call-interactively #'my-org-fill-paragraph)
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

;; Formatted Copy
(defun my-formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))

;; Clipboard Transforms Using Pandoc
(defun my-org-to-markdown ()
  "convert clipboard contents from org to markdown and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t markdown"))
  (yank))

(defun my-markdown-to-org ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f markdown -t org"))
  (yank))

(defun my-tex-to-org ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f latex -t org"))
  (yank))

(defun my-org-to-tex ()
  "convert clipboard contents from org to tex and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t latex"))
  (yank))

(defun my-tex-to-markdown ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f latex -t markdown --markdown-headings=atx"))
  (yank))

(defun my-markdown-to-tex ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f markdown -t latex"))
  (yank))

(defun my-cite-to-org ()
  "convert clipboard contents from markdown to org with citations and paste"
  (interactive)
  (kill-new (shell-command-to-string (concat "pbpaste | pandoc --bibliography=" my-bibliography " -s -t markdown-native_divs-raw_html-citations | pandoc -f markdown -t org")))
  (yank))

(defun my-cite-to-markdown ()
  "convert clipboard contents to markdown with citations and paste"
  (interactive)
  (kill-new (shell-command-to-string (concat "pbpaste | pandoc --bibliography=" my-bibliography " -s -t markdown-native_divs-raw_html-citations --markdown-headings=atx")))
  (yank))

(defun my-bibtex-to-yaml-reference ()
  "convert clipboard bibtex contents to yaml and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc-citeproc -y -f bibtex | pbcopy"))
  (yank))

(defun my-md-to-rtf ()
  "convert md to rtf and send to clipboard"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f markdown -t rtf | pbcopy"))
  (yank))

(defun my-md-to-org ()
  "convert md to org and send to clipboard"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f markdown -t org | pbcopy"))
  (yank))

(defun my-org-to-rtf ()
  "convert org to rtf and send to clipboard"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t html | /usr/bin/textutil -stdin -stdout -format html -convert rtf -fontsize 14 -font Helvetica | pbcopy")))

(defun my-org-to-mail-rtf ()
  "copy buffer, convert clipboard contents from org to rtf, and send to mail message"
  (interactive)
  (my-copy-whole-buffer-to-clipboard)
  ;; (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t rtf"))
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t html | /usr/bin/textutil -stdin -stdout -format html -convert rtf -fontsize 14 | pbcopy"))
  (kill-buffer)
  (delete-frame)
  (do-applescript "if application \"Mail\" is running then
      tell application \"Mail\"
      activate
      delay 0.35
      tell application \"System Events\"
      keystroke \"v\" using {command down}
      end tell
      end tell
      end if"))

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
                  (concat my-var-dir "scratch"))))

(defun my--load-persistent-scratch ()
  "Reload the scratch buffer."
  (let ((scratch-file (concat my-var-dir "scratch")))
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
    (if markdown-hide-markup
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

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
      In Delete Selection mode, if the mark is active, just deactivate it;
      then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; Quit Message Function
(defun my--quit-p (&optional prompt)
  "Return t if this session should be killed. Prompts the user for
      confirmation."
  (or (yes-or-no-p (format "››› %s" (or prompt "Quit Emacs?")))
      (ignore (message "Aborted"))))
(setq confirm-kill-emacs nil)
(add-hook 'kill-emacs-query-functions #'my--quit-p)
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

(remove-hook 'kill-emacs-query-functions #'my--quit-p)
(add-hook 'kill-emacs-query-functions #'my--quit)

;;* Org functions
(defun my-org-table-kill-cell-text ()
  "Copy the content of the current org-mode table cell to the kill ring."
  (interactive)
  (when (org-at-table-p)
    (kill-new
     (string-trim
      (substring-no-properties(org-table-get-field))))
    (message "copied cell: @%d$%d"
             (org-table-current-line)
             (org-table-current-column) )))

;; reset all checkboxes in a narrowed org buffer
;; https://mbork.pl/2025-07-07_Mass_resetting_Org_mode_checkboxes
(defun my-org-reset-checkbox-state-buffer ()
  "Reset all checkboxes in the (narrowed portion of) the buffer."
  (interactive "*")
  (org-map-entries #'org-reset-checkbox-state-subtree
                   t nil
                   'archive 'comment))
;; build daily agendas using sub-trees
(defun my-copy-org-subtree-to-daily-agenda (day)
  "Copy the current org-mode subtree and paste it under the 'Agenda' heading in the org-roam daily agenda file specified by 'day'. Day is computed based on today"
  ;; TODO: finish this function, test changes
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-copy-subtree)
    (save-excursion
      (org-roam-dailies-goto-tomorrow 1)
      (let ((agenda-heading (org-find-exact-headline-in-buffer "Agenda")))
        (if agenda-heading
            (goto-char agenda-heading)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (org-insert-heading "Agenda" nil t)
          ;; (insert "* Agenda\n")
          ;; (forward-line -1)
          ))
      (org-end-of-subtree t t)
      (org-paste-subtree 2) ;; paste as a level 2 heading
      (save-buffer))))

(defun my-copy-org-subtree-to-tomorrow-agenda ()
  "Copy the current org-mode subtree and paste it under the 'Agenda' heading in the org-roam tomorrow file specified by date."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-copy-subtree)
    (save-excursion
      (org-roam-dailies-goto-tomorrow 1)
      (let ((agenda-heading (org-find-exact-headline-in-buffer "Agenda")))
        (if agenda-heading
            (goto-char agenda-heading)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (org-insert-heading "Agenda" nil t)))
      (org-end-of-subtree t t)
      ;; (insert "\n" content)
      ;; (forward-line -1)
      (org-paste-subtree 2) ;; paste as a level 2 heading
      (save-buffer))))

(defun my-copy-org-subtree-to-today-agenda ()
  "Copy the current org-mode subtree and paste it under the 'Agenda' heading in today's org-roam daily file."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-copy-subtree)
    (let ((content (car kill-ring)))
      (org-roam-dailies-goto-today)
      (let ((agenda-heading (org-find-exact-headline-in-buffer "Agenda")))
        (if agenda-heading
            (goto-char agenda-heading)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "* Agenda\n")
          (forward-line -1)))
      (org-end-of-subtree t t)
      ;; (insert "\n" content)
      (forward-line -1)
      (org-paste-subtree 2)
      (save-buffer))))

;; https://github.com/alphapapa/unpackaged.el
(define-minor-mode unpackaged/org-table-face-mode
  "Apply `org-table' face family to all text in Org tables.
Useful for forcibly applying the face to portions of table data
that might have a different face, which could affect alignment."
  :global nil
  (let ((keywords '((unpackaged/org-table-face-matcher 0 'org-table))))
    (if unpackaged/org-table-face-mode
        (font-lock-add-keywords nil keywords 'append)
      (font-lock-remove-keywords nil keywords))
    (font-lock-flush)))

(cl-defun unpackaged/org-table-face-matcher
    (limit &optional (face `(:family ,(face-attribute 'org-table :family))))
  "Apply FACE to entire Org tables.
A `font-lock-keywords' function that searches up to LIMIT."
  (cl-flet* ((find-face (face &optional limit not)
                        ;; Return next position up to LIMIT that has FACE, or doesn't if NOT.
                        (cl-loop with prev-pos
                                 with pos = (point)
                                 while (not (eobp))
                                 do (setf pos (next-single-property-change pos 'face nil limit))
                                 while (and pos (not (equal pos prev-pos)))
                                 for face-at = (get-text-property pos 'face)
                                 for face-matches-p = (or (eq face-at face)
                                                          (when (listp face-at)
                                                            (member face face-at)))
                                 when (or (and not (not face-matches-p))
                                          face-matches-p)
                                 return pos
                                 do (setf prev-pos pos)))
             (apply-face-from (pos face)
                              (unless (eobp)
                                (let* ((property-at-start (get-text-property pos 'face))
                                       (table-face-start (if (or (eq property-at-start 'org-table)
                                                                 (when (listp property-at-start)
                                                                   (member 'org-table property-at-start)))
                                                             (point)
                                                           (find-face 'org-table limit)))
                                       table-face-end)
                                  (when table-face-start
                                    (goto-char table-face-start)
                                    (setf table-face-end (line-end-position))
                                    (add-face-text-property table-face-start table-face-end face)
                                    (goto-char table-face-end))))))
    (cl-loop with applied-p
             for applied = (apply-face-from (point) face)
             when applied
             do (setf applied-p t)
             while applied
             finally return applied-p)))

(defun unpackaged/org-attach-download (url)
  "Download file at URL and attach with `org-attach'.
Interactively, look for URL at point, in X clipboard, and in
kill-ring, prompting if not found.  With prefix, prompt for URL."
  (interactive (list (if current-prefix-arg
                         (read-string "URL: ")
                       (or (org-element-property :raw-link (org-element-context))
                           (org-web-tools--get-first-url)
                           (read-string "URL: ")))))
  (when (yes-or-no-p (concat "Attach file at URL: " url))
    (let* ((temp-dir (make-temp-file "org-attach-download-" 'dir))
           (basename (file-name-nondirectory (directory-file-name url)))
           (local-path (expand-file-name basename temp-dir))
           size)
      (unwind-protect
          (progn
            (url-copy-file url local-path 'ok-if-exists 'keep-time)
            (setq size (file-size-human-readable
                        (file-attribute-size
                         (file-attributes local-path))))
            (org-attach-attach local-path nil 'mv)
            (message "Attached %s (%s)" url size))
        (delete-directory temp-dir)))))

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
               ;; FIXME: looking-back is supposed to be called with more arguments.
               (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))))
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

(defun my-find-elisp-thing-at-point-other-window ()
  "Find thing under point and go to it another window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb
             (not (equal symb 0))
             (not (fboundp symb)))
        (find-variable-other-window symb)
      (find-function-at-point))))
;;** Fix Parentheses
(defun my-fix-lonely-parens ()
  "Move all closing parenthesis at start of indentation to previous line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*)" nil t)
      (delete-indentation))))

;; ref: https://xenodium.com/deleting-from-emacs-sequence-vars/
(defun my-remove-from-list-variable ()
  "Remove an item from a list using completing-read"
  (interactive)
  (let* ((var (intern
               (completing-read "From variable: "
                                (let (symbols)
                                  (mapatoms
                                   (lambda (sym)
                                     (when (and (boundp sym)
                                                (seqp (symbol-value sym)))
                                       (push sym symbols))))
                                  symbols) nil t)))
         (values (mapcar (lambda (item)
                           (setq item (prin1-to-string item))
                           (concat (truncate-string-to-width
                                    (nth 0 (split-string item "\n"))
                                    (window-body-width))
                                   (propertize item 'invisible t)))
                         (symbol-value var)))
         (index (progn
                  (when (seq-empty-p values) (error "Already empty"))
                  (seq-position values (completing-read "Delete: " values nil t)))))
    (unless index (error "Eeek. Something's up."))
    (set var (append (seq-take (symbol-value var) index)
                     (seq-drop (symbol-value var) (1+ index))))
    (message "Deleted: %s" (truncate-string-to-width
                            (seq-elt values index)
                            (- (window-body-width) 9)))))


;;* Git functions
;;** New Git Project
(defun my-git-new-project ()
  "Initializes a new git repo and adds it to project.el's known projects."
  (interactive)
  (let ((project-dir (expand-file-name
                      (read-directory-name "New project root:"))))
    (magit-init project-dir)
    (setq default-directory project-dir)
    ;; make sure project.el remembers new project
    (let ((pr (project--find-in-directory default-directory)))
      (project-remember-project pr))))

;;** Clone a Git Repo from Clipboard
;; http://xenodium.com/emacs-clone-git-repo-from-clipboard/
(defun my-git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finishe.
  Git repo is cloned to directory set by `my-user-elisp-dir'."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir my-user-elisp-dir)
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

;;* Marking text in org-mode and other buffers
;; written with LLM, hope it works. yolo
(defun my-mark-inside-org-block ()
  "Mark the inside of any org block when the cursor is inside or on the block delimiter."
  (interactive)
  (let* ((element (org-element-context))
         (type (org-element-type element))
         ;; If we're inside a block element, check the parent
         (block-element (cond
                         ;; If current element is already a block, use it
                         ((memq type '(src-block example-block quote-block center-block
                                                 special-block export-block verse-block))
                          element)
                         ;; Otherwise check if parent is a block
                         ((memq (org-element-type (org-element-property :parent element))
                                '(src-block example-block quote-block center-block
                                            special-block export-block verse-block))
                          (org-element-property :parent element))
                         ;; No block found
                         (t nil))))
    (when block-element
      (let ((contents-begin (org-element-property :contents-begin block-element))
            (contents-end (org-element-property :contents-end block-element)))
        (when (and contents-begin contents-end)
          (goto-char contents-begin)
          (set-mark contents-begin)
          (goto-char contents-end))))))


;; old version doesn't work on quote blocks.
;; (defun my-mark-inside-org-block ()
;;   "Mark the inside of any org block when the cursor is inside or on the block delimiter."
;;   (interactive)
;;   (when-let* ((element (org-element-context))
;;               (type (org-element-type element))
;;               ;; Check if element is a block with contents
;;               ((memq type '(src-block example-block quote-block center-block
;;                                       special-block export-block verse-block)))
;;               (begin (org-element-property :begin element))
;;               (end (org-element-property :end element)))
;;     (goto-char begin)
;;     (forward-line 1)
;;     (set-mark (point))
;;     (goto-char end)
;;     (forward-line -2)))


;;* Download helpers
;; I use this function to download MP3s for kids from youtube
(defun my-youtube-dl-urls-in-region (start end)
  "Download links from a selection in org (or any other text mode) into an MP3 output."
  (interactive "r")
  (let ((lines (split-string (buffer-substring-no-properties start end) "\n")))
    (mapc (lambda (line)
            (string-match
             (rx (or "http" "https")
                 "://"
                 (+ (not (any " " "]" "," "\n"))))
             line)
            ;; "\\(http\\|https\\)://[^ ]+" line)
            (let ((url (match-string 0 line)))
              (when url
                (let* ((song-name-dirty (string-trim (shell-command-to-string
                                                      (concat "yt-dlp --get-title '"
                                                              url
                                                              "'"))))
                       ;; sanitize song name be removing characters
                       (song-name (replace-regexp-in-string
                                   (rx (any "!@#$%^&*()[]{}:;,<.>/?\\|'\"`~"))
                                   ""
                                   song-name-dirty)))
                  (when song-name
                    (shell-command (concat "yt-dlp --extract-audio --audio-format mp3 -o '~/Downloads/"
                                           song-name
                                           ".mp3' "
                                           "'" url "'"))))
                )))
          lines)))

;;* JSON parsers and integration
(defun my-compose-email-from-json (&optional str)
  "Create a new mu4e email from JSON in optional str argument, selected region, or {} pair around cursor.
JSON can have 'To', 'Subject', 'Body', 'Cc', and 'Bcc' fields.

Check to see if org-msg is enabled, and if so, use appropriate commands to jump to message body."
  (interactive)
  (let* ((json-string (or str ;; JSON string passed to function?
                          ;; check if a region is selected
                          (if (region-active-p)
                              (buffer-substring-no-properties (region-beginning) (region-end))
                            (save-excursion
                              (when (search-backward "{" nil t)
                                (let* ((start (point))
                                       (end (search-forward "}" nil t)))
                                  (buffer-substring-no-properties start end)))))))
         (json-obj (condition-case nil
                       (json-read-from-string json-string)
                     (error nil)))
         (to (when json-obj (cdr (assoc 'To json-obj))))
         (subject (when json-obj (cdr (assoc 'Subject json-obj))))
         (body (when json-obj (cdr (assoc 'Body json-obj))))
         (cc (when json-obj (cdr (assoc 'Cc json-obj))))
         (bcc (when json-obj (cdr (assoc 'Bcc json-obj)))))
    
    ;; Debug output
    (message "JSON string: %s" json-string)
    (message "Parsed JSON: %s" json-obj)
    ;; (message "To: %s" to)
    
    ;; Create new message and fill fields
    (with-current-buffer (mu4e-compose-new)
      (when (and to (not (string-empty-p to)))
        (message-goto-to)
        (insert to))
      
      (when (and cc (not (string-empty-p cc)))
        (message-goto-cc)
        (insert cc))

      (when (and bcc (not (string-empty-p bcc)))
        (message-goto-bcc)
        (insert bcc))
      
      (when (and subject (not (string-empty-p subject)))
        (message-goto-subject)
        (insert subject))
      
      (when (and body (not (string-empty-p body)))
        (if org-msg-mode
            (org-msg-goto-body)
          (message-goto-body))
        (insert body)))))

;;* provide my-setup-functions
(provide 'my-setup-functions)
