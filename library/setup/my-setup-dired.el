;; my-setup-dired.el -*- lexical-binding: t -*-

;;* Async file operations
;; Run dired copy/rename/etc. asynchronously. Re-homed here from a deleted module.
(use-package async
  :defer 1
  :config
  (dired-async-mode 1))


;;* Dired Settings
(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-jump-other-window)
  :bind (:map dired-mode-map
              ("M-<up>"   . my-dired-drag-item-up)
              ("M-<down>" . my-dired-drag-item-down)
              ("l" . dired-find-alternate-file)
              ("h" . my-dired-updirectory)
              ("E" . gnus-dired-attach)
              ("G" . gptel-add)
              ("O" . org-attach-dired-to-subtree)
              ("S" . xah-open-in-external-app)
              ("TAB" . dired-cycle-dired-windows))
  :custom
  ;; compress to .zip files by default with dired "Z" command (otherwise its .tar.gz)
  (dired-compress-directory-default-suffix ".zip")
  ;; make sure git knows when a file is renamed
  (dired-vc-rename-file t)
  ;; Like with ls, append "@" to file names if they're symlinks
  (dired-ls-F-marks-symlinks t)
  ;; don't ask about killing buffer visiting file
  (dired-clean-confirm-killing-deleted-buffers nil)
  ;; always delete and copy recursively
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-dwim-target t)
  ;; allow editing file permissions
  (wdired-allow-to-change-permissions t)
  ;; open PDF files in external viewer
  (dired-guess-shell-alist-user '(("\.pdf$" . default)))
  ;; reuse the same buffer when opening a new directory
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t)
  ;; drag files from dired into other programs (!!!)
  (dired-mouse-drag-files t)
  ;; Cycle from bottom to top of buffer and vice versa (Emacs 30 built-in)
  (dired-movement-style 'cycle)
  :config
  ;; Cycle between dired windows like an orthodox file manager (tere-style)
  ;; ref: https://mbork.pl/2026-04-13_Binding_TAB_in_Dired_to_something_useful
  (defun dired-cycle-dired-windows ()
    "Switch to the next Dired window in the selected frame."
    (interactive)
    (let ((dired-windows (seq-filter
                          (lambda (window)
                            (eq (buffer-local-value
                                 'major-mode (window-buffer window))
                                'dired-mode))
                          (window-list))))
      (if (cdr dired-windows)
          (select-window (cadr dired-windows))
        (user-error "No other Dired window to cycle to"))))

  ;; Function to move up a directory like in ranger
  (defun my-dired-updirectory ()
    (interactive)
    (find-alternate-file ".."))

  (when sys-mac
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil))
  ;; Use GNU ls as `gls' from `coreutils' when available (needed on macOS for
  ;; --group-directories-first); fall back to the system ls otherwise.
  (if (executable-find "gls")
      (setq insert-directory-program "gls"
            dired-listing-switches "-laFGh1v --group-directories-first")
    (setq dired-listing-switches "-laFGh1v")))


;;** Dired Helper Functions

;; interactive way of reordering dired items. ref:
;; https://xenodium.com/interactive-ordering-of-dired-items

(defun my-dired-drag-item-up ()
  "Drag dired item up in buffer."
  (interactive)
  (unless (dired-get-filename nil t)
    (error "Not a dired draggable item"))
  (when (= (line-number-at-pos) 2)
    (error "Already at top"))
  (let* ((inhibit-read-only t)
         (col (current-column))
         (item-start (line-beginning-position))
         (item-end (1+ (line-end-position)))
         (item (buffer-substring item-start item-end)))
    (delete-region item-start item-end)
    (forward-line -1)
    (beginning-of-line)
    (insert item)
    (forward-line -1)
    (move-to-column col)))

(defun my-dired-drag-item-down ()
  "Drag dired item down in buffer."
  (interactive)
  (unless (dired-get-filename nil t)
    (error "Not a dired draggable item"))
  (when (save-excursion
          (forward-line 1)
          (eobp))
    (error "Already at bottom"))
  (let* ((inhibit-read-only t)
         (col (current-column))
         (item-start (line-beginning-position))
         (item-end (1+ (line-end-position)))
         (item (buffer-substring item-start item-end)))
    (delete-region item-start item-end)
    (forward-line 1)
    (beginning-of-line)
    (insert item)
    (forward-line -1)
    (move-to-column col)))



(defun add-todays-date-remove-old (str)
  "Remove any date in the format YYYYMMDD from the end of STR, then add today's date.
   If the date that was removed was today's date, add a lowercase letter as well."
  (let* ((today (format-time-string "%Y%m%d"))
         (old-date-regex "\\([0-9]\\{8\\}\\)\\([a-z]\\)?\\'")
         (today-date-regex (concat "\\(" today "\\)\\([a-z]\\)?\\'")))
    (cond
     ((string-match today-date-regex str)
      (let* ((letter (match-string 2 str))
             (next-letter (if letter
                              (char-to-string (1+ (string-to-char letter)))
                            "a")))
        (replace-regexp-in-string today-date-regex (concat today next-letter) str)))
     ((string-match old-date-regex str)
      (replace-regexp-in-string old-date-regex today str))
     (t (concat str "." today)))))

(defun my-dired--operate-marked-files-add-date (operation)
  "Apply OPERATION to each marked file, adding today's date to its name.
OPERATION is a function of two args (OLD-NAME NEW-NAME), e.g.
`rename-file' or `copy-file'. Only runs in Dired/Dirvish buffers."
  (if (or (eq major-mode 'dired-mode) (eq major-mode 'dirvish-mode))
      (dolist (file (dired-get-marked-files))
        (let* ((file-dir (file-name-directory file))
               (file-name (file-name-nondirectory file))
               (name (file-name-sans-extension file-name))
               (ext (file-name-extension file-name))
               (new-name (concat file-dir (add-todays-date-remove-old name) "." ext)))
          (funcall operation file new-name)))
    (message "Not in Dired mode.")))

(defun my-dired-rename-marked-files-add-date ()
  "Rename marked file(s) in dired buffer, adding today's date."
  (interactive)
  (my-dired--operate-marked-files-add-date #'rename-file))

(defun my-dired-copy-marked-files-add-date ()
  "Copy marked file(s) in dired buffer, adding today's date."
  (interactive)
  (my-dired--operate-marked-files-add-date #'copy-file))

(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired/dirvish-marked files in external app.
When called in emacs lisp, if @fname is given, open that.
URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2022-09-14"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (or (string-equal major-mode "dired-mode") (string-equal major-mode "dirvish-mode"))
                (dired-get-marked-files)
              ;; `buffer-file-name' returns nil for buffers that don't visit a
              ;; file (e.g. *mu4e-headers*, *Messages*, *scratch*).  The original
              ;; code did (list (buffer-file-name)) unconditionally, which put nil
              ;; into the list and then crashed with "wrong-type-argument: arrayp,
              ;; nil" when shell-quote-argument tried to quote it.  The `when'
              ;; guard makes $file-list nil in that case, so the (when $do-it-p …)
              ;; block below simply does nothing.
              (when (buffer-file-name) (list (buffer-file-name))))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                       (start-process "" nil "xdg-open" $fpath))) $file-list))))))

;;** Dired-Recent to keep track of recently visited folders
(use-package dired-recent
  :after dired
  :config
  (defun my-dired-recent-insert ()
    "insert a string with the selected directory from the dired-recent-directories"
    (interactive)
    (unless dired-recent-directories
      (dired-recent-load-list))
    (let* ((label (completing-read "Dired recent: " dired-recent-directories))
           (res (or (get-text-property
                     0 'dired-recent-restore-file-list
                     ;; Get from original string stored in list, completing-read
                     ;; strips the properties.
                     (car (member label dired-recent-directories)))
                    label)))
      (insert res)))

  (dired-recent-mode 1))

;;** Narrow Dired to Match Filter
(use-package dired-narrow
  :after (dired)
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;;** Dired Colors
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;;** Dired preview
;; I disabled this bc it was getting annoying
;; still useful to keep around so I can enable on a per-buffer basis
(use-package dired-preview
  :commands (dired-preview-mode dired-preview-global-mode)
  :custom
  (dired-preview-delay 1) ;; in sec
  (dired-preview-max-size (expt 2 20))
  (dired-preview-ignored-extensions-regexp
   (concat "\\."
           (regexp-opt '("gz" "zst" "tar" "xz" "rar" "zip" "iso" "epub")))))

;;** Dired Ranger
;; https://github.com/Fuco1/dired-hacks#dired-ranger
;; Very helpful way of copying/moving files
;; Note that to move first you need to copy the file and then go to the target directory and move
(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
              ("s-c"  . dired-ranger-copy)
              ("s-m"  . dired-ranger-move)
              ("s-v"  . dired-ranger-paste)))

;;* Other file management packages
;; Open, view, browse, restore or permanently delete trashed files
;; or directories in trash can with Dired-like look and feel.
(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(provide 'my-setup-dired)
;;; my-setup-dired.el ends here
