;; my-dired.el -*- lexical-binding: t -*-


;;* Dired Settings
(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-jump-other-window)
  :bind (:map dired-mode-map
              ("l" . dired-find-alternate-file)
              ("h" . my-dired-updirectory)
              ("E" . gnus-dired-attach)
              ("G" . gptel-add)
              ("O" . org-attach-dired-to-subtree)
              ("S" . xah-open-in-external-app))
  :custom
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
  :config
  ;; Function to move up a directory like in ranger
  (defun my-dired-updirectory ()
    (interactive)
    (find-alternate-file ".."))

  (when sys-mac
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)
    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and sys-mac (executable-find "gls"))
            (and (not sys-mac) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)
    ;; list directories first 
    (setq dired-listing-switches "-laFGh1v --group-directories-first")))


;;** Dired Helper Functions
;; https://emacs.dyerdwelling.family/emacs/20240918092253-emacs--adding-disk-usage-reporting-to-emacs-dired-mode/
;; report disk usage for a directory under cursor
(defun my-dired-du ()
  "Run 'du -hc' on the directory under the cursor in Dired."
  (interactive)
  (let ((current-dir (dired-get-file-for-visit)))
    (if (file-directory-p current-dir)
        (dired-do-async-shell-command "du -hc" nil (list current-dir))
      (message "The current point is not a directory."))))

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

(defun my-dired-rename-marked-files-add-date ()
  "Add a date to marked files in dired buffer.
Check to make sure in dired major mode."
  (interactive)
  (if (or (eq major-mode 'dired-mode) (eq major-mode 'dirvish-mode))
      (progn
        (let* ((files (dired-get-marked-files)))
          (dolist (file files)
            (let* ((file-dir (file-name-directory file))
                   (file-name (file-name-nondirectory file))
                   (name (file-name-sans-extension file-name))
                   (ext (file-name-extension file-name))
                   (new-name (concat file-dir (add-todays-date-remove-old name) "." ext)))
              (rename-file file new-name))))))
  (message "Not in Dired mode."))

(defun my-dired-copy-marked-files-add-date ()
  "Copy marked file(s) and add today's date in dired buffer.
Check to make sure in dired major mode."
  (interactive)
  (if (or (eq major-mode 'dired-mode) (eq major-mode 'dirvish-mode))
      (progn
        (let* ((files (dired-get-marked-files)))
          (dolist (file files)
            (let* ((file-dir (file-name-directory file))
                   (file-name (file-name-nondirectory file))
                   (name (file-name-sans-extension file-name))
                   (ext (file-name-extension file-name))
                   (new-name (concat file-dir (add-todays-date-remove-old name) "." ext)))
              (copy-file file new-name)))))
    (message "Not in Dired mode.")))

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
              (list (buffer-file-name)))))
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
  ;; squash the annoying keybinding
  :bind (("C-x C-d" . my-dired-recent-insert)
         :map dired-recent-mode-map
         ("C-x C-d" . my-dired-recent-insert))
  :config
  (defun my-dired-recent-insert ()
    "insert a string with the selected directory from the dired-recent-directories"
    (interactive)
    (unless dired-recent-directories
      (dired-recent-load-list))
    (let* ((selectrum-should-sort nil)
           (label (completing-read "Dired recent: " dired-recent-directories))
           (res (or (get-text-property
                     0 'dired-recent-restore-file-list
                     ;; Get from original string stored in list, completing-read
                     ;; strips the properties.
                     (car (member label dired-recent-directories)))
                    label)))
      (insert res)))

  (dired-recent-mode 1))

;;** Narrow Dired to Match Filter
(use-package dired-hacks
  :vc (:url "https://github.com/Fuco1/dired-hacks")
  :after (dired)
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;;** Dired Sort
;; not using--erase if never need
(use-package dired-quick-sort
  :disabled t
  :bind (:map dired-mode-map
              ("s" . hydra-dired-quick-sort/body)))

;;** Dired Colors
(use-package diredfl
:hook (dired-mode . diredfl-global-mode))

;;** Dired preview
;; I disabled this bc it was getting annoying
;; still useful to keep around so I can enable on a per-buffer basis
(use-package dired-preview
:config
(setq dired-preview-delay 1) ;; in sec
(setq dired-preview-max-size (expt 2 20))
(setq dired-preview-ignored-extensions-regexp
      (concat "\\."
              "\\(gz\\|"
              "zst\\|"
              "tar\\|"
              "xz\\|"
              "rar\\|"
              "zip\\|"
              "iso\\|"
              "epub"
              "\\)"))
;; Enable `dired-preview-mode' in a given Dired buffer or do it
;; globally:
;; (dired-preview-global-mode 1)
)

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

;;** Cycle Dired Buffer
;;https://www.reddit.com/r/emacs/comments/qnthhw/comment/hjiv2uc/?utm_source=share&utm_medium=web2x&context=3
;; Allow for cycling from bottom to top of dired buffer and vice versa
(add-hook 'dired-mode-hook
          (defun my-dired-wrap ()
            "Cycle from bottom to top of buffer"
            (make-local-variable 'post-command-hook)
            (add-hook 'post-command-hook
                      (defun my-dired-wrap-1 ()
                        ""
                        (if (= 1 (save-excursion
                                   (forward-line)))
                            (goto-line 3))
                        (if (= -1 (save-excursion
                                    (forward-line -1)))
                            (goto-line (count-lines
                                        (point-min)
                                        (point-max))))))))

;;* Dirvish (an improved Dired)
;; original author repo not updated, using doom fork
(use-package dirvish
  :disabled t
  :vc (:url "https://github.com/hlissner/dirvish")
  :bind (:map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
              ("a"   . dirvish-quick-access)
              ("f"   . dirvish-file-info-menu)
              ("y"   . dirvish-yank-menu)
              ("N"   . dirvish-narrow)
              ;; ("^"   . dirvish-history-last) ;; return the dired-up-directory command back
              ("h"   . dirvish-history-jump) ; remapped `describe-mode'
              ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
              ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
              ("TAB" . dirvish-subtree-toggle)
              ("M-f" . dirvish-history-go-forward)
              ("M-b" . dirvish-history-go-backward)
              ("M-l" . dirvish-ls-switches-menu)
              ("M-m" . dirvish-mark-menu)
              ("M-t" . dirvish-layout-toggle)
              ("M-s" . dirvish-setup-menu)
              ("M-e" . dirvish-emerge-menu)
              ("M-j" . dirvish-fd-jump)))

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
;;; my-dired.el ends here
