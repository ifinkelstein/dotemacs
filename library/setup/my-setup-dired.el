;;; my-dired.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein

;;; Commentary:
;; See https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer for discussion
;; of how to avoid creating lots of dired buffers.

;;; Code:

;;;; Dired Settings
(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-jump-other-window)
  :bind (:map dired-mode-map
         ("l" . dired-find-alternate-file)
         ("h" . my-dired-updirectory)
         ("E" . gnus-dired-attach)
         ("O" . org-attach-dired-to-subtree)
         ("S" . xah-open-in-external-app))
  ;; "q" #'quit-window)
  :custom
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
  ;; new variable in emacs 28.1 to reuse the same buffer when opening a new directory
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


;;;; Dired Helper Functions
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
  (if (eq major-mode 'dired-mode)
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
  (if (eq major-mode 'dired-mode)
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
;;;; Dired-Recent to keep track of recently visited folders
;; TODO: move to :bind to be more canonical
(use-package dired-recent
  :after dired
  :commands (dired-recent-open)
  :config
  (dired-recent-mode 1)
  (define-key dired-recent-mode-map (kbd "C-x C-d") nil))

;;;; Narrow Dired to Match Filter
(use-package dired-narrow
  :bind (:map dired-mode-map
         ("/" . dired-narrow)))

;;;; Dired Sort
(use-package dired-quick-sort
  :bind (:map dired-mode-map
         ("s" . hydra-dired-quick-sort/body)))

;;;; Dired Colors
(use-package diredfl
  :hook (dired-mode . diredfl-global-mode))

;;;; Peep Dired
(use-package peep-dired
  :commands (peep-dired)
  :bind* (:map dired-mode-map
          ("P" . peep-dired)
          :map peep-dired-mode-map
          ("j"    . peep-dired-next-file)
          ("k"    . peep-dired-prev-file)
          ("RET"  . my-peep-dired-open)
          ("TAB"  . my-other-window))
  :config
  ;; helper function for opening files in full window
  (defun my-peep-dired-open ()
    "open files from peep-dired & clean-up"
    (interactive)
    (peep-dired-kill-buffers-without-window)
    (dired-find-file)
    (delete-other-windows))
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "pdf" "gif"))
  (setq peep-dired-cleanup-eagerly nil)
  (setq peep-dired-enable-on-directories t)
  (setq peep-dired-cleanup-on-disable t))

;;;; Dired Ranger
;; https://github.com/Fuco1/dired-hacks#dired-ranger
;; Very helpful way of copying/moving files
;; Note that to move first you need to copy the file and then go to the target directory and move
(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
         ("s-c"  . dired-ranger-copy)
         ("s-m"  . dired-ranger-move)
         ("s-v"  . dired-ranger-paste)))

;;;; Cycle Dired Buffer
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

;;;; Dirvish (an improved Dired)
(use-package dirvish
  :vc (dirvish :url "https://github.com/alexluigit/dirvish"
               :lisp-dir "extensions/")
  :after dired
  :init
  (require 'dirvish)
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a :custom option
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("w" "~/work/"                     "Work")
     ("t" "~/work/projects/" "Projects")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (setq dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  (setq dired-listing-switches "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

  ;; register a plain text dirvish dispatcher for docx files
  (dirvish-define-preview docx (file ext)
    "Preview docx files in plain text
 Require: `pandoc' (executable)"
    :require ("pandoc" )
    (cond ((equal ext "docx") `(shell . ("pandoc" "-t" "plain",file)))))
  (add-to-list 'dirvish-preview-dispatchers 'docx)
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


(provide 'my-setup-dired)
;;; my-dired.el ends here
