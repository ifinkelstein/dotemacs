;;; scratch.el --- better scratch settings -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein

;;; Commentary:

;; Better scratch settings
;; See https://www.reddit.com/r/emacs/comments/4cmfwp/scratch_buffer_hacks_to_increase_its_utility/

;; Provide options for persistent scratch buffer
;; Don't kill scratch, bury it instead
;; These settings can be enabled/disabled via the relevant variables

;;; Code:
(message "Setting up scratch settings...")

;;;; Variables

(defcustom my-persistent-scratch t
  "If t, make scratch buffer persist across sessions.
Default is to persist."
  :group 'my-emacs
  :type 'boolean)

(defcustom my-scratch-save-dir my-cache-dir
  "Default directory for saving scratch file."
  :group 'my-emacs
  :type 'string)

(defcustom my-scratch-default-dir (concat (getenv "HOME") "/")
  "Default directory for scratch buffer.
User may prefer to set this to `my-scratch-save-dir'."
  :group 'my-emacs
  :type 'string)

;;;; Functions

(defun my--bury-scratch ()
  "Don't kill scratch buffer, bury it."
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil)
    t))

(add-hook 'kill-buffer-query-functions 'my--bury-scratch)

(defun my--save-persistent-scratch ()
  "Save the contents of *scratch*."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max)
                  (concat my-scratch-save-dir "scratch"))))

(defun my--load-persistent-scratch ()
  "Reload the scratch buffer."
  (let ((scratch-file (concat my-scratch-save-dir "scratch")))
    (if (file-exists-p scratch-file)
        (with-current-buffer (get-buffer "*scratch*")
          (delete-region (point-min) (point-max))
          (insert-file-contents scratch-file))))
  ;; set default dir for scratch buffer
  (with-current-buffer (get-buffer "*scratch*")
    (setq-local default-directory my-scratch-default-dir)))

;; Hooks for loading and saving the scratch buffer
(cond (my-persistent-scratch
       (add-hook 'after-init-hook 'my--load-persistent-scratch)
       (add-hook 'kill-emacs-hook 'my--save-persistent-scratch)
       ;; Save scratch buffer every 5 minutes (300 seconds)
       (run-with-idle-timer 300 t 'my--save-persistent-scratch)))

;;; End Setup-Scratch
(provide 'my-setup-scratch)
;;; scratch.el ends here
