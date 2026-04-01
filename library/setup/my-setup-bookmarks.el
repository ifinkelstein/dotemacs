;;; my-setup-bookmarks.el --- Bookmark packages and unified transient  -*- lexical-binding: t; -*-

;; Consolidates: Emacs bookmarks, casual (for bookmark list), dogears
;; (auto place history), javelin (harpoon-style pinned buffers),
;; registers, and a unified transient to access them all.

;;; Code:

(require 'transient)

;;* Emacs Bookmarks (built-in)
(use-package bookmark
  :ensure nil
  :defer 2
  :config
  (setq bookmark-default-file (concat my-cache-dir "bookmarks")))

;;* Casual (transient UI for bookmark list buffer)
(use-package casual
  :after bookmark
  :bind (:map bookmark-bmenu-mode-map
         ("M-o" . casual-bookmarks-tmenu)))

;;* Dogears (automatic place history)
(use-package dogears
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-r" . dogears-remember)
              ("M-g M-D" . dogears-sidebar))
  :config
  (dogears-mode 1))

;;* Javelin (harpoon-style pinned buffers)
(use-package javelin
  :bind (("M-g j" . javelin-add-file)
         ("M-g /" . javelin-toggle-quick-menu)
         ("M-g 1" . javelin-go-or-assign-to-1)
         ("M-g 2" . javelin-go-or-assign-to-2)
         ("M-g 3" . javelin-go-or-assign-to-3)
         ("M-g 4" . javelin-go-or-assign-to-4)
         ("M-g 5" . javelin-go-or-assign-to-5)))

;;* Embark Integration
;;
;; Adds embark actions for javelin and dogears:
;; - J on file/buffer/bookmark: pin to javelin slot
;; - E on file: dogear this location
;; - In dogears-list-mode: embark on entries with custom actions

;; Declare external functions for byte-compiler
(declare-function javelin-assign-to "javelin")
(declare-function dogears-remember "dogears")

;; Declare external variables for byte-compiler
(defvar dogears-list)
(defvar embark-general-map)
(defvar embark-file-map)
(defvar embark-buffer-map)
(defvar embark-bookmark-map)
(defvar embark-target-finders)
(defvar embark-keymap-alist)

;;** Javelin pin actions for files and buffers

(defun my-embark-javelin-pin-file (file)
  "Pin FILE to a javelin slot."
  (let ((slot (read-number "Javelin slot (1-9): " 1)))
    (with-current-buffer (find-file-noselect file)
      (javelin-assign-to slot)
      (message "Pinned %s to javelin slot %d"
               (file-name-nondirectory file) slot))))

(defun my-embark-javelin-pin-buffer (buffer)
  "Pin BUFFER to a javelin slot."
  (let ((slot (read-number "Javelin slot (1-9): " 1)))
    (with-current-buffer buffer
      (javelin-assign-to slot)
      (message "Pinned %s to javelin slot %d" buffer slot))))

(defun my-embark-javelin-pin-bookmark (bookmark)
  "Promote BOOKMARK to a javelin slot."
  (let ((slot (read-number "Javelin slot (1-9): " 1)))
    (bookmark-jump bookmark)
    (javelin-assign-to slot)
    (message "Pinned bookmark to javelin slot %d" slot)))

;;** Dogears actions for files

(defun my-embark-dogears-remember-file (file)
  "Remember FILE location in dogears."
  (save-window-excursion
    (find-file file)
    (dogears-remember)
    (message "Dogeared %s" (file-name-nondirectory file))))

;;** Dogears target finder and keymap

(defun my-embark-target-dogears-at-point ()
  "Target the dogear entry at point in `dogears-list-mode'."
  (when (derived-mode-p 'dogears-list-mode)
    (when-let* ((place (tabulated-list-get-id)))
      (cons 'dogears place))))

(defun my-embark-dogears-jump (place)
  "Jump to dogears PLACE."
  (bookmark-jump place))

(defun my-embark-dogears-delete (place)
  "Delete PLACE from dogears list."
  (setq dogears-list (delete place dogears-list))
  (when (derived-mode-p 'dogears-list-mode)
    (revert-buffer))
  (message "Dogear deleted"))

(defun my-embark-dogears-to-javelin (place)
  "Promote dogears PLACE to a javelin slot."
  (let ((slot (read-number "Javelin slot (1-9): " 1)))
    (bookmark-jump place)
    (javelin-assign-to slot)
    (message "Promoted to javelin slot %d" slot)))

(defun my-embark-dogears-to-bookmark (place)
  "Save dogears PLACE as a named bookmark."
  (bookmark-jump place)
  (call-interactively #'bookmark-set))

;;** Register with embark after it loads

(with-eval-after-load 'embark
  ;; Dogears keymap (must be defined after embark loads for parent)
  (defvar-keymap embark-dogears-map
    :doc "Keymap for embark actions on dogears entries."
    :parent embark-general-map
    "RET" #'my-embark-dogears-jump
    "j"   #'my-embark-dogears-jump
    "d"   #'my-embark-dogears-delete
    "J"   #'my-embark-dogears-to-javelin
    "b"   #'my-embark-dogears-to-bookmark)

  ;; Javelin actions on files, buffers, bookmarks
  (keymap-set embark-file-map "J" #'my-embark-javelin-pin-file)
  (keymap-set embark-buffer-map "J" #'my-embark-javelin-pin-buffer)
  (keymap-set embark-bookmark-map "J" #'my-embark-javelin-pin-bookmark)

  ;; Dogears action on files
  (keymap-set embark-file-map "E" #'my-embark-dogears-remember-file)

  ;; Dogears target finder and keymap
  (add-to-list 'embark-target-finders #'my-embark-target-dogears-at-point)
  (add-to-list 'embark-keymap-alist '(dogears . embark-dogears-map)))

;;* Unified Bookmark Transient

(declare-function dogears-go "dogears")
(declare-function dogears-back "dogears")
(declare-function dogears-forward "dogears")
(declare-function dogears-remember "dogears")
(declare-function dogears-list "dogears")
(declare-function dogears-sidebar "dogears")
(declare-function dogears-mode "dogears")
(declare-function bookmark-get-bookmark "bookmark")
(declare-function javelin-add-file "javelin")
(declare-function javelin-go-to "javelin")
(declare-function javelin-go-to-1 "javelin")
(declare-function javelin-go-to-2 "javelin")
(declare-function javelin-go-to-3 "javelin")
(declare-function javelin-go-to-4 "javelin")
(declare-function javelin-go-to-5 "javelin")
(declare-function javelin-delete "javelin")
(declare-function javelin-toggle-quick-menu "javelin")
(declare-function javelin-go-to-next "javelin")
(declare-function javelin-go-to-prev "javelin")
(declare-function javelin-clear "javelin")
(declare-function javelin-assign-to "javelin")
(declare-function javelin--bookmark-positions "javelin")
(declare-function javelin--bookmark-display-target "javelin")
(declare-function consult-bookmark "consult")
(declare-function consult-register "consult")
(declare-function consult-register-store "consult-register")
(declare-function consult-mark "consult")
(declare-function consult-global-mark "consult")
(declare-function consult-recent-file "consult")
(declare-function goto-last-change "goto-chg")
(declare-function goto-last-change-reverse "goto-chg")
(declare-function casual-editkit-registers-tmenu "casual-editkit-utils")
(declare-function casual-editkit-bookmarks-tmenu "casual-editkit-utils")

(defvar dogears-list)
(defvar dogears-idle)
(defvar dogears-idle-timer)
(defvar dogears-message)
(defvar bookmark-save-flag)
(defvar bookmark-use-annotations)
(defvar bookmark-fringe-mark)

(defun my-bookmarks--bookmark-status ()
  "Return bookmark settings status for transient display."
  (format "Bookmarks: auto-save %s, annotations %s, fringe %s"
          (if bookmark-save-flag "on" "off")
          (if bookmark-use-annotations "on" "off")
          (if bookmark-fringe-mark "on" "off")))

(defun my-bookmarks--dogears-clear ()
  "Clear all dogears history."
  (interactive)
  (when (y-or-n-p "Clear all dogears history? ")
    (setq dogears-list nil)
    (message "Dogears history cleared")))

(defun my-bookmarks--dogears-toggle-idle ()
  "Toggle dogears idle auto-remember on/off."
  (interactive)
  (if dogears-idle
      (progn
        (setq dogears-idle nil)
        (when (bound-and-true-p dogears-idle-timer)
          (cancel-timer dogears-idle-timer))
        (message "Dogears idle auto-remember disabled"))
    (setq dogears-idle 5)
    (when (bound-and-true-p dogears-mode)
      (setq dogears-idle-timer
            (run-with-idle-timer dogears-idle 'repeat #'dogears-remember)))
    (message "Dogears idle auto-remember enabled (5s)")))

(defun my-bookmarks--dogears-idle-status ()
  "Return dogears idle status for transient display."
  (if dogears-idle
      (format "Idle: %ss" dogears-idle)
    "Idle: off"))

(defun my-bookmarks--javelin-status ()
  "Return a formatted string showing current javelin slot assignments."
  (if (not (fboundp 'javelin--bookmark-positions))
      ""
    (let ((positions (javelin--bookmark-positions)))
      (if (null positions)
          "(no pins)"
        (mapconcat
         (lambda (pair)
           (let* ((pos (car pair))
                  (bm-name (cdr pair))
                  (record (bookmark-get-bookmark bm-name 'noerror))
                  (display (if record
                               (javelin--bookmark-display-target record)
                             "?")))
             (format "%d:%s" pos (truncate-string-to-width display 20 nil nil t))))
         positions "  ")))))

;;;###autoload (autoload 'my-bookmarks-tmenu "my-setup-bookmarks" nil t)
(transient-define-prefix my-bookmarks-tmenu ()
  "Unified bookmark management."
  [:description
   (lambda () (format "Javelin: %s" (my-bookmarks--javelin-status)))
   ["Pin (Javelin)"
    ("t" "Throw (pin here)" javelin-add-file)
    ("T" "Assign to slot..." javelin-assign-to)
    ("1" "Slot 1" javelin-go-to-1 :transient t)
    ("2" "Slot 2" javelin-go-to-2 :transient t)
    ("3" "Slot 3" javelin-go-to-3 :transient t)
    ("4" "Slot 4" javelin-go-to-4 :transient t)
    ("5" "Slot 5" javelin-go-to-5 :transient t)
    ("x" "Delete slot..." javelin-delete :transient t)
    ("X" "Clear all pins" javelin-clear :transient t)]

   ["History (Dogears)"
    ("e" "Remember here" dogears-remember :transient t)
    ("b" "Back" dogears-back :transient t)
    ("f" "Forward" dogears-forward :transient t)
    ("g" "Go to..." dogears-go)
    ("l" "List" dogears-list)
    ("s" "Sidebar" dogears-sidebar)]

   ["Emacs Bookmarks"
    ("m" "Set..." bookmark-set)
    ("j" "Jump..." consult-bookmark)
    ("J" "Jump (no preview)..." bookmark-jump)
    ("A" "Annotate..." bookmark-set-no-overwrite)
    ("L" "List all" list-bookmarks)
    ("d" "Delete..." bookmark-delete)
    ("S" "Save now" bookmark-save :transient t)
    ("<" "More..." casual-editkit-bookmarks-tmenu)]

   ["Registers"
    ("r" "View/jump (SPC r)" consult-register)
    ("R" "Store point (SPC R)" point-to-register)
    (":" "Jump to (g:)" jump-to-register)
    ("+" "Smart store..." consult-register-store)
    ("w" "Store window..." window-configuration-to-register)
    ("c" "Copy region..." copy-to-register :if use-region-p)
    ("i" "Insert..." insert-register)
    ("a" "Append region..." append-to-register :if use-region-p)
    (">" "More..." casual-editkit-registers-tmenu)]]

  ["Navigate"
   :class transient-row
   ("n" "Next pin" javelin-go-to-next :transient t)
   ("p" "Prev pin" javelin-go-to-prev :transient t)
   ("/" "Quick menu" javelin-toggle-quick-menu)
   ("'" "Mark ring" consult-mark)
   ("\"" "Global marks" consult-global-mark)
   ("." "Last edit" goto-last-change :transient t)
   ("," "Prev edit" goto-last-change-reverse :transient t)
   ("F" "Recent files" consult-recent-file)]

  [:description
   (lambda () (my-bookmarks--dogears-idle-status))
   :class transient-row
   ("I" "Toggle idle" my-bookmarks--dogears-toggle-idle :transient t)
   ("M" "Toggle messages" (lambda () (interactive)
                            (setq dogears-message (not dogears-message))
                            (message "Dogears messages: %s" (if dogears-message "on" "off")))
    :transient t)
   ("D" "Toggle mode" dogears-mode :transient t)
   ("K" "Clear history" my-bookmarks--dogears-clear :transient t)]

  [:description
   (lambda () (my-bookmarks--bookmark-status))
   :class transient-row
   ("C-s" "Toggle auto-save" (lambda () (interactive)
                               (setq bookmark-save-flag (not bookmark-save-flag))
                               (message "Bookmark auto-save: %s" (if bookmark-save-flag "on" "off")))
    :transient t)
   ("C-a" "Toggle annotations" (lambda () (interactive)
                                 (setq bookmark-use-annotations (not bookmark-use-annotations))
                                 (message "Bookmark annotations prompt: %s" (if bookmark-use-annotations "on" "off")))
    :transient t)
   ("C-f" "Toggle fringe" (lambda () (interactive)
                            (setq bookmark-fringe-mark (not bookmark-fringe-mark))
                            (message "Bookmark fringe marks: %s" (if bookmark-fringe-mark "on" "off")))
    :transient t)
   ("q" "Quit" transient-quit-all)])

;; Global binding
(keymap-global-set "M-g m" #'my-bookmarks-tmenu)

(provide 'my-setup-bookmarks)
;;; my-setup-bookmarks.el ends here
