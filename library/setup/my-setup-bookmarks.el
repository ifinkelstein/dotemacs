;;; my-setup-bookmarks.el --- Bookmark packages and unified transient  -*- lexical-binding: t; -*-

;; Consolidates: Emacs bookmarks, casual (for bookmark list), dogears
;; (auto place history), javelin (harpoon-style pinned buffers),
;; registers, and a unified transient to access them all.

;;; Code:

(require 'transient)

;;; External declarations

;; Dogears
(declare-function dogears-go "dogears")
(declare-function dogears-back "dogears")
(declare-function dogears-forward "dogears")
(declare-function dogears-remember "dogears")
(declare-function dogears-list "dogears")
(declare-function dogears-sidebar "dogears")
(declare-function dogears-mode "dogears")
(defvar dogears-list)
(defvar dogears-idle)
(defvar dogears-idle-timer)
(defvar dogears-message)

;; Javelin
(declare-function javelin-add-file "javelin")
(declare-function javelin-assign-to "javelin")
(declare-function javelin-delete "javelin")
(declare-function javelin-toggle-quick-menu "javelin")
(declare-function javelin-go-to-next "javelin")
(declare-function javelin-go-to-prev "javelin")
(declare-function javelin-clear "javelin")
(declare-function javelin--bookmark-positions "javelin")
(declare-function javelin-go-or-assign-to-1 "javelin")
(declare-function javelin-go-or-assign-to-2 "javelin")
(declare-function javelin-go-or-assign-to-3 "javelin")
(declare-function javelin-go-or-assign-to-4 "javelin")
(declare-function javelin-go-or-assign-to-5 "javelin")
(declare-function javelin-go-or-assign-to-6 "javelin")
(declare-function javelin-go-or-assign-to-7 "javelin")
(declare-function javelin-go-or-assign-to-8 "javelin")
(declare-function javelin-go-or-assign-to-9 "javelin")
(declare-function javelin-go-or-assign-to-10 "javelin")

;; Bookmark
(declare-function bookmark-get-bookmark "bookmark")
(defvar bookmark-save-flag)
(defvar bookmark-use-annotations)
(defvar bookmark-fringe-mark)

;; Consult
(declare-function consult-bookmark "consult")
(declare-function consult-register "consult")
(declare-function consult-register-store "consult-register")
(declare-function consult-mark "consult")
(declare-function consult-global-mark "consult")
(declare-function consult-recent-file "consult")

;; Other
(declare-function goto-last-change "goto-chg")
(declare-function goto-last-change-reverse "goto-chg")
(declare-function casual-editkit-registers-tmenu "casual-editkit-utils")
(declare-function casual-editkit-bookmarks-tmenu "casual-editkit-utils")

;; Embark
(defvar embark-general-map)
(defvar embark-file-map)
(defvar embark-buffer-map)
(defvar embark-bookmark-map)
(defvar embark-target-finders)
(defvar embark-keymap-alist)

;;; Package configuration

(use-package bookmark
  :ensure nil
  :defer 2
  :config
  (setq bookmark-default-file (concat my-cache-dir "bookmarks")))

(use-package casual
  :after bookmark
  :bind (:map bookmark-bmenu-mode-map
              ("M-o" . casual-bookmarks-tmenu)))

(use-package dogears
  :bind (("M-g d" . dogears-go)
         ("M-g M-b" . dogears-back)
         ("M-g M-f" . dogears-forward)
         ("M-g M-d" . dogears-list)
         ("M-g M-r" . dogears-remember)
         ("M-g M-D" . dogears-sidebar))
  :config
  (dogears-mode 1))

(use-package javelin
  :bind (("M-g j" . javelin-add-file)
         ("M-g /" . javelin-toggle-quick-menu)
         ("M-g 1" . javelin-go-or-assign-to-1)
         ("M-g 2" . javelin-go-or-assign-to-2)
         ("M-g 3" . javelin-go-or-assign-to-3)
         ("M-g 4" . javelin-go-or-assign-to-4)
         ("M-g 5" . javelin-go-or-assign-to-5)
         ("M-g 6" . javelin-go-or-assign-to-6)
         ("M-g 7" . javelin-go-or-assign-to-7)
         ("M-g 8" . javelin-go-or-assign-to-8)
         ("M-g 9" . javelin-go-or-assign-to-9)
         ("M-g 0" . javelin-go-or-assign-to-10)))

;;; Embark integration

(defun my-embark-javelin-pin (target)
  "Pin TARGET (file, buffer, or bookmark) to a javelin slot."
  (let ((slot (read-number "Javelin slot (1-10): " 1)))
    (cond
     ((bufferp target)
      (with-current-buffer target
        (javelin-assign-to slot)))
     ((and (stringp target) (file-exists-p target))
      (with-current-buffer (find-file-noselect target)
        (javelin-assign-to slot)))
     (t ; bookmark
      (bookmark-jump target)
      (javelin-assign-to slot)))
    (message "Pinned to javelin slot %d" slot)))

(defun my-embark-dogears-remember-file (file)
  "Remember FILE location in dogears."
  (save-window-excursion
    (find-file file)
    (dogears-remember)
    (message "Dogeared %s" (file-name-nondirectory file))))

(defun my-embark-bookmark-file (file)
  "Set a bookmark at the beginning of FILE."
  (save-window-excursion
    (find-file file)
    (call-interactively #'bookmark-set)))

(defun my-embark-bookmark-buffer (buffer)
  "Set a bookmark at point in BUFFER."
  (with-current-buffer buffer
    (call-interactively #'bookmark-set)))

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
  (bookmark-jump place)
  (my-embark-javelin-pin (current-buffer)))

(defun my-embark-dogears-to-bookmark (place)
  "Save dogears PLACE as a named bookmark."
  (bookmark-jump place)
  (call-interactively #'bookmark-set))

(with-eval-after-load 'embark
  (defvar-keymap embark-dogears-map
    :doc "Keymap for embark actions on dogears entries."
    :parent embark-general-map
    "RET" #'my-embark-dogears-jump
    "j"   #'my-embark-dogears-jump
    "d"   #'my-embark-dogears-delete
    "J"   #'my-embark-dogears-to-javelin
    "b"   #'my-embark-dogears-to-bookmark)

  (keymap-set embark-file-map "J" #'my-embark-javelin-pin)
  (keymap-set embark-buffer-map "J" #'my-embark-javelin-pin)
  (keymap-set embark-bookmark-map "J" #'my-embark-javelin-pin)
  (keymap-set embark-file-map "E" #'my-embark-dogears-remember-file)
  (keymap-set embark-file-map "m" #'my-embark-bookmark-file)
  (keymap-set embark-buffer-map "m" #'my-embark-bookmark-buffer)
  (keymap-set embark-general-map "m" #'bookmark-set)

  (add-to-list 'embark-target-finders #'my-embark-target-dogears-at-point)
  (add-to-list 'embark-keymap-alist '(dogears . embark-dogears-map)))

;;; Transient helpers

(defun my-bookmarks--javelin-slot-desc (n)
  "Return description for javelin slot N."
  (if-let* (((fboundp 'javelin--bookmark-positions))
            (positions (javelin--bookmark-positions))
            (bm-name (alist-get n positions))
            (record (bookmark-get-bookmark bm-name 'noerror))
            (filename (bookmark-get-filename record)))
      (truncate-string-to-width (file-name-nondirectory filename) 12 nil nil t)
    "<empty>"))

(defun my-bookmarks--dogears-idle-status ()
  "Return dogears idle status for transient display."
  (if dogears-idle (format "Idle: %ss" dogears-idle) "Idle: off"))

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

(defun my-bookmarks--toggle-dogears-message ()
  "Toggle dogears message on/off."
  (interactive)
  (setq dogears-message (not dogears-message))
  (message "Dogears messages: %s" (if dogears-message "on" "off")))

(defun my-bookmarks--toggle-auto-save ()
  "Toggle bookmark auto-save on/off."
  (interactive)
  (setq bookmark-save-flag (not bookmark-save-flag))
  (message "Bookmark auto-save: %s" (if bookmark-save-flag "on" "off")))

(defun my-bookmarks--toggle-annotations ()
  "Toggle bookmark annotations prompt on/off."
  (interactive)
  (setq bookmark-use-annotations (not bookmark-use-annotations))
  (message "Bookmark annotations prompt: %s" (if bookmark-use-annotations "on" "off")))

(defun my-bookmarks--toggle-fringe ()
  "Toggle bookmark fringe marks on/off."
  (interactive)
  (setq bookmark-fringe-mark (not bookmark-fringe-mark))
  (message "Bookmark fringe marks: %s" (if bookmark-fringe-mark "on" "off")))

(defun my-bookmarks--set ()
  "Set a bookmark, deferring prompt to avoid transient conflicts."
  (interactive)
  (run-at-time 0 nil #'call-interactively #'bookmark-set))

(defun my-bookmarks--list ()
  "Show bookmark list buffer."
  (interactive)
  (bookmark-bmenu-list)
  (switch-to-buffer "*Bookmark List*"))

;;; Transient menus

;;;###autoload (autoload 'my-bookmarks-settings-tmenu "my-setup-bookmarks" nil t)
(transient-define-prefix my-bookmarks-settings-tmenu ()
  "Bookmark settings."
  [:description
   my-bookmarks--dogears-idle-status
   ("I" "Toggle idle" my-bookmarks--dogears-toggle-idle :transient t)
   ("M" "Toggle messages" my-bookmarks--toggle-dogears-message :transient t)
   ("D" "Toggle mode" dogears-mode :transient t)
   ("K" "Clear history" my-bookmarks--dogears-clear :transient t)]

  [:description
   my-bookmarks--bookmark-status
   ("s" "Toggle auto-save" my-bookmarks--toggle-auto-save :transient t)
   ("a" "Toggle annotations" my-bookmarks--toggle-annotations :transient t)
   ("f" "Toggle fringe" my-bookmarks--toggle-fringe :transient t)]

  ["Javelin"
   ("X" "Clear all pins" javelin-clear :transient t)]

  [("q" "Back" transient-quit-one)])

;;;###autoload (autoload 'my-bookmarks-tmenu "my-setup-bookmarks" nil t)
(transient-define-prefix my-bookmarks-tmenu ()
  "Unified bookmark management."
  [["Bookmarks"
    ("m" "Set..." my-bookmarks--set)
    ("j" "Jump..." consult-bookmark)
    ("J" "Jump (no preview)..." bookmark-jump)
    ("A" "Annotate..." bookmark-set-no-overwrite)
    ("L" "List all" my-bookmarks--list)
    ("d" "Delete..." bookmark-delete)
    ("S" "Save now" bookmark-save :transient t)
    ("<" "More..." casual-editkit-bookmarks-tmenu)]

   ["Javelin"
    ("t" "Throw (pin here)" javelin-add-file)
    ("T" "Assign to slot..." javelin-assign-to)
    ("1" javelin-go-or-assign-to-1 :description (lambda () (my-bookmarks--javelin-slot-desc 1)))
    ("2" javelin-go-or-assign-to-2 :description (lambda () (my-bookmarks--javelin-slot-desc 2)))
    ("3" javelin-go-or-assign-to-3 :description (lambda () (my-bookmarks--javelin-slot-desc 3)))
    ("4" javelin-go-or-assign-to-4 :description (lambda () (my-bookmarks--javelin-slot-desc 4)))
    ("5" javelin-go-or-assign-to-5 :description (lambda () (my-bookmarks--javelin-slot-desc 5)))]

   ["Javelin 6-0"
    ("6" javelin-go-or-assign-to-6 :description (lambda () (my-bookmarks--javelin-slot-desc 6)))
    ("7" javelin-go-or-assign-to-7 :description (lambda () (my-bookmarks--javelin-slot-desc 7)))
    ("8" javelin-go-or-assign-to-8 :description (lambda () (my-bookmarks--javelin-slot-desc 8)))
    ("9" javelin-go-or-assign-to-9 :description (lambda () (my-bookmarks--javelin-slot-desc 9)))
    ("0" javelin-go-or-assign-to-10 :description (lambda () (my-bookmarks--javelin-slot-desc 10)))
    ("x" "Delete slot..." javelin-delete :transient t)]

   ["Dogears"
    ("e" "Remember here" dogears-remember :transient t)
    ("b" "Back" dogears-back :transient t)
    ("f" "Forward" dogears-forward :transient t)
    ("g" "Go to..." dogears-go)
    ("l" "List" dogears-list)
    ("s" "Sidebar" dogears-sidebar)]

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

  [""
   :class transient-row
   ("?" "Settings..." my-bookmarks-settings-tmenu)
   ("q" "Quit" transient-quit-all)])

(keymap-global-set "M-g m" #'my-bookmarks-tmenu)

(provide 'my-setup-bookmarks)
;;; my-setup-bookmarks.el ends here
