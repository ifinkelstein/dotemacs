;;; my-setup-bookmarks.el --- Bookmarks, registers and marks  -*- lexical-binding: t; -*-

;; Emacs bookmarks plus a small transient for bookmarks, registers,
;; marks and recent places.

;;; Code:

(require 'transient)

(use-package bookmark
  :ensure nil
  :defer 2
  :custom
  (bookmark-default-file (concat my-cache-dir "bookmarks")))

;;;###autoload (autoload 'my-bookmarks-tmenu "my-setup-bookmarks" nil t)
(transient-define-prefix my-bookmarks-tmenu ()
  "Bookmarks, registers and marks."
  [["Bookmarks"
    ("j" "Jump..." consult-bookmark)
    ("m" "Set..." bookmark-set)]
   ["Registers"
    ("r" "View/jump" consult-register)
    ("R" "Store point" point-to-register)]
   ["Places"
    ("." "Last edit" goto-last-change :transient t)
    ("'" "Mark ring" consult-mark)
    ("F" "Recent files" consult-recent-file)]])

(keymap-global-set "M-g m" #'my-bookmarks-tmenu)

(provide 'my-setup-bookmarks)
;;; my-setup-bookmarks.el ends here
