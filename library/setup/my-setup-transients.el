;;; my-setup-transients.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein

;;; Commentary:

;; Setup for transients. I am slowly going to build up infrequent commands into transient menus so I don't have to remember them.

;;; Code:
(message "Setting up transient settings...")

;;;; Transient defaults
;;;;; Transient Menus
(use-package transient
  :ensure nil
  :defer 1
  :custom
  (transient-levels-file (concat my-cache-dir "transient/levels.el"))
  (transient-values-file (concat my-cache-dir "transient/values.el"))
  (transient-history-file (concat my-cache-dir "transient/history.el"))
  ;; (transient-detect-key-conflicts t) ;; disabled bc conflicts with "casual" transients
  (transient-force-fixed-pitch t)
  (transient-show-popup t)
  ;; set transient popup to top of window
  (transient-display-buffer-action '(display-buffer-in-side-window
                                     (side . top)
                                     (dedicated . t)
                                     (inhibit-same-window . t)
                                     (window-parameters (no-other-window . t)))))

;;;; smerge transient
;; activate smerge transient with "RET"
;; source: https://github.com/axelf4/dotfiles/blob/374e148ff808669eb081e97b53a6b09d4f4e47c9/.config/emacs/init.el#L396-L426
(with-eval-after-load 'smerge-mode
  (require 'transient)
  (transient-define-prefix smerge-dispatch ()
    "Invoke an SMerge command from a list of available commands."
    [["Keep"
      ("b" "Base" smerge-keep-base)
      ("u" "Upper" smerge-keep-upper)
      ("l" "Lower" smerge-keep-lower)
      ("a" "All" smerge-keep-all) ("RET" "Current" smerge-keep-current)]
     ["Diff"
      ("<" "Base/upper" smerge-diff-base-upper)
      ("=" "Upper/lower" smerge-diff-upper-lower)
      (">" "Base/lower" smerge-diff-base-lower)
      ("R" "Refine" smerge-refine :transient t)]
     ["Other"
      ("C" "Combine" smerge-combine-with-next)
      ("r" "Resolve" smerge-resolve) ("x" "Kill current" smerge-kill-current)]])
  (define-key (plist-get smerge-text-properties 'keymap)
    (kbd "RET") '(menu-item "" smerge-dispatch)))

;;;; daily planning transient
;; Transient to help me plan the next day
(transient-define-prefix my-daily-plan ()
  "Set of functions for planning the day's tasks."
  ["Populating an agenda"
   ("t" "Copy to today's agenda" my-copy-org-subtree-to-today-agenda)
   ("T" "Copy to tomorrow's agenda" my-copy-org-subtree-to-tomorrow-agenda)])









(provide 'my-setup-transients)
;;; my-setup-transients.el ends here
