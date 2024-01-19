;;; windows.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein


;;; Commentary:

;; Setup for windows

;;; Code:
(message "Setting up window settings...")

;;;; Window Setup
(use-package window
  :ensure nil
  :custom
  ;; Emacs often opens buffers in new windows. Make window splitting and
  ;; placement more predictable.
  ;; TODO: revisit this setting?
  (display-buffer-base-action nil))

;;;; Window Division
;; Vertical window divider
(use-package frame
  :ensure nil
  :custom
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))
;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

;;;;; Window Movement
;;FIXME: Figure out how best to streamline window movement.
;; Quickly switch windows in Emacs
(use-package ace-window
  :commands (ace-window
             ace-swap-window
             aw-flip-window))

(defun my-other-window ()
  (interactive)
  (other-window 1))
;; (bind-key* "C-c C-o" 'my-other-window) ;; interferes with org-open-link



;; Easy window movement by key
(use-package windmove
  :ensure nil
  :commands (windmove-up
             windmove-down
             windmove-left
             windmove-right)
  ;; :bind (("C-c C-h" . #'windmove-left)
  ;;        ("C-c C-l" . #'windmove-right)
  ;;        ("C-c C-j" . #'windmove-down)
  ;;        ("C-c C-k" . #'windmove-up))
  :config
  ;;;; Org keybindings and windmove
  ;; Default bindings conflict with org-mode
  ;; documentation: https://orgmode.org/manual/Conflicts.html
  ;; solution: re-assign windmove keybindings to hyper-<arrow>
  (windmove-default-keybindings 'hyper))

;;;;; Window Restore
;; Winner mode is a built-in package for restoring window configurations
;; https://www.emacswiki.org/emacs/WinnerMode
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))


(provide 'my-setup-windows)
;;; windows.el ends here
