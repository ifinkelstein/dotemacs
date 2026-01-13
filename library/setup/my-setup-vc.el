;;; my-setup-vc.el -*- lexical-binding: t -*-

;; * VC
(use-package vc
  :ensure nil
  :hook (emacs-startup . vc-mode)
  :custom
  (vc-follow-symlinks t)
  (vc-log-short-style '(file)))

(use-package vc-git
  :ensure nil
  :after vc
  :config
  (setq vc-git-diff-switches "--patch-with-stat")
  (setq vc-git-print-log-follow t))

(use-package vc-annotate
  :ensure nil
  :after vc
  :config
  (setq vc-annotate-display-mode 'scale))

;; * Magit
(use-package magit
  :after transient
  :commands
  (magit-blame-mode
   magit-commit
   magit-diff
   magit-log
   magit-status)
  :hook ((git-commit-mode . turn-on-flyspell)
	     (git-commit-mode . cpm/git-commit-auto-fill-everywhere))
  :bind ((:map magit-log-mode-map
               ;; Keybindings for use with updating packages interactively
               ("Q" . #'exit-recursive-edit)))
  :init
  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  
  :custom
  ;; explicitly pointing to homebrew-installed git is supposed to speed up magit
  ;; https://gregnewman.io/blog/speed-up-magit-on-macos/
  (magit-git-executable "/opt/homebrew/bin/git")
  :config
  (setq magit-log-margin '(t "%Y-%m-%d.%H:%M:%S "  magit-log-margin-width nil 18))
  (setq magit-refresh-status-buffer t)
  ;; Fine grained diffs
  (setq magit-diff-refine-hunk t)
  ;; control magit initial visibility
  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . hide) (unpushed . hide) ([unpulled status] . show)))
  (global-git-commit-mode t) ; use emacs as editor for git commits

  ;; refresh status buffer
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  ;; no magit header line as it conflicts some mode-lines
  ;; NOTE: this may not be required anymore
  (advice-add 'magit-set-header-line-format :override #'ignore)
  ;; display magit setting
  ;; (setq magit-display-buffer-function #'my-diplay-magit-in-other-window)
  ;; (setq magit-display-buffer-function #'my-magit-display-buffer-pop-up-frame)

  ;; make magit full frame and restore frame config after
  ;; https://takeonrules.com/2024/03/01/quality-of-life-improvement-for-entering-and-exiting-magit/
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function
        #'magit-restore-window-configuration)

  (defun cpm/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 80 characters."
    (setq fill-column 80)
    (setq-local comment-auto-fill-only-comments nil))
  (setq git-commit-summary-max-length 50)
  (with-eval-after-load 'meow
    (add-hook 'git-commit-mode-hook
              (lambda ()
                (meow-insert-mode))))

  ) ;; use-package magit

;; optional: display magit status in new frame
(defun my-magit-display-buffer-pop-up-frame (buffer)
  (if (with-current-buffer buffer (eq major-mode 'magit-status-mode))
      (display-buffer buffer
                      '((display-buffer-reuse-window
                         display-buffer-pop-up-frame)
                        (reusable-frames . t)))
    (magit-display-buffer-traditional buffer)))

;; optional: display magit in other window & create one if only 1 window
(defun my-display-magit-in-other-window (buffer)
  (if (one-window-p)
      (progn
        (split-window-right)
        (other-window 1)
        (display-buffer buffer
                        '((display-buffer-reuse-window))))
    (magit-display-buffer-traditional buffer)))

;; * Git Gutter HL (Diff-HL)
;; Nice vc highlighting in margin/fringe
;; See https://www.reddit.com/r/emacs/comments/suxc9b/modern_gitgutter_in_emacs/
;; And https://github.com/jimeh/.emacs.d/blob/master/modules/version-control/siren-diff-hl.el
(use-package diff-hl
  :hook
  ((prog-mode . diff-hl-mode)
   (text-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-side 'left)
  (diff-hl-fringe-bmp-function 'cpm--diff-hl-fringe-bmp-from-type)
  (diff-hl-fringe-face-function 'cpm--diff-hl-fringe-face-from-type)
  (diff-hl-margin-symbols-alist
   '((insert . "┃")
     (delete . "┃")
     (change . "┃")
     (unknown . "?")
     (ignored . "i")))
  :init
  (defun cpm--diff-hl-fringe-face-from-type (type _pos)
    (intern (format "cpm--diff-hl-%s" type)))

  (defun cpm--diff-hl-fringe-bmp-from-type(type _pos)
    (intern (format "cpm--diff-hl-%s" type)))

  (defun cpm--diff-hl-set-render-mode ()
    (diff-hl-margin-mode (if window-system -1 1)))
  :config
  (diff-hl-margin-mode 1)
  (define-fringe-bitmap 'diff-hl-insert
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'diff-hl-change
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'diff-hl-delete
    [#b00000011] nil nil '(center repeated)))

;; * Diff Files with Vdiff
(use-package vdiff-magit
  :after magit
  :defer t
  :init
  (with-eval-after-load 'magit
    (define-key magit-mode-map "e" #'vdiff-magit-dwim)
    (define-key magit-mode-map "E" #'vdiff-magit)
    (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
    (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
    (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
    (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)))

;; * Ediff
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  ;; Quit ediff without confirmation prompt
  (defun my-ediff-quit-no-confirm (orig-fun &optional reverse-default-keep-variants)
    "Quit ediff without y-or-n-p confirmation."
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (funcall orig-fun reverse-default-keep-variants)))
  (advice-add 'ediff-quit :around #'my-ediff-quit-no-confirm))

;; * Quick Commits
;; Make a quick commit without opening magit.
(defun my-quick-commit ()
  "Quickly commit the current file-visiting buffer from the mini-buffer."
  (interactive)
  (shell-command (concat "git add '" (buffer-file-name) "' && git commit -m '" (read-string "Enter commit message: ") "'")))

;; ** End Setup VC
(provide 'my-setup-vc)
;; my-setup-vc.el ends here
