;;; my-setup-vc.el -*- lexical-binding: t -*-

;; * VC
(use-package vc
  :ensure nil
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
  :commands
  (magit-blame-mode
   magit-commit
   magit-diff
   magit-log
   magit-status)
  :hook ((git-commit-mode . jinx-mode)
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
  (magit-git-executable (executable-find "git"))
  :config
  (setq magit-log-margin '(t "%Y-%m-%d.%H:%M:%S "  magit-log-margin-width nil 18))
  ;; Fine grained diffs
  (setq magit-diff-refine-hunk t)
  ;; control magit initial visibility
  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . hide) (unpushed . hide) ([unpulled status] . show)))

  ;; refresh status buffer on every save
  ;; NOTE: this is on the global after-save-hook so fires for every saved file.
  ;; If magit status refreshes feel slow, consider removing this hook.
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

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

;; * Git Gutter HL (Diff-HL)
;; Nice vc highlighting in margin/fringe
(use-package diff-hl
  :hook
  ((prog-mode . diff-hl-mode)
   (text-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh . ignore)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-margin-symbols-alist
   '((insert . "┃")
     (delete . "┃")
     (change . "┃")
     (unknown . "?")
     (ignored . "i")))
  :config
  (diff-hl-margin-mode 1))

;; * Diff Files with Vdiff
(use-package vdiff-magit
  :after magit
  :config
  (define-key magit-mode-map "e" #'vdiff-magit-dwim)
  (define-key magit-mode-map "E" #'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

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

;; ** End Setup VC
(provide 'my-setup-vc)
;; my-setup-vc.el ends here
