;;; init.el  -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;; Author: Ilya Finkelstein
;; Version: 0.1

;;; Commentary:
;; This is the base init file to load the entire emacs config. For ease of
;; navigation use outline-mode to cycle headlines open and closed (<Tab> and
;; <S-Tab>) to navigate through sections, and "imenu" to locate individual
;; use-package definitions.
;; This config is heavily adapted from: https://github.com/Lambda-Emacs/lambda-emacs

;;; Code:
;;;; Startup
;;;;; Use-Package
;; Use-Package Settings
(use-package use-package
  :custom
  ;; Don't automatically defer
  (use-package-always-defer nil)
  ;; Report loading details
  (use-package-verbose t)
  ;; This is really helpful for profiling
  (use-package-minimum-reported-time 0)
  ;; Expand normally
  (use-package-expand-minimally )
  ;; auto-install packages
  (use-package-always-ensure t)
  ;; Navigate use-package declarations w/imenu
  (use-package-enable-imenu-support t))

;; add the :vc keyword to use-package
;; This should be upstreamed in future emacs releases
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

;; (package-refresh-contents)
;;;;; El-Patch
;; Package for helping advise/modify features of other packages
(use-package el-patch
  :ensure t
  :hook (emacs-startup . el-patch-use-package-mode)
  :custom
  (el-patch-enable-use-package-integration t))

;;;;; Security
;; Properly verify outgoing ssl connections.
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(use-package gnutls
  :ensure nil
  :defer 1
  :custom
  (gnutls-verify-error t)
  (gnutls-min-prime-bits 3072))

;;;;; Auto-compile
;; Automatically byte-recompile changed elisp libraries
(use-package auto-compile
  :ensure t
  :defer 1
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter nil)
  (auto-compile-use-mode-line nil)
  (auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;;;; Emacs Build Version
(defun my-emacs-version ()
  "A convenience function to print the emacs-version in the echo-area/*messages* buffer and put
emacs-version string on the kill ring."
  (interactive)
  (let ((emacs (emacs-version)))
    (message (emacs-version))
    (kill-new emacs)))

;;;;; Outline Navigation
;; Navigate elisp files easily. Outline is a built-in library and we can easily
;; configure it to treat elisp comments as headings.
(use-package outline
  :ensure nil
  :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map ;; for colemak keyboard
         ("<tab>"   . outline-cycle)
         ("S-<tab>" . outline-cycle-buffer)
         ("M-n"     . outline-move-subtree-down)
         ("M-e"     . outline-move-subtree-up)
         ("M-m"     . outline-promote)
         ("M-i"     . outline-demote))
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; prevent `outline-level' from being overwritten by `lispy'
              (setq-local outline-level #'outline-level)
              ;; setup heading regexp specific to `emacs-lisp-mode'
              (setq-local outline-regexp ";;;\\(;* \\)")
              ;; heading alist allows for subtree-like folding
              (setq-local outline-heading-alist
                          '((";;; " . 1)
                            (";;;; " . 2)
                            (";;;;; " . 3)
                            (";;;;;; " . 4)
                            (";;;;;;; " . 5))))))
;;;;; User settings
;; Give Emacs some personal info
(setq user-full-name "Ilya Finkelstein"
      user-mail-address "ilya@finkelsteinlab.org")

;; where to store private or "secret" info
(let ((private (expand-file-name "private.el" my-user-dir)))
  (if (file-exists-p private)
	  (load-file private)))



;; org settings
(setq org-directory "/Users/ilya/Work/org/"
      org-cal-directory "/Users/ilya/Work/org/cal"
      org-default-notes-file (concat org-directory "inbox.org")
      org-agenda-files (list org-directory org-cal-directory))



;;;; Load Configuration Modules

;; This section loads a series of elisp libraries or 'modules' as defined below.

(defun my--default-modules ()
  "Load a default Emacs configuration."
  (message "
;; ======================================================
;; *Loading default setup
;; ======================================================")
  (measure-time
   (cl-dolist (mod (list
                    ;; Core modules
                    'my-setup-libraries
                    'my-setup-settings
                    'my-setup-functions
                    'my-setup-server
                    'my-setup-scratch

                    ;; UI modules
                    'my-setup-frames
                    'my-setup-windows
                    'my-setup-buffers
                    'my-setup-fonts
                    'my-setup-faces
                    'my-setup-colors
                    'my-setup-completion
                    'my-setup-keybindings
                    'my-setup-help
                    'my-setup-modeline
                    'my-setup-theme
                    ;; 'my-setup-splash

                    ;; Navigation & Search modules
                    'my-setup-navigation
                    'my-setup-search
                    'my-setup-dired

                    ;; Project & Tab/Workspace modules
                    'my-setup-vc
                    'my-setup-projects
                    'my-setup-tabs

                    ;; Org modules
                    'my-setup-org-base
                    'my-setup-org-extensions

                    ;; Reading modules
                    'my-setup-reading

                    ;; Writing modules
                    'my-setup-writing
                    'my-setup-notes
                    'my-setup-latex
                    'my-setup-citations

                    ;; Shell & Terminal
                    'my-setup-shell
                    ;; 'my-setup-eshell

                    ;; Programming modules
                    'my-setup-programming

                    ;; Productivity
                    'my-setup-mail
                    'my-setup-pdf
                    'my-setup-workspaces
                    'my-setup-ai

                    ;; other convenience functions
                    'my-setup-media
                    ))
     (require mod))))


;; Load default config
(message "*Loading default Emacs modules")
(my--default-modules)

;; MacOS settings - defer load until after init.
(when sys-mac
  (message "*Load MacOS settings...*")
  (require 'my-setup-macos))

;; Ask if user would like to create a config file.
;; ((when (not (file-exists-p my-config-file))
;;    (y-or-n-p "Would you like to create a user configuration file? ")
;;    (progn
;;      (with-temp-file my-config-file
;;        (insert-file my-default-config-file))
;;      (load-file my-config-file))))

;;;; After Startup
;; reset file-name-handler-alist
(add-hook 'emacs-startup-hook (lambda ()
                                (setq file-name-handler-alist my-file-name-handler-alist)
                                ;; reset garbage collection
                                ;; ref: https://www.reddit.com/r/emacs/comments/1h8879p/lowering_gcconsthreshold_could_help_relieve_gc/
                                (setq gc-cons-percentage 0.2
                                      gc-cons-threshold 80000000)
                                ;; Startup time
                                (message (format ";; ======================================================\n;; Emacs ready in %.2f seconds with %d garbage collections.\n;; ======================================================"
                                                 (float-time
                                                  (time-subtract after-init-time before-init-time)) gcs-done)
                                         (put 'narrow-to-page 'disabled nil))))


(put 'ns-print-buffer 'disabled nil)
