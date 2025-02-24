;;; init.el  -*- lexical-binding: t -*-
;; Author: Ilya Finkelstein
;; Version: 0.1

;;; Commentary:
;; This is the base init file to load the entire emacs config. For ease of
;; navigation use outline-mode to cycle headlines open and closed (<Tab> and
;; <S-Tab>) to navigate through sections, and "imenu" to locate individual
;; use-package definitions.
;; This config is heavily adapted from:
;; https://github.com/Lambda-Emacs/lambda-emacs
;; https://github.com/karthink/.emacs.d


;;*Bug Hunter
;; this package helps to bisect an init file
;; (use-package bug-hunter)

;;* System Variables
;; Check the operating system
(defconst sys-linux   (eq system-type 'gnu/linux))
(defconst sys-mac     (eq system-type 'darwin))
(defconst sys-win     (memq system-type '(cygwin windows-nt ms-dos)))

;;* Paths
;;  We're going to define a number of directories that are used throughout this
;;  configuration to store different types of files. This allows us to keep
;;  `user-emacs-directory' tidy.

(defconst my-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

(defconst my-library-dir (concat my-emacs-dir "library/")
  "The directory for my Emacs Lisp libraries.
This will house all setup libraries and external libraries or packages.")

(defconst my-user-dir (concat my-library-dir "private/")
  "Storage for personal elisp, scripts, and any other private files.")

(defconst my-setup-dir (concat my-library-dir "setup/")
  "The storage location of the setup-init files.")

(defconst my-var-dir (concat my-emacs-dir "var/")
  "The directory for non-essential file storage.
Contents are subject to change. Used for package storage (elpa or
straight) and by `my-etc-dir' and `my-cache-dir'.")

(defconst my-etc-dir (concat my-var-dir "etc/")
  "The directory for non-volatile storage.
  These are not deleted or tampered with by emacs functions. Use
  this for dependencies like servers or config files that are
  stable (i.e. it should be unlikely that you need to delete them
               if something goes wrong).")

(defconst my-cache-dir (concat my-var-dir "cache/")
  "The directory for volatile storage.
  Use this for transient files that are generated on the fly like
  caches and ephemeral/temporary files. Anything that may need to
  be cleared if there are problems.")

;; Find the user configuration file
;; This file may need to be created first
(defconst my-config-file (expand-file-name "config.el" my-user-dir)
  "The user's configuration file.")

;; Make System Directories
;; Directory paths
(dolist (dir (list my-library-dir my-var-dir my-etc-dir my-cache-dir my-user-dir my-setup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Add all configuration files to load-path
(eval-and-compile
  (progn
    (push my-setup-dir load-path)
    (push my-user-dir load-path)))

;; where to store private or "secret" info
(let ((private (expand-file-name "private.el" my-user-dir)))
  (if (file-exists-p private)
	  (load-file private)))


;; org folder and file settings
;; move to org module?
(setq org-directory "/Users/ilya/Work/org/"
      org-cal-directory "/Users/ilya/Work/org/cal"
      org-default-notes-file (concat org-directory "inbox.org")
      org-agenda-files (list org-directory org-cal-directory))

;;* Package Management
;; Load the package-system.
(require 'package)

;; Make sure the elpa/ folder exists after setting it above.
(unless (file-exists-p package-user-dir)
  (mkdir package-user-dir t))
(setopt package-quickstart-file (expand-file-name "package-quickstart.el" my-cache-dir))

;;** Package Archives
;; See https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/ for discussion
(setopt package-archives
        '(("elpa" . "https://elpa.gnu.org/packages/")
          ;; ("elpa-devel" . "https://elpa.gnu.org/devel/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")
          ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
          )

        ;; Highest number gets priority (what is not mentioned gets priority 0)
        package-archive-priorities
        '(;; Prefer MELPA
          ("melpa" . 90)
          ("elpa" . 80))

        ;; Set location of package directory
        package-user-dir (expand-file-name "elpa/" my-var-dir)
        package-gnupghome-dir (concat package-user-dir "gnupg"))
;; initialize the package system
(package-initialize)
;;** Use-Package Settings
(use-package use-package
  :custom
  ;; Don't automatically defer
  (use-package-always-defer nil)
  ;; Report loading details
  (use-package-verbose t)
  ;; Expand normally
  (use-package-expand-minimally )
  ;; auto-install packages
  (use-package-always-ensure t)
  ;; Navigate use-package declarations w/imenu
  (use-package-enable-imenu-support t))

;; enable compat package early
(use-package compat)
;; (use-package compat-macs
;;   :if sys-mac)

;; Package for helping advise/modify features of other packages
;; (use-package el-patch
;;   :ensure t
;;   :hook (emacs-startup . el-patch-use-package-mode)
;;   :custom
;;   (el-patch-enable-use-package-integration t))

;; refresh package list--this can take a while, so I'm commenting it out
;; (package-refresh-contents)


;;* UI settings
;; Disable certain byte compiler warnings to cut down on the noise. This is a
;; personal choice and can be removed if you would like to see any and all byte
;; compiler warnings.
;; NOTE: Setopt won't work here
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))

;; Don't produce backtraces when errors occur.
;; This can be set to `t' interactively when debugging.
(setopt debug-on-error nil)

;;** Themes
;;* Other/unsorted config

;; Ordinarily we might leave theme loading until later in the init process, but
;; this leads to the initial frame flashing either light or dark color,
;; depending on the system settings. Let's avoid that by loading a default theme
;; before initial frame creation. The modus themes are built in and excellent.
;; NOTE: 1. The default theme is set only if there are no user configuration
;; files, otherwise it is left to the user to do; 2. This system check only
;; works for MacOS, with an emacs build with the ns-system-appearance patch. For
;; examples of such builds see https://github.com/mclear-tools/build-emacs-macos
;; or https://github.com/d12frosted/homebrew-emacs-plus


;;* Security
;; Properly verify outgoing ssl connections.
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html
;; May be updated, per their updated blog
(use-package gnutls
  :ensure nil
  :defer 1
  :custom
  (gnutls-verify-error t)
  (gnutls-min-prime-bits 3072))

;;* Auto-compile
;; Automatically byte-recompile changed elisp libraries
(use-package auto-compile
  :disabled t
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter nil)
  (auto-compile-use-mode-line nil)
  (auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;* Convenience Functions
;;** Emacs Build Version
(defun my-emacs-version ()
  "A convenience function to print the emacs-version in the echo-area/*messages* buffer and put
emacs-version string on the kill ring."
  (interactive)
  (let ((emacs (emacs-version)))
    (message (emacs-version))
    (kill-new emacs)))

;;* Load configuration modules
;; This section loads a series of elisp libraries or 'modules' as defined below.

(message "
======================================================
Loading modules
======================================================")
;; this order is meaningful, as things may break at some point
(require 'my-setup-libraries) ;; common emacs libraries
(require 'my-setup-ui)
(require 'my-setup-functions) ;; load a bunch of helper functions for many things
(require 'my-setup-navigation)
(require 'my-setup-buffers)
(require 'my-setup-search)
(require 'my-setup-completion)
(require 'my-setup-keybindings)
(require 'my-setup-programming)
(require 'my-setup-org)

(require 'my-setup-dired)
(require 'my-setup-notes) ;; org-roam and helpers
(require 'my-setup-projects)

(require 'my-setup-reading)
(require 'my-setup-writing)
(require 'my-setup-latex)
(require 'my-setup-pdf)
(require 'my-setup-vc)
(require 'my-setup-shell) ;; various shell and CLI-related functions
(require 'my-setup-mail)

(require 'my-setup-ai)
(require 'my-setup-media) 

(require 'my-setup-server)
(require 'my-setup-slack)


;;** MacOS settings - defer load until after init.
(when sys-mac
  (require 'my-setup-macos))

;;* Outline Navigation
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
  :custom-face
  (outline-1 ((t (:height 1.1))))
  (outline-2 ((t (:height 1.05))))
  (outline-3 ((t (:height 1.0))))
  (outline-4 ((t (:height 0.95))))
  (outline-5 ((t (:height 0.9))))
  (outline-6 ((t (:height 0.85))))
  (outline-7 ((t (:height 0.8))))
  (outline-8 ((t (:height 0.75))))

  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; prevent `outline-level' from being overwritten by `lispy'
              (setq-local outline-level #'outline-level)
              ;; setup heading regexp specific to `emacs-lisp-mode'
              (setq-local outline-regexp ";;\\*+")
              (setq-local page-delimiter ";;\\**")
              ;; heading alist allows for subtree-like folding
              (setq-local outline-heading-alist
                          '((";;* " . 1)
                            (";;** " . 2)
                            (";;*** " . 3)
                            (";;**** " . 4)
                            (";;***** " . 5))))

            (outline-minor-mode 1)
            (outline-hide-sublevels 5)
            ))





;;* After Startup
;; reset file-name-handler-alist
(add-hook 'emacs-startup-hook (lambda ()
                                ;; (setq file-name-handler-alist my-file-name-handler-alist)
                                ;; reset garbage collection
                                ;; ref: https://www.reddit.com/r/emacs/comments/1h8879p/lowering_gcconsthreshold_could_help_relieve_gc/
                                (setq gc-cons-percentage 0.2
                                      gc-cons-threshold 80000000)
                                ;; Startup time
                                ))

;; turn off print command?
(put 'ns-print-buffer 'disabled nil)

;;*  User settings
;; Give Emacs some personal info
(setq-default user-full-name "Ilya Finkelstein"
              user-mail-address "ilya@finkelsteinlab.org")

