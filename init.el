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

;;* System Variables
;; Check the operating system
(defconst sys-mac     (eq system-type 'darwin))

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
straight) and by `my-cache-dir'.")

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
(dolist (dir (list my-library-dir my-var-dir my-cache-dir my-user-dir my-setup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Add all configuration files to load-path
(eval-and-compile
  (progn
    (push my-setup-dir load-path)
    (push my-user-dir load-path)))

;; where to store private or "secret" info
(load (expand-file-name "private.el" my-user-dir) t)

;;* Package Management
;; Load the package-system.
(require 'package)

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
;; Let MELPA packages (e.g. transient) override stale built-in versions
(setopt package-install-upgrade-built-in t
        ;; But keep org from ELPA — MELPA org can conflict with the built-in
        package-pinned-packages '((org . "elpa")))
;; initialize the package system
(package-initialize)
;;** Use-Package Settings
(use-package use-package
  :custom
  ;; Don't automatically defer
  (use-package-always-defer nil)
  ;; Report loading details
  (use-package-verbose t)
  ;; auto-install packages
  (use-package-always-ensure t)
  ;; Navigate use-package declarations w/imenu
  (use-package-enable-imenu-support t))

;;* UI settings
;; Disable certain byte compiler warnings to cut down on the noise. This is a
;; personal choice and can be removed if you would like to see any and all byte
;; compiler warnings.
;; NOTE: Setopt won't work here
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Don't produce backtraces when errors occur.
;; This can be set to `t' interactively when debugging.
(setopt debug-on-error nil)

;;* Security
;; Properly verify outgoing ssl connections.
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(use-package gnutls
  :ensure nil
  :defer 1
  :custom
  (gnutls-verify-error t))

;;* Load configuration modules
;; This section loads a series of elisp libraries or 'modules' as defined below.

(message "
======================================================
Loading modules
======================================================")
;; this order is meaningful, as things may break at some point
(defmacro my-require (feature)
  "Require FEATURE with before/after messages and error trapping."
  `(condition-case err
       (progn
         (message ">> Loading %s..." ',feature)
         (require ',feature)
         (message ">> Loading %s...done" ',feature))
     (error (display-warning 'init (format "FAILED loading %s: %s" ',feature err) :error))))

(my-require my-setup-server)
(my-require my-setup-ui)
(my-require my-setup-functions)
(my-require my-setup-navigation)
(my-require my-setup-bookmarks)
(my-require my-setup-buffers)
(my-require my-setup-search)
(my-require my-setup-completion)
(my-require my-setup-keybindings)
(my-require my-setup-programming)
(my-require my-setup-org)

(my-require my-setup-dired)
(my-require my-setup-notes)
(my-require my-setup-projects)

(my-require my-setup-reading)
(my-require my-setup-writing)
(my-require my-setup-latex)
(my-require my-setup-pdf)
(my-require my-setup-vc)
(my-require my-setup-shell)
(my-require my-setup-mail)

(my-require my-setup-ai)
(my-require my-setup-media)

(my-require my-setup-slack)


;;** MacOS settings - defer load until after init.
(when sys-mac
  (my-require my-setup-macos))

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
              (setq-local page-delimiter ";;\\*+")
              ;; heading alist allows for subtree-like folding
              (setq-local outline-heading-alist
                          '((";;* " . 1)
                            (";;** " . 2)
                            (";;*** " . 3)
                            (";;**** " . 4)
                            (";;***** " . 5))))))


;;* After Startup
;; reset garbage collection
;; ref: https://www.reddit.com/r/emacs/comments/1h8879p/lowering_gcconsthreshold_could_help_relieve_gc/
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-percentage 0.2
                                      gc-cons-threshold 80000000)
                                ;; Startup time
                                ))

;;*  Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;*  User settings
;; user-full-name and user-mail-address are set in private.el
