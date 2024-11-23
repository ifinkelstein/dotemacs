;;; early-init.el --- summary -*- lexical-binding: t; no-byte-compile: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;; Author: Ilya Finkelstein
;; Version:0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


;;; Commentary:

;; This is the early-init file.
;; Targets emacs 29 or later

;;;; Speed up startup

;; Help speed up emacs initialization See
;; https://blog.d46.us/advanced-emacs-startup/ and
;; http://tvraman.github.io/emacspeak/blog/emacs-start-speed-up.html and
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; This will be set back to normal at the end of the init file

(defvar my-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;; Garbage collection
;; Defer garbage collection further back in the startup process. We'll lower
;; this to a more reasonable number at the end of the init process (i.e. at end of
;; init.el)

(setq gc-cons-threshold most-positive-fixnum)

;; Adjust garbage collection thresholds during startup, and thereafter
;; See http://akrl.sdf.org https://gitlab.com/koral/gcmh

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (let ((inhibit-message t))
                           (message "Garbage Collector has run for %.06fsec"
                                    (k-time (garbage-collect)))))))

;;;; Clean View
;; UI - Disable visual cruft

;; Resizing the Emacs frame can be an expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setopt frame-inhibit-implied-resize t
        ;; HACK: Don't show size info (or anything else) in frame title
        frame-title-format "\n"
        ;; Disable start-up screen
        inhibit-startup-screen t
        inhibit-startup-message t
        ;; We'll provide our own splash screen, thanks
        inhibit-splash-screen t
        ;; No message in initial scratch buffer
        initial-scratch-message nil)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setopt tool-bar-mode nil
        scroll-bar-mode nil)

;; Fundamental mode at startup.
;; This helps with load-time since no extra libraries are loaded.
(setopt initial-major-mode 'fundamental-mode)

;; Echo buffer -- don't display any message
;; https://emacs.stackexchange.com/a/437/11934
(defun display-startup-echo-area-message ()
  (message ""))

;;;; System Variables
;; Check the system used
(defconst sys-linux   (eq system-type 'gnu/linux))
(defconst sys-mac     (eq system-type 'darwin))
(defconst sys-win     (memq system-type '(cygwin windows-nt ms-dos)))

;;;; Directory Variables
;;  We're going to define a number of directories that are used throughout this
;;  configuration to store different types of files. This is a bit like the
;;  `no-littering' package, and allows us to keep `user-emacs-directory' tidy.

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

;; (defconst my-default-config-file (concat my-library-dir "my-default-config.el")
;;   "A sample default configuration of the personal config file to get the user started.")

;;;; User Configuration Variables

;; Find the user configuration file
(defconst my-config-file (expand-file-name "config.el" my-user-dir)
  "The user's configuration file.")

;;;; Make System Directories
;; Directory paths
(dolist (dir (list my-library-dir my-var-dir my-etc-dir my-cache-dir my-user-dir my-setup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;;; Load Path
;; Add all configuration files to load-path
(eval-and-compile
  (progn
    (push my-setup-dir load-path)
    (push my-user-dir load-path)))

;;;; Prefer Newer files
;; Prefer newer versions of files
(setopt load-prefer-newer t)

;;;; Byte Compile Warnings
;; Disable certain byte compiler warnings to cut down on the noise. This is a
;; personal choice and can be removed if you would like to see any and all byte
;; compiler warnings.
;; NOTE: Setopt won't work here
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))

;;;; Check Errors
;; Don't produce backtraces when errors occur.
;; This can be set to `t' interactively when debugging.
(setopt debug-on-error nil)

;;;; Custom Settings & Default Theme
;; Ordinarily we might leave theme loading until later in the init process, but
;; this leads to the initial frame flashing either light or dark color,
;; depending on the system settings. Let's avoid that by loading a default theme
;; before initial frame creation. The modus themes are built in and excellent.
;; NOTE: 1. The default theme is set only if there are no user configuration
;; files, otherwise it is left to the user to do; 2. This system check only
;; works for MacOS, with an emacs build with the ns-system-appearance patch. For
;; examples of such builds see https://github.com/mclear-tools/build-emacs-macos
;; or https://github.com/d12frosted/homebrew-emacs-plus

;; Use this variable for checking what the active them setting is vis-a-vis the
;; system light or dark mode.
(defvar active-theme nil "Variable for holding light/dark value of theme appearance.")
(defvar light-theme nil "Variable for holding light value of theme appearance.")
(defvar dark-theme nil "Variable for holding dark value of theme appearance.")

;;;; Bootstrap Package System
;; Load the package-system.
(require 'package)

;;;; Package Archives
;; See https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/ for discussion
(setopt package-archives
        '(("elpa" . "https://elpa.gnu.org/packages/")
          ("elpa-devel" . "https://elpa.gnu.org/devel/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/"))

        ;; Highest number gets priority (what is not mentioned gets priority 0)
        package-archive-priorities
        '(;; Prefer development packages
          ("elpa-devel" . 99)
          ("melpa" . 90))

        ;; Set location of package directory
        package-user-dir (expand-file-name "elpa/" my-var-dir)
        package-gnupghome-dir (concat package-user-dir "gnupg"))

;; Make sure the elpa/ folder exists after setting it above.
(unless (file-exists-p package-user-dir)
  (mkdir package-user-dir t))
(setopt package-quickstart-file (expand-file-name "package-quickstart.el" my-cache-dir))

;;;; Measure Time Macro
;; Useful macro to wrap functions in for testing
;; See https://stackoverflow.com/q/23622296
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "
;; ======================================================
;; %s *Elapsed time: %.06f*
;; ======================================================
" (if load-file-name
      (file-name-nondirectory (format "%s |" load-file-name))
    "")
(float-time (time-since time)))))

;;; early-init.el ends here
