;;; theme.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein

;;; Commentary:

;; Theme settings & functions

;;; Code:

;;;; No-confirm themes
(setq custom-safe-themes t)

;;;; Custom Theme Folder
;;
(defcustom my-custom-themes-dir (concat my-user-dir "custom-themes/")
  "Set a custom themes directory path."
  :group 'my-emacs
  :type 'string)

;; Make the custom themes dir.
(mkdir my-custom-themes-dir t)
(setq-default custom-theme-directory my-custom-themes-dir)

;; find all themes recursively in custom-theme-folder
(let ((basedir custom-theme-directory))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'custom-theme-load-path (concat basedir f)))))

;;;; Disable All Custom Themes
(defun my-disable-all-themes ()
  "Disable all active themes & reset mode-line."
  (interactive)
  (progn
    (dolist (i custom-enabled-themes)
      (disable-theme i))
    ;; disable window-divider mode
    (window-divider-mode -1)
    ;; revert to mode line
    (setq-default header-line-format nil)
    (setq-default mode-line-format
                  '((:eval
                     (list
                      "%b "
                      "%m "
                      (cond ((and buffer-file-name (buffer-modified-p))
                             (propertize "(**)" 'face `(:foreground "#f08290")))
                            (buffer-read-only "(RO)" ))
                      " %l:%c %0"
                      " "
                      ))))
    (force-mode-line-update)))

;;;; Load Theme Wrapper
(defun my-load-theme ()
  (interactive)
  (progn
    (my-disable-all-themes)
    (call-interactively 'load-theme)))

;;;; Toggle Menubar
;; toggle menubar to light or dark
(defun my-osx-toggle-menubar-theme ()
  "Toggle menubar to dark or light using shell command."
  (interactive)
  (shell-command "dark-mode"))
(defun my-osx-menubar-theme-light ()
  "Turn dark mode off."
  (interactive)
  (shell-command "dark-mode off"))
(defun my-osx-menubar-theme-dark ()
  "Turn dark mode on."
  (interactive)
  (shell-command "dark-mode on"))

;;;; Theme & menubar toggle
(defun toggle-dark-light-theme ()
  "Coordinate setting of theme with os theme and toggle."
  (interactive)
  (if (eq active-theme 'light-theme)
      (progn (my-osx-menubar-theme-dark)
             (setq active-theme 'dark-theme))
    (progn (my-osx-menubar-theme-light)
           (setq active-theme 'light-theme))))

;;;; After Load Theme Hook
(defvar my-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'my-after-load-theme-hook))

;;;; My Themes
;; Set a default theme
(use-package lambda-themes
  :ensure nil
  :init
  (unless (package-installed-p 'lambda-themes)
    (package-vc-install "https://github.com/Lambda-Emacs/lambda-themes.git"))
  :custom
  ;; Custom settings. To turn any of these off just set to `nil'.
  (lambda-themes-set-variable-pitch t)
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  :custom-face
  (shadow ((t (:foreground "#aaaaaa")))) ; shadow font a bit more readable. inherited by dired, dirvish and others
  ) ;; use-package

;;;;; System Appearance Hook
;; See https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun my--system-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (progn
              (load-theme 'my-light)
              (setq active-theme 'light-theme)))
    ('dark (progn
             (load-theme 'my-dark)
             (setq active-theme 'dark-theme)))))

(defcustom my-ui-mac-system-theme nil
  "When `t' use theme that matches macOS system theme."
  :group 'my-emacs
  :type 'boolean)

;; Add the hook on MacOS
(when (and sys-mac
           my-ui-mac-system-theme)
  (add-hook 'ns-system-appearance-change-functions #'my--system-apply-theme))

;;;;; Define User Theme
(defcustom my-ui-theme nil
  "Default user theme."
  :group 'my-emacs
  :type 'symbol)

;; If set, load user theme, otherwise load my-themes
(cond ((bound-and-true-p my-ui-theme)
       (load-theme my-ui-theme t))
      ((eq active-theme 'light-theme)
       (load-theme 'lambda-light t))
      ((eq active-theme 'dark-theme)
       (load-theme 'lambda-dark t))
      (t
       (load-theme 'lambda-light t)))

;; kind-icon needs to have its cache flushed after theme change
(with-eval-after-load 'kind-icon
  (add-hook 'my-themes-after-load-theme-hook #'kind-icon-reset-cache))

;;; Provide
(provide 'my-setup-theme)
;;; theme.el ends here
