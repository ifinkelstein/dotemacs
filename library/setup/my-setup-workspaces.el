;;; my-setup-workspaces.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein


;;; Commentary:

;;; Code:

;;;; Workspace Functions
;; inspired by Colin McLear's .emacs setup:
;; https://github.com/mclear-tools/dotemacs/blob/master/my-setup-workspaces.el

;;;;; General Settings
;; rename frames with their major mode. This helps yabai not whig out
(setq-default frame-title-format '("%f [" mode-name "]"))

;;;;; Startup Workspaces
(defun my--workspace-setup ()
  "Set up workspace at startup."
  ;; Add *Messages* and *splash* to Tab \`Home\'
  (progn
    (tab-bar-rename-tab "Home")
    (when (get-buffer "*Messages*")
      (set-frame-parameter nil
                           'buffer-list
                           (cons (get-buffer "*Messages*")
                                 (frame-parameter nil 'buffer-list))))
    (when (get-buffer "*splash*")
      (set-frame-parameter nil
                           'buffer-list
                           (cons (get-buffer "*splash*")
                                 (frame-parameter nil 'buffer-list))))))

(add-hook 'emacs-startup-hook #'my--workspace-setup)

(defun my-go-home ()
  "Go to home tab."
  (interactive)
  (tab-bar-switch-to-tab "Home"))

;;;;; Open Project in New Workspace
(defun my-open-existing-project-and-workspace ()
  "Open a project as its own workspace"
  (interactive)
  (progn
    (tab-bar-new-tab)
    (call-interactively 'project-switch-project-open-file)
    (tab-bar-rename-tab (tabspaces--name-tab-by-project-or-default))
    (project-magit-dir)))

;;;;;; Open Agenda as Workspace
(defun my-open-agenda-in-workspace ()
  "Open agenda in its own workspace. Create workspace, if necessary"
  (interactive)
  ;; load required functions
  (require 'org)
  (require 'org-super-agenda)
  (if (member "Agenda" (tabspaces--list-tabspaces))
      (progn
        (tab-bar-switch-to-tab "Agenda"))
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Agenda")
      (tab-move-to 1)))
  ;; load the inbox, gtd, and agenda
  (my-goto-gtd.org)
  (my-goto-inbox.org)
  (org-agenda nil "z"))

;;;;; Open emacs.d in Workspace
(defun my-open-emacsd-in-workspace ()
  "Open emacs.d in its own workspace"
  (interactive)
  (if (member "emacs.d" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "emacs.d")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "emacs.d")
      (find-file my-config-file)
      (split-window-right)
      (other-window 1)
      (project-magit-dir))))

;;;;; Open Notes in Workspace

(defun my-open-notes-in-workspace ()
  "Open notes dir in its own workspace"
  (interactive)
  (if (member "Notes" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "Notes")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Notes")
      (org-roam-dailies-goto-today)
      (end-of-buffer))))

;;;;; Elfeed Workspace
(defun my-open-elfeed-in-workspace ()
  "Open Elfeed in its own workspace."
  (interactive)
  (cond ((member "Elfeed" (tabspaces--list-tabspaces))
         (tab-bar-switch-to-tab "Elfeed"))
        (t
         (tab-bar-new-tab)
         (tab-bar-rename-tab "Elfeed")
         (elfeed)
         (elfeed-update))))

;;;;; Terminal Workspace
(defun my-vterm-workspace ()
  "Open vterm in home dir in its own workspace"
  (interactive)
  (let ((default-directory "~/"))
    (require 'multi-vterm)
    (multi-vterm-next)))

(defun my-open-new-terminal-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (if (member "Terminal" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "Terminal")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Terminal")
      (my-vterm-workspace)
      (delete-other-windows))))

;;;;; Eshell Workspace
(defun my-open-new-eshell-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (if (member "Eshell" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "Eshell")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Eshell")
      (my-eshell-home)
      (rename-buffer "eshell-workspace")
      (delete-other-windows))))

;;;;; Open Mu4e Email in Workspace
(defun my-open-email-in-workspace ()
  "Open mu4e email in its own workspace"
  (interactive)
  (cond ((member "Email" (tabspaces--list-tabspaces))
         (tab-bar-switch-to-tab "Email")
         (cond ((get-buffer "*mu4e-headers*")
                (switch-to-buffer "*mu4e-headers*"))
               ((get-buffer " *mu4e-main*")
                (switch-to-buffer " *mu4e-main*")
                (delete-other-windows))
               (t (mu4e))))
        (t
         (tab-bar-new-tab)
         (tab-bar-rename-tab "Email")
         (require 'org) ; need this for loading?
         (find-file (concat org-directory "mail.org"))
         (mu4e)
         (switch-to-buffer "*davmail*")
         (switch-to-buffer " *mu4e-main*"))))

;;;;; Open New Buffer & Workspace
;; This function is a bit weird; It creates a new buffer in a new workspace with a
;; dummy git project to give the isolation of buffers typical with a git project
;; I'm sure there is a more elegant way to do this but I don't know how :)
(defun my-open-new-buffer-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (tab-bar-new-tab)
  (tab-bar-rename-tab-tab "New project")
  (let ((my-project-temp-dir "/tmp/temp-projects/"))
    (progn
      (when (not (file-exists-p my-project-temp-dir))
        (make-directory my-project-temp-dir t))
      (when (not (file-exists-p (concat my-project-temp-dir ".git/")))
        (magit-init my-project-temp-dir))
      (when (not (file-exists-p (concat my-project-temp-dir "temp")))
        (with-temp-buffer (write-file (concat my-project-temp-dir "temp")))))
    (setq default-directory my-project-temp-dir)
    (find-file (concat my-project-temp-dir "temp"))))

;;;; Workspace Hooks
;; (advice-add 'tab-bar-rename-tab :after #'my-tabs--create-scratch-tab)
(defun my-tabs--create-scratch-tab ()
  "Create a scratch buffer for every tab workspace."
  (get-buffer-create (concat "*scratch-" (string-trim (tabspaces--name-tab-by-project-or-default) "[\\*]" "[\\*]") "*")))

;;;; Workspace Shortcuts (Splash)
;; Rebind splash keys to personal functions
(with-eval-after-load 'my-setup-splash
  (bind-keys :map my-splash-mode-map
    ("a" . my-open-agenda-in-workspace)
    ("c" . my-open-emacsd-in-workspace)
    ("e" . my-open-elfeed-in-workspace)
    ("m" . my-open-email-in-workspace)
    ("n" . my-open-notes-in-workspace)
    ("p" . my-open-existing-project-and-workspace)
    ("q" . my-splash-screen-bury)
    ("s" . my-open-slack-in-workspace)
    ("[esc]" . my-splash-screen-bury)
    ("k" . my-splash-screen-kill)))

;;;; Workspace Keybindings

;; See user config for keybinds
(bind-keys :map my+leader-map
  ("1" . my-open-agenda-in-workspace)
  ("2" . my-open-email-in-workspace)
  ("3" . my-open-notes-in-workspace)
  ("4" . my-open-new-terminal-and-workspace)
  ("5" . my-open-elfeed-in-workspace)
  ("6" . my-open-dir-in-workspace) ;; TODO: write this
  ("7" . my-open-emacsd-in-workspace))


;; additional keybindings for workspaces
(defun my-move-tab-to ()
  "Prompt for where to move tab to, and move there.
Tab numbering starts at 1."
  (interactive)
  (tab-move-to (read-number "Move tab to:" 1)))


(bind-keys :map my+workspace-keys
  ("m" . my-move-tab-to)
  ("n" . tab-bar-rename-tab)
  )
(bind-key "N" #'my-open-new-buffer-and-workspace 'project-prefix-map)

(provide 'my-setup-workspaces)
