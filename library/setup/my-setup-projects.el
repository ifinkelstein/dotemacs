;;; my-setup-projects.el -*- lexical-binding: t -*-

;;* Activities
;; manage frames/tabs, windows, and buffers according to their purpose. An
;; “activity” comprises a frame or tab, its window configuration, and the
;; buffers displayed in them–its “state”; this state would be related to a
;; certain task the user performs at various times, such as developing a certain
;; software project, reading and writing email, working with one’s Org mode
;; system, etc.
(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t) )

;;* Project
;; Use project to switch to, and search in, projects
(use-package project
  :ensure nil
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file)
  :custom
  (project-list-file (concat my-cache-dir "projects"))
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             (project-vc-dir "VC-Dir")
                             (project-magit-dir "Magit status")))
  (project-vc-extra-root-markers '(".dir-locals.el" ".project.el" "package.json" "requirements.txt" "autogen.sh"))

  :config
  ;; Use Ripgrep if installed
  (when (shell-command-to-string "command rg --version")
    (setq xref-search-program 'ripgrep))
  (setq my-project-dir "~/Work/")
  ;; remove deleted projects from list
  (project-forget-zombie-projects))

(defun my--project-name ()
  "Return name of project without path"
  (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-"))))

;; magit function for project
(defun project-magit-dir ()
  "Run magit in the current project's root"
  (interactive)
  (magit-status))

;;** Open project & file
(with-eval-after-load 'project
  (defun project-switch-project-open-file (dir)
    "Switch to another project by running an Emacs command.
Open file using project-find-file

When called in a program, it will use the project corresponding
to directory DIR."
    (interactive (list (project-prompt-project-dir)))
    (let ((default-directory dir)
          (project-current-inhibit-prompt t))
      (call-interactively 'project-find-file))))

;;* Isolate buffers to a tab with tabspaces
(use-package tabspaces
  ;; Add some functions to the project map
  :bind (:map project-prefix-map
              ("p" . tabspaces-open-or-create-project-and-workspace))
  :hook (emacs-startup . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Home")
  :config
  (defun my--consult-tabspaces ()
    "Deactivate isolated buffers when not using tabspaces."
    (require 'consult)
    (cond (tabspaces-mode
           ;; hide full buffer list (still available with "b")
           (consult-customize consult--source-buffer :hidden t :default nil)
           (add-to-list 'consult-buffer-sources 'consult--source-workspace))
          (t
           (consult-customize consult--source-buffer :hidden nil :default t)
           (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))
  (add-hook 'tabspaces-mode-hook #'my--consult-tabspaces))

;;;;; Consult Isolated Workspace Buffers
;; Filter Buffers for Consult-Buffer
(defun my-buff-filter (buffer)
  (let ((blst (cl-remove (buffer-name) (frame-parameter nil 'buffer-list))))
    (memq buffer blst)))

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                           :predicate #'tabspaces--local-buffer-p
                           :sort 'visibility
                           :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer."))

;;* Workspaces
;; not all tabs are activities
;; eg., I like tabs for:
;; - agenda
;; - notes
;; - [x] mail

;;** Open Agenda in Workspace
(defun my-open-agenda-in-workspace ()
  "Open agenda in its own workspace. Create workspace, if necessary"
  (interactive)
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
  (org-agenda nil "f"))

;;** Open Notes in Workspace
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

;;** Terminal Workspace
(defun my-vterm-workspace ()
  "Open vterm in home dir in its own workspace"
  (interactive)
  (let ((default-directory "~/"))
    (require 'multi-vterm)
    (multi-vterm-next)))

(defun my-open-terminal-in-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (if (member "Terminal" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "Terminal")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Terminal")
      (my-vterm-workspace)
      (delete-other-windows))))

;;** Open Mu4e Email in Workspace
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
         (mu4e)))
  
  ;; move to second location 
  (tab-bar-move-tab-to 2))

;;* provide my-setup-projects
(provide 'my-setup-projects)

