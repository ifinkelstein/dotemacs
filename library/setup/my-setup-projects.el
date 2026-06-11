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
             project-switch-project)
  :custom
  (project-list-file (concat my-cache-dir "projects"))
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             (project-vc-dir "VC-Dir")
                             (magit-project-status "Magit status")))
  (project-vc-extra-root-markers '(".dir-locals.el" ".project.el" "package.json" "requirements.txt" "autogen.sh"))

  :config
  ;; Use Ripgrep if installed
  (when (executable-find "rg")
    (setopt xref-search-program 'ripgrep))
  ;; remove deleted projects from list
  (project-forget-zombie-projects))

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
  (with-eval-after-load 'consult
    ;; Set the consult-workspace buffer list source.
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
      "Set workspace buffer list for consult-buffer.")

    (defun my--consult-tabspaces ()
      "Deactivate isolated buffers when not using tabspaces."
      (cond (tabspaces-mode
             ;; hide full buffer list (still available with "b")
             (consult-customize consult-source-buffer :hidden t :default nil)
             (add-to-list 'consult-buffer-sources 'consult--source-workspace))
            (t
             (consult-customize consult-source-buffer :hidden nil :default t)
             (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))
    (add-hook 'tabspaces-mode-hook #'my--consult-tabspaces)))

;;* Workspaces
;; not all tabs are activities
;; eg., I like tabs for:
;; - agenda
;; - notes
;; - [x] mail

;;** Open Agenda + Notes + Email in Workspaces
(defun my-open-agenda-notes-email-in-workspaces ()
  "Open Agenda, Notes, and Email workspaces.
Email is opened last because mu4e starts asynchronously."
  (interactive)
  (my-open-agenda-in-workspace)
  (my-open-notes-in-workspace)
  (my-open-email-in-workspace))

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
      (goto-char (point-max)))))

;;** Terminal Workspace
(defun my-open-terminal-in-workspace ()
  "Open ghostel terminal in its own workspace."
  (interactive)
  (if (member "Terminal" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "Terminal")
    (tab-bar-new-tab)
    (tab-bar-rename-tab "Terminal")
    (let ((default-directory "~/"))
      (ghostel))
    (delete-other-windows)))

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

