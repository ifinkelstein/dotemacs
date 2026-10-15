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
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :config
  (require 'vtable)
  (require 'transient)

;;** Helpers
  (defun my-activities--saved-time (activity)
    "Return the time ACTIVITY was last saved, preferring its last state."
    (let ((states (list (activities-activity-last activity)
                        (activities-activity-default activity))))
      (seq-some (lambda (state)
                  (when state
                    (map-elt (activities-activity-state-etc state) 'time)))
                states)))

  (defun my-activities--buffers (activity)
    "Return buffer names and file names recorded in ACTIVITY's saved states."
    (let (result)
      (dolist (state (list (activities-activity-default activity)
                           (activities-activity-last activity)))
        (dolist (item (and state (flatten-tree (activities-activity-state-window-state state))))
          (when (activities-buffer-p item)
            (push (activities-buffer-name item) result)
            (when-let* ((file (activities-buffer-filename item)))
              (push (abbreviate-file-name file) result)))))
      (delete-dups (nreverse result))))

  (defun my-activities--match-p (regexp activity)
    "Return non-nil if REGEXP matches ACTIVITY's name or one of its buffers."
    (or (string-match-p regexp (activities-activity-name activity))
        (seq-some (lambda (name) (string-match-p regexp name))
                  (my-activities--buffers activity))))

  (defun my-activities--matching (regexp)
    "Return activities whose name or buffers match REGEXP."
    (seq-filter (apply-partially #'my-activities--match-p regexp)
                (map-values activities-activities)))

  (defun my-activities--discard (activity)
    "Discard ACTIVITY without confirmation, closing it first if active."
    (when (activities-activity-active-p activity)
      (ignore-errors (activities-close activity)))
    (setf activities-activities
          (map-delete activities-activities (activities-activity-name activity))))

;;** Global commands
  (defun my-activities-resume-reset (activity)
    "Resume ACTIVITY in its default state, discarding its last state."
    (interactive (list (activities-completing-read :prompt "Resume activity (reset)" :default nil)))
    (activities-resume activity :resetp t))

  (defun my-activities-find (regexp)
    "Resume an activity whose name or saved buffers match REGEXP."
    (interactive (list (read-regexp "Find activity (name or buffer)")))
    (let ((matches (my-activities--matching regexp)))
      (unless matches
        (user-error "No activity matches %S" regexp))
      (activities-resume
       (activities-completing-read
        :activities (mapcar (lambda (activity)
                              (cons (activities-activity-name activity) activity))
                            matches)
        :prompt (format "Resume activity matching %S" regexp)
        :default nil))))

  (defun my-activities-show-buffers (activity)
    "Show the buffers and files recorded in ACTIVITY's saved states."
    (interactive (list (activities-completing-read :prompt "Show buffers of activity")))
    (let ((buffers (my-activities--buffers activity))
          (name (activities-activity-name activity)))
      (unless buffers
        (user-error "Activity %S has no saved buffers" name))
      (with-output-to-temp-buffer "*Activity buffers*"
        (princ (format "Buffers in activity %S:\n\n" name))
        (dolist (buffer buffers)
          (princ (concat "  " buffer "\n"))))))

  (defun my-activities-discard-many (activities)
    "Discard ACTIVITIES permanently after a single confirmation.
Interactively, choose them with `completing-read-multiple'."
    (interactive
     (list (mapcar (lambda (name) (map-elt activities-activities name))
                   (completing-read-multiple "Discard activities: " (activities-names) nil t))))
    (unless activities
      (user-error "No activities selected"))
    (let ((names (mapcar #'activities-activity-name activities)))
      (when (yes-or-no-p (format "Discard %d activities permanently (%s)? "
                                 (length names) (string-join names ", ")))
        (mapc #'my-activities--discard activities)
        (activities--persist)
        (message "Discarded %d activities" (length names)))))

  (defun my-activities-discard-stale (days)
    "Discard inactive activities whose saved state is older than DAYS days."
    (interactive (list (read-number "Discard inactive activities older than (days): " 90)))
    (let* ((cutoff (time-subtract nil (days-to-time days)))
           (stale (seq-filter (lambda (activity)
                                (and (not (activities-activity-active-p activity))
                                     (let ((saved (my-activities--saved-time activity)))
                                       (or (null saved) (time-less-p saved cutoff)))))
                              (map-values activities-activities))))
      (unless stale
        (user-error "No inactive activities older than %d days" days))
      (my-activities-discard-many stale)))

;;** List buffer
  ;; `activities-list' leaves the *Activities* buffer in fundamental-mode; the
  ;; row keys (RET k s D n p q) live in a text-property keymap that vtable
  ;; attaches to the rows.  That map shadows the local map only for keys it
  ;; defines, so new keys go in a local map installed by advice below.
  (defvar-local my-activities-list-filter-regexp nil
    "Regexp restricting the rows shown in the *Activities* list, or nil for all.")

  (defun my-activities-list--objects ()
    "Return the activities to list, honoring `my-activities-list-filter-regexp'."
    (if my-activities-list-filter-regexp
        (my-activities--matching my-activities-list-filter-regexp)
      (map-values activities-activities)))

  (defun my-activities-list--current ()
    "Return the activity at point in the list, or signal a user error."
    (or (vtable-current-object)
        (user-error "No activity at point")))

  (defmacro my-activities-list-define (name command &rest args)
    "Define NAME as a command calling COMMAND with the activity at point and ARGS.
The list buffer is reverted afterwards, as in `activities-list-command'."
    (declare (indent defun))
    `(defun ,name ()
       ,(format "Call `%s' on the activity at point and revert the list." command)
       (interactive)
       (let ((list-buffer (current-buffer)))
         (,command (my-activities-list--current) ,@args)
         (with-current-buffer list-buffer
           (vtable-revert-command)))))

  (my-activities-list-define my-activities-list-resume activities-resume)
  (my-activities-list-define my-activities-list-resume-reset activities-resume :resetp t)
  (my-activities-list-define my-activities-list-suspend activities-suspend)
  (my-activities-list-define my-activities-list-kill activities-kill)
  (my-activities-list-define my-activities-list-discard activities-discard)
  (my-activities-list-define my-activities-list-show-buffers my-activities-show-buffers)

  (defun my-activities-list-rename (name)
    "Rename the activity at point to NAME and revert the list."
    (interactive
     (list (read-string (format "Rename activity %S to: "
                                (activities-activity-name (my-activities-list--current))))))
    (activities-rename (my-activities-list--current) name)
    (vtable-revert-command))

  (defun my-activities-list-filter (regexp)
    "Show only activities whose name or buffers match REGEXP.
An empty REGEXP shows all activities again."
    (interactive
     (list (read-regexp "Filter activities (name or buffer, empty to clear)"
                        my-activities-list-filter-regexp)))
    (let ((regexp (unless (string-empty-p regexp) regexp)))
      (when (and regexp (null (my-activities--matching regexp)))
        (user-error "No activity matches %S" regexp))
      (setq my-activities-list-filter-regexp regexp
            mode-line-process (and regexp (format " [filter: %s]" regexp)))
      (vtable-revert-command)))

  (transient-define-prefix my-activities-list-transient ()
    "Commands available in the *Activities* list buffer."
    [["Activity at point"
      ("RET" "Resume" my-activities-list-resume)
      ("R" "Resume (reset)" my-activities-list-resume-reset)
      ("b" "Show buffers" my-activities-list-show-buffers)
      ("r" "Rename" my-activities-list-rename)
      ("s" "Suspend" my-activities-list-suspend)
      ("k" "Kill" my-activities-list-kill)
      ("D" "Discard" my-activities-list-discard)]
     ["List"
      ("/" "Filter" my-activities-list-filter)
      ("g" "Revert" vtable-revert-command)
      ("S" "Sort by column" vtable-sort-by-current-column)
      ("N" "New activity" activities-new)
      ("X" "Discard many" my-activities-discard-many)
      ("x" "Discard stale" my-activities-discard-stale)
      ("q" "Bury" bury-buffer)]])

  (defvar-keymap my-activities-list-map
    :doc "Extra keys for the *Activities* list; vtable's row keymap takes precedence."
    "?" #'my-activities-list-transient
    "/" #'my-activities-list-filter
    "R" #'my-activities-list-resume-reset
    "b" #'my-activities-list-show-buffers
    "r" #'my-activities-list-rename
    "N" #'activities-new
    "X" #'my-activities-discard-many
    "x" #'my-activities-discard-stale)

  (defun my-activities-list--setup (&rest _)
    "Install the local keymap and filtering in the *Activities* buffer."
    (with-current-buffer "*Activities*"
      (use-local-map my-activities-list-map)
      (setq my-activities-list-filter-regexp nil
            mode-line-process nil)
      (when-let* ((table (save-excursion (goto-char (point-min)) (vtable-current-table))))
        (setf (vtable-objects-function table) #'my-activities-list--objects))))

  (advice-add 'activities-list :after #'my-activities-list--setup))

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
    (setopt xref-search-program 'ripgrep)))

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
  (org-agenda nil "t"))

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
         (if (get-buffer "*mu4e-headers*")
             (switch-to-buffer "*mu4e-headers*")
           (mu4e)))
        (t
         (tab-bar-new-tab)
         (tab-bar-rename-tab "Email")
         (mu4e)))
  
  ;; move to second location 
  (tab-bar-move-tab-to 2))

;;* provide my-setup-projects
(provide 'my-setup-projects)

