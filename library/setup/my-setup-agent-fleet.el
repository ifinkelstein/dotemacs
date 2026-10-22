;;; my-setup-agent-fleet.el -*- lexical-binding: t -*-

;;* Agent fleets
;; A fleet is a set of folders, each running one agy session in its own tab.
;; Every tab is an activity named FLEET/AGENT, and a fleet's tabs share a tab
;; group named FLEET.  `my-agent-fleet-stop' saves and closes them all and
;; `my-agent-fleet-start' brings them back; agy's bookmark handler resumes
;; each agent's conversation.  `my-agent-sidebar' lists the agents and what
;; each one is doing.

(require 'seq)
(require 'subr-x)

(declare-function activities-named "activities" (name))
(declare-function activities-resume "activities" (activity &rest keys))
(declare-function activities-suspend "activities" (activity))
(declare-function activities-define "activities" (name &rest keys))
(declare-function activities-activity-active-p "activities" (activity))
(declare-function activities-activity-name "activities" (activity))
(declare-function activities--persist "activities" (&optional forcep))
(declare-function my-activities--discard "my-setup-projects" (activity))
(declare-function agy--buffer-name "agy" (dir instance))
(declare-function agy--start "agy" (dir &optional switches force-name select))
(declare-function agy--term-kill "agy" (backend buffer))
(defvar activities-bookmark-store)
(defvar agy-display-window-fn)
(defvar agy-status)
(defvar agy-status-time)
(defvar agy--backend)

(defvar my-agent-fleets
  '(("evolution" :glob "~/Work/25 projects/evolution-*" :strip "evolution-"))
  "Agent fleets, as (NAME . PLIST).
:glob is a wildcard matching the fleet's folders.  :strip is a prefix
removed from folder names to name the agents.")

(defvar my-agent-fleet-stop-action 'kill
  "What stopping an agent does after saving and closing its tab.
`kill' ends its agy session; the conversation resumes on the next start.
`keep' leaves the session running in the background.")

(defvar my-agent-fleet-start-interval 0.5
  "Seconds between agents when starting a fleet.
Each tab finishes restoring before the next one opens.")

(defvar-local my-agent--seen-time nil
  "When this agy buffer was last shown in the selected window.")

;;** Agents
;; An agent is a plist with :fleet, :name, :dir and :activity.

(defvar my-agent--by-activity (make-hash-table :test #'equal)
  "Agents by activity name, filled whenever a fleet is listed.")

(defun my-agent-fleet--agents (fleet)
  "Agents of FLEET, one per folder matching its :glob."
  (let ((spec (or (alist-get fleet my-agent-fleets nil nil #'equal)
                  (user-error "No agent fleet named %S" fleet))))
    (mapcar (lambda (dir)
              (let* ((name (string-remove-prefix (or (plist-get spec :strip) "")
                                                 (file-name-nondirectory dir)))
                     (agent (list :fleet fleet :name name
                                  :dir (file-name-as-directory (file-truename dir))
                                  :activity (concat fleet "/" name))))
                (puthash (plist-get agent :activity) agent my-agent--by-activity)
                agent))
            (seq-filter #'file-directory-p
                        (file-expand-wildcards (expand-file-name (plist-get spec :glob)))))))

(defun my-agent--all ()
  "Agents of every fleet."
  (mapcan (lambda (fleet) (my-agent-fleet--agents (car fleet))) my-agent-fleets))

(defun my-agent--for-activity (name)
  "The agent whose activity is NAME, or nil."
  (when (assoc (car (split-string name "/")) my-agent-fleets)
    (or (gethash name my-agent--by-activity)
        (progn (my-agent--all)
               (gethash name my-agent--by-activity)))))

(defun my-agent--buffer (agent)
  "AGENT's live agy buffer, or nil."
  (when (featurep 'agy)
    (get-buffer (agy--buffer-name (plist-get agent :dir) nil))))

(defun my-agent--activity (agent)
  "AGENT's saved activity, or nil."
  (activities-named (plist-get agent :activity)))

(defun my-agent--state (agent)
  "What AGENT is doing: `stopped', `working', `waiting' or `idle'.
`waiting' means it finished a turn that has not been looked at yet."
  (let ((buffer (my-agent--buffer agent)))
    (if (not buffer)
        'stopped
      (with-current-buffer buffer
        (pcase agy-status
          ('working 'working)
          ('waiting (if (and my-agent--seen-time
                             (time-less-p agy-status-time my-agent--seen-time))
                        'idle
                      'waiting))
          (_ 'idle))))))

(defun my-agent--read (prompt)
  "Read an agent with PROMPT, offering FLEET/NAME for every fleet."
  (let ((agents (mapcar (lambda (agent) (cons (plist-get agent :activity) agent))
                        (my-agent--all))))
    (cdr (assoc (completing-read (format-prompt prompt nil) agents nil t) agents))))

(defun my-agent-fleet--read (prompt)
  "Read a fleet name with PROMPT, unless there is only one."
  (if (cdr my-agent-fleets)
      (completing-read (format-prompt prompt nil) my-agent-fleets nil t)
    (caar my-agent-fleets)))

;;** Starting and stopping

(defun my-agent--group-tab (activity &rest _)
  "Put the current tab in ACTIVITY's fleet group when ACTIVITY is an agent."
  (when-let* ((agent (my-agent--for-activity (activities-activity-name activity))))
    (tab-bar-change-tab-group (plist-get agent :fleet))))

(with-eval-after-load 'activities
  (advice-add 'activities-resume :after #'my-agent--group-tab))

(defun my-agent--after-open (agent)
  "Show or hide the sidebar in AGENT's tab and select its agy window."
  (my-agent-sidebar--sync)
  (when-let* ((buffer (my-agent--buffer agent))
              (window (get-buffer-window buffer)))
    (select-window window)))

(defun my-agent-open (agent)
  "Go to AGENT's tab, starting the agent when it is not running.
A saved agent resumes its tab and conversation; a new one gets a tab
running agy in its folder."
  (interactive (list (my-agent--read "Open agent")))
  (require 'agy)
  ;; A new tab built from the sidebar window would keep only the sidebar.
  (when (window-parameter nil 'window-side)
    (select-window (window-main-window)))
  (let ((activity (my-agent--activity agent)))
    (cond
     ((and activity (activities-activity-active-p activity))
      (activities-resume activity)
      (my-agent--after-open agent))
     (activity
      (activities-resume activity)
      ;; Activities restores the tab's windows from a timer.
      (run-at-time 0.1 nil #'my-agent--after-open agent))
     (t
      (tab-bar-new-tab)
      (let ((agy-display-window-fn (lambda (buffer)
                                     (switch-to-buffer buffer)
                                     (selected-window))))
        (if-let* ((buffer (my-agent--buffer agent)))
            (switch-to-buffer buffer)
          (agy--start (plist-get agent :dir) nil nil t)))
      (delete-other-windows)
      (let ((activities-bookmark-store nil))
        (activities-define (plist-get agent :activity)))
      (tab-bar-change-tab-group (plist-get agent :fleet))
      (my-agent--after-open agent)))
    (my-agent--refresh)))

(defun my-agent-stop (agent)
  "Save and close AGENT's tab, then apply `my-agent-fleet-stop-action'."
  (interactive (list (my-agent--read "Stop agent")))
  (when-let* ((activity (my-agent--activity agent))
              ((activities-activity-active-p activity)))
    (activities-suspend activity))
  (when-let* (((eq my-agent-fleet-stop-action 'kill))
              (buffer (my-agent--buffer agent)))
    (agy--term-kill (buffer-local-value 'agy--backend buffer) buffer))
  (my-agent--refresh))

(defun my-agent-fleet--start-next (agents count)
  "Open the first of AGENTS that is not already open, then the rest.
COUNT is how many have been opened so far."
  (let ((agent (car agents)))
    (cond
     ((null agents)
      (message "Agent fleet: started %d agent%s" count (if (= count 1) "" "s")))
     ((when-let* ((activity (my-agent--activity agent)))
        (activities-activity-active-p activity))
      (my-agent-fleet--start-next (cdr agents) count))
     (t
      (my-agent-open agent)
      (run-at-time my-agent-fleet-start-interval nil
                   #'my-agent-fleet--start-next (cdr agents) (1+ count))))))

(defun my-agent-fleet-start (fleet)
  "Open a tab for every agent of FLEET that is not open yet."
  (interactive (list (my-agent-fleet--read "Start fleet")))
  (require 'agy)
  (my-agent-fleet--start-next (or (my-agent-fleet--agents fleet)
                                  (user-error "Fleet %s matches no folders" fleet))
                              0))

(defun my-agent-fleet-stop (fleet)
  "Stop every agent of FLEET; see `my-agent-stop'.
When agents would be killed mid-turn, ask first."
  (interactive (list (my-agent-fleet--read "Stop fleet")))
  (let* ((agents (my-agent-fleet--agents fleet))
         (busy (seq-filter (lambda (agent) (eq (my-agent--state agent) 'working))
                           agents)))
    (when (and busy
               (eq my-agent-fleet-stop-action 'kill)
               (not (yes-or-no-p
                     (format "Still working: %s.  Stop anyway? "
                             (mapconcat (lambda (agent) (plist-get agent :name))
                                        busy ", ")))))
      (user-error "Fleet %s left running" fleet))
    (mapc #'my-agent-stop agents)
    (message "Agent fleet %s stopped" fleet)))

(defun my-agent-fleet-forget (fleet)
  "Stop FLEET and discard its saved tabs, so the next start begins afresh.
Conversations are kept by agy and still resume."
  (interactive (list (my-agent-fleet--read "Forget fleet")))
  (when (yes-or-no-p (format "Discard the saved tabs of fleet %s? " fleet))
    (my-agent-fleet-stop fleet)
    (dolist (agent (my-agent-fleet--agents fleet))
      (when-let* ((activity (my-agent--activity agent)))
        (my-activities--discard activity)))
    (activities--persist t)
    (my-agent--refresh)))

;;** Status in tabs and the sidebar

(defconst my-agent--marks
  '((working "◌" success)
    (waiting "●" warning)
    (stopped "·" shadow)
    (idle " " default))
  "Mark and face for each agent state.")

(defun my-agent-tab-label (tab)
  "Label for TAB when it holds an agent, as (MARK . NAME), else nil."
  (when-let* ((activity (alist-get 'activity (cdr tab)))
              (agent (my-agent--for-activity (activities-activity-name activity))))
    (cons (pcase (my-agent--state agent)
            ('working "◌ ")
            ('waiting "● ")
            (_ ""))
          (plist-get agent :name))))

(defun my-agent--mark-seen (&rest _)
  "Note that the selected window's agy buffer has been looked at."
  (with-current-buffer (window-buffer (selected-window))
    (when (bound-and-true-p agy-status)
      (let ((was-waiting (and (eq agy-status 'waiting)
                              (or (null my-agent--seen-time)
                                  (time-less-p my-agent--seen-time agy-status-time)))))
        (setq my-agent--seen-time (current-time))
        (when was-waiting
          (my-agent--refresh))))))

(defun my-agent--on-event (event)
  "Refresh the tab bar and sidebar after agy EVENT.
An agent finishing while it is in front of the user counts as seen."
  (when-let* ((buffer (plist-get event :buffer))
              ((buffer-live-p buffer))
              ((eq buffer (window-buffer (selected-window))))
              ((eq (frame-focus-state) t)))
    (with-current-buffer buffer
      (setq my-agent--seen-time (current-time))))
  (my-agent--refresh))

(defun my-agent--refresh-later ()
  "Refresh once this agy buffer is gone."
  (run-at-time 0 nil #'my-agent--refresh))

(defun my-agent--setup-buffer ()
  "Keep the tab bar and sidebar current when this agy buffer dies."
  (add-hook 'kill-buffer-hook #'my-agent--refresh-later nil t))

(add-hook 'window-selection-change-functions #'my-agent--mark-seen)
(add-hook 'window-buffer-change-functions #'my-agent--mark-seen)
(with-eval-after-load 'agy
  (add-hook 'agy-event-functions #'my-agent--on-event)
  (add-hook 'agy-start-hook #'my-agent--setup-buffer))

;;*** Sidebar

(defvar my-agent-sidebar-width 32
  "Width of the agent sidebar.")

(defvar my-agent-sidebar--shown nil
  "Non-nil when the sidebar should appear in every agent tab.")

(defconst my-agent-sidebar--buffer-name "*Agents*")

(defun my-agent--age (agent)
  "Time since AGENT's status last changed, as a short string."
  (let* ((buffer (my-agent--buffer agent))
         (time (and buffer (buffer-local-value 'agy-status-time buffer)))
         (secs (and time (float-time (time-since time)))))
    (cond ((null secs) "")
          ((< secs 60) "now")
          ((< secs 3600) (format "%dm" (/ secs 60)))
          ((< secs 86400) (format "%dh" (/ secs 3600)))
          (t (format "%dd" (/ secs 86400))))))

(defun my-agent-sidebar--current ()
  "The agent of the selected tab, or nil."
  (when-let* ((activity (alist-get 'activity (cdr (tab-bar--current-tab-find)))))
    (my-agent--for-activity (activities-activity-name activity))))

(defun my-agent-sidebar--render (&rest _)
  "Redraw the agent list, keeping point on the same line."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (current (plist-get (my-agent-sidebar--current) :activity)))
    (erase-buffer)
    (pcase-dolist (`(,fleet . ,_) my-agent-fleets)
      (let* ((agents (my-agent-fleet--agents fleet))
             (running (seq-count (lambda (agent) (my-agent--buffer agent)) agents)))
        (insert (propertize (format "%s  %d/%d\n" fleet running (length agents))
                            'face 'bold 'my-agent-fleet fleet))
        (dolist (agent agents)
          (pcase-let ((`(,mark ,face) (alist-get (my-agent--state agent) my-agent--marks)))
            (insert (propertize
                     (concat " " (propertize mark 'face face)
                             (propertize (format " %4s " (my-agent--age agent)) 'face 'shadow)
                             (propertize (plist-get agent :name)
                                         'face (cond ((equal (plist-get agent :activity) current)
                                                      'bold)
                                                     ((not (my-agent--buffer agent)) 'shadow))))
                     'my-agent agent 'my-agent-fleet fleet)
                    "\n")))
        (insert "\n")))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun my-agent--refresh ()
  "Redraw the tab bar and, when it exists, the sidebar."
  (force-mode-line-update t)
  (when-let* ((buffer (get-buffer my-agent-sidebar--buffer-name)))
    (with-current-buffer buffer
      (my-agent-sidebar--render))))

(defun my-agent-sidebar--agent ()
  "The agent on this line, or signal."
  (or (get-text-property (point) 'my-agent)
      (user-error "No agent on this line")))

(defun my-agent-sidebar--fleet ()
  "The fleet of this line, or signal."
  (or (get-text-property (point) 'my-agent-fleet)
      (user-error "No fleet on this line")))

(defun my-agent-sidebar-visit ()
  "Go to the agent on this line, starting it if needed."
  (interactive)
  (my-agent-open (my-agent-sidebar--agent)))

(defun my-agent-sidebar-stop ()
  "Stop the agent on this line."
  (interactive)
  (my-agent-stop (my-agent-sidebar--agent)))

(defun my-agent-sidebar-start-fleet ()
  "Start every agent of this line's fleet."
  (interactive)
  (my-agent-fleet-start (my-agent-sidebar--fleet)))

(defun my-agent-sidebar-stop-fleet ()
  "Stop every agent of this line's fleet."
  (interactive)
  (my-agent-fleet-stop (my-agent-sidebar--fleet)))

(defvar-keymap my-agent-sidebar-mode-map
  :doc "Keys in the agent sidebar."
  "RET" #'my-agent-sidebar-visit
  "<mouse-1>" #'my-agent-sidebar-visit
  "s" #'my-agent-sidebar-visit
  "k" #'my-agent-sidebar-stop
  "S" #'my-agent-sidebar-start-fleet
  "K" #'my-agent-sidebar-stop-fleet
  "q" #'my-agent-sidebar)

(define-derived-mode my-agent-sidebar-mode special-mode "Agents"
  "List of fleet agents and their status.
\\{my-agent-sidebar-mode-map}"
  (setq-local revert-buffer-function #'my-agent-sidebar--render
              truncate-lines t
              cursor-type nil
              bookmark-make-record-function #'my-agent-sidebar--bookmark-record)
  (hl-line-mode 1))

(defun my-agent-sidebar--buffer ()
  "The sidebar buffer, created and drawn if needed."
  (or (get-buffer my-agent-sidebar--buffer-name)
      (with-current-buffer (get-buffer-create my-agent-sidebar--buffer-name)
        (my-agent-sidebar-mode)
        (my-agent-sidebar--render)
        (current-buffer))))

;; Activities saves the sidebar with each agent tab; restoring just redraws it.
(defun my-agent-sidebar--bookmark-record ()
  "Bookmark record for the sidebar."
  ;; Freshly consed: `bookmark-make-record' modifies the record in place.
  (list nil (cons 'handler 'my-agent-sidebar--bookmark-handler)))

(defun my-agent-sidebar--bookmark-handler (_bookmark)
  "Restore the sidebar buffer."
  (set-buffer (my-agent-sidebar--buffer)))

(defun my-agent-sidebar--window ()
  "The sidebar's window in this tab, or nil."
  (get-buffer-window my-agent-sidebar--buffer-name))

(defun my-agent-sidebar--show ()
  "Show the sidebar on the left of this tab."
  (unless (my-agent-sidebar--window)
    (display-buffer-in-side-window
     (my-agent-sidebar--buffer)
     `((side . left) (slot . 0) (window-width . ,my-agent-sidebar-width)
       (preserve-size . (t . nil))
       (window-parameters . ((no-other-window . t)
                             (no-delete-other-windows . t)))))))

(defun my-agent-sidebar--sync (&rest _)
  "Show or hide the sidebar in an agent tab to match `my-agent-sidebar'."
  (when (my-agent-sidebar--current)
    (if my-agent-sidebar--shown
        (my-agent-sidebar--show)
      (when-let* ((window (my-agent-sidebar--window)))
        (delete-window window))))
  (my-agent--refresh))

(add-hook 'tab-bar-tab-post-select-functions #'my-agent-sidebar--sync)

(defun my-agent-sidebar ()
  "Toggle the agent sidebar; it follows you through the agent tabs."
  (interactive)
  (setq my-agent-sidebar--shown (not (my-agent-sidebar--window)))
  (if my-agent-sidebar--shown
      (my-agent-sidebar--show)
    (delete-window (my-agent-sidebar--window)))
  (my-agent--refresh))

;;* provide my-setup-agent-fleet
(provide 'my-setup-agent-fleet)
