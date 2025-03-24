
(message "Setting up org mode...")
;; re-enable org-modern, lambda-line

;;* Org
;; Use built-in org
(use-package org
  :ensure nil
  :commands (org-mode org-capture org-attach-attach org-attach)
  :mode (("\\.org$" . org-mode))
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . unpackaged/org-table-face-mode))
  :bind
  (:map global-map
        ("C-c a" . org-agenda)
        ("C-c t" . my-org-capture-todo))
  (:map org-mode-map
        ("<C-M-return>" . org-insert-todo-heading-respect-content)
        ("C-M-n" . org-metaup)
        ("C-M-e" . org-metadown)
        ("C-M-o" . org-metaright)
        ("C-M-m" . org-metaleft)
        ("M-N" . org-shiftdown)
        ("M-E" . org-shiftup)
        ("M-I" . org-shiftright)
        ("M-M" . org-shiftleft)
        ("<s-return>" . unpackaged/org-return-dwim) ;; skip property drawer in headings
        ;; easily emphasize text
        ("s-b" . (lambda () (interactive) (org-emphasize-dwim ?*)))
        ("s-i" . (lambda () (interactive) (org-emphasize-dwim ?/)))
        ("s-u" . (lambda () (interactive) (org-emphasize-dwim ?_)))
        ("s-M-`" . org-emphasize-with-verbatim-dwim)
        ("s-M-~" . org-emphasize-with-code-dwim)
        ;; better pasting behavior in org-mode
        ("s-v" . org-yank))

  :init
  ;; Org-Emphasis-Regex settings. Set regex boundaries for emphasis.
  ;; Load this before org-mode is loaded.
  ;; See https://emacs.stackexchange.com/q/54673/11934
  ;; https://emacs.stackexchange.com/q/54632/11934
  (setq org-emphasis-regexp-components
        '("-—[:space:]('\"{["
          "\] - [:space:].,:!?;'\")}\\["
          "[:space:]"
          "."
          1))
  :custom
  ;; Aesthetics & UI
  (org-auto-align-tags nil) ;; don't auto-align tags
  (org-catch-invisible-edits 'smart) ;; prevent editing invisible area
  (org-cycle-separator-lines 0) ;; no empty lines in collapsed view
  (org-ellipsis "…") ;; nicer elipses "↷" "↴" "▼"
  (org-fontify-quote-and-verse-blocks t) ;; make quotes stand out
  (org-hide-emphasis-markers t)  ;; hide emph markers
  (org-hide-leading-stars t)  ;; hide leading stars
  (org-image-actual-width  500) ;; show all images at 500px using imagemagik
  (org-pretty-entities t) ;; make latex look good, etc.
  (org-pretty-entities-include-sub-superscripts t) ;; prettify sub/superscripts
  (org-read-date-prefer-future 'time) ;; Incomplete dates refer to future dates & times
  (org-startup-folded nil) ;; Don't start org in outline
  (org-startup-with-inline-images t) ;; load inline images when opening org file
  (org-tags-column 0) ;; place tags directly next to headline text
  (org-use-sub-superscripts nil) ;; disable sub/superscript displays in org

  ;; Footnotes
  (org-footnote-section nil) ;; place footnotes locally
  (org-footnote-auto-adjust t) ;; renumber footnotes

  ;; Indentation
  (org-adapt-indentation nil) ;; adapt indentation
  (org-startup-indented t) ;; start with indentation of headlines
  (org-src-preserve-indentation t) ;; preserve code indentation

  ;; Insertion/Yanking
  (org-insert-heading-respect-content t) ;; insert new headings after subtree
  (org-M-RET-may-split-line '((default . t)))  ;; don't split line when creating a new headline, list item, or table field
  (org-yank-adjusted-subtrees t)  ;; adjust subtrees to depth when yanked
  (org-yank-folded-subtrees t) ;; fold subtrees on yank

  ;; Lists
  (org-list-allow-alphabetical t) ;; allow alphabetical list
  ;; Demote sequence for list bullets
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (org-list-indent-offset 1) ;; increase sub-item indentation

  ;; Logging
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-log-redeadline nil) ;; don't log the time a task was rescheduled/redeadlined.
  (org-log-reschedule nil)

  ;; Movement
  (org-return-follows-link t) ;; make RET follow links
  (org-special-ctrl-a/e t)  ;; better movement in headers

  ;; Searching
  (org-imenu-depth 8) ;; scan to depth 8 w/imenu
  (imenu-auto-rescan t) ;; make sure imenu refreshes

  ;; Source block settings
  (org-src-fontify-natively t) ;; use lang-specific fontification
  (org-src-window-setup 'other-window) ;; edit source in other window
  (org-src-tab-acts-natively t) ;; use lang bindings
  (org-confirm-babel-evaluate t) ;; confirm evaluation

  ;; org speed command
  (org-use-speed-commands t)
  (org-speed-commands 
   '(("Outline Navigation")
     ("n" . (org-speed-move-safe 'org-next-visible-heading))
     ("e" . (org-speed-move-safe 'org-previous-visible-heading))
     ("f" . (org-speed-move-safe 'org-forward-heading-same-level))
     ("b" . (org-speed-move-safe 'org-backward-heading-same-level))
     ("F" . org-next-block)
     ("B" . org-previous-block)
     ("u" . (org-speed-move-safe 'outline-up-heading))
     ("j" . org-goto)
     ("g" . (org-refile '(4)))
     ("Outline Visibility")
     ("c" . org-cycle)
     ("C" . org-shifttab)
     (" " . org-display-outline-path)
     ("s" . org-toggle-narrow-to-subtree)
     ("k" . org-cut-subtree)
     ;; ("=" . org-columns)
     ("Outline Structure Editing")
     ("U" . org-metaup)
     ("D" . org-metadown)
     ("r" . org-metaright)
     ("l" . org-metaleft)
     ("R" . org-shiftmetaright)
     ("L" . org-shiftmetaleft)
     ("i" . (progn (forward-char 1) (call-interactively 'org-insert-heading-respect-content)))
     ("^" . org-sort)
     ("w" . org-refile)
     ("a" . org-archive-subtree-default-with-confirmation)
     ("m" . org-mark-subtree)
     ;; ("@" . org-mark-subtree)
     ;; ("#" . org-toggle-comment)
     ("Clock Commands")
     ("I" . org-clock-in)
     ("O" . org-clock-out)
     ("P" . org-pomodoro)
     ("Meta Data Editing")
     ("t" . org-todo)
     ("," . (org-priority))
     ("0" . (org-priority ?\ ))
     ("1" . (org-priority ?A))
     ("2" . (org-priority ?B))
     ("3" . (org-priority ?C))
     ("q" . org-set-tags-command)
     ("E" . org-set-effort)
     ;; ("E" . org-inc-effort)
     ;; ("W" . (lambda (m) (interactive "sMinutes before warning: ") (org-entry-put (point) "APPT_WARNTIME" m)))
     ("Agenda Views etc")
     ("v" . org-agenda)
     ("/" . org-sparse-tree)
     ("Misc")
     ;; ("<" . (org-agenda-set-restriction-lock 'subtree))
     ;; (">" . (org-agenda-remove-restriction-lock))  
     ("o" . org-open-at-point)
     ("?" . org-speed-command-help)))

  ;; TODOS
  (org-use-fast-todo-selection 'expert) ;; don't use popup window for todos
  ;; don't set to DONE if children aren’t DONE
  (org-enforce-todo-dependencies t)
  ;; (org-enforce-todo-checkbox-dependencies t) ;; allow todo to be done even if checkboxes are open
  :config
  ;; Load additional org modules
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-tempo t)
  (add-to-list 'org-modules 'org-protocol t)

  ;; set some faces to be fixed pitch font
  ;; removed org-todo bc it looked not great
  (dolist (face '(org-block org-table org-list-dt org-tag org-quote
                            org-code org-link  org-formula
                            org-verbatim org-checkbox org-meta-line org-meta-line
                            org-cite org-date org-hide))
    (set-face-attribute face nil :inherit 'fixed-pitch))

  ;; add a few more template expansions
  ;; These are invoked with C-c C-,
  (setq new-structure-template-alist
        '(("el" . "src emacs-lisp")
          ("b" . "src bash")))
  (dolist (ele new-structure-template-alist)
    (add-to-list 'org-structure-template-alist ele))

  (add-hook 'org-mode-hook (lambda ()  ;; don't pair < symbols
                             (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  ) ;; use-package org

;;* org setup ends here

;;** org-capture
(use-package org-capture
  :ensure nil
  :bind
  (:map org-capture-mode-map
        ("<s-return>" . unpackaged/org-return-dwim)))

;;** Org Agenda

(use-package org-agenda
  :ensure nil
  :commands (org-agenda)
  ;; Always highlight the current agenda line
  :hook ((org-agenda-mode . (lambda () (hl-line-mode 1))))
  :custom
  ;; Agenda logging
  (org-agenda-start-with-log-mode t)

  ;; Agenda styling
  (org-auto-align-tags t) ;; Align tags
  (org-agenda-tags-column 'auto) ;; Put tags to the right
  (org-agenda-breadcrumbs-separator "  ")
  (org-agenda-block-separator " ") ;; No default block seperator
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "–––––––––––––– Now")
  (org-agenda-compact-blocks t) ;; fit more information in the agend view

  ;; Display properties
  (org-agenda-tags-column org-tags-column)

  ;; hide inherited tags, but this makes it impossible to sort by these tags
  ;; so I need to change this locally in some agenda views
  (org-agenda-show-inherited-tags nil)

  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)

  ;; from stack overflow https://stackoverflow.com/a/22900459/6277148
  ;; note that the formatting is nicer that just using '%b'
  (org-agenda-prefix-format
   '((agenda . " %-18c%?-10t ")
     (timeline . "  % s")
     (todo . " %?-8c:%b %?s")
     (tags . " ")
     (search . " %i %-12:c")))

  ;; Scheduling
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-todo-ignore-deadlines 'far)
  ;; Sorting order for tasks on the agenda
  (org-agenda-sorting-strategy
   (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
           (todo category-up effort-up)
           (tags category-up effort-up)
           (search category-up))))
  ;; (org-agenda-sorting-strategy
  ;;  '((agenda time-up) (todo time-up) (tags time-up) (search time-up)))

  (calendar-week-start-day 1) ;; Start week on Monday

  ;; Agenda Custom Commands
  ;; Configure custom agenda views
  ;; https://orgmode.org/manual/Storing-searches.html#Storing-searches
  ;; https://systemcrafters.cc/emacs-from-scratch/organize-your-life-with-org-mode/
  ;; https://doc.norang.ca/org-mode.html#CustomAgendaViews
  (org-agenda-custom-commands
   (quote (("h" "Ongoing and Habits" tags-todo "ongoing"
            ((org-agenda-overriding-header "Habits")
             (org-agenda-sorting-strategy
              '(todo-state-down effort-up category-keep))))
           (" " "Agenda"
            ((agenda "" nil)
             (tags "REFILE"
                   ((org-agenda-overriding-header "Tasks to Refile")
                    (org-tags-match-list-sublevels nil)))
             (tags-todo "-CANCELLED/!"
                        ((org-agenda-overriding-header "Stuck Projects")
                         (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                         (org-agenda-sorting-strategy
                          '(category-keep))))
             (tags-todo "-HOLD-CANCELLED/!"
                        ((org-agenda-overriding-header "Projects")
                         (org-agenda-skip-function 'bh/skip-non-projects)
                         (org-tags-match-list-sublevels 'indented)
                         (org-agenda-sorting-strategy
                          '(category-keep))))
             (tags-todo "-CANCELLED/!NEXT"
                        ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                               (if bh/hide-scheduled-and-waiting-next-tasks
                                                                   ""
                                                                 " (including WAITING and SCHEDULED tasks)")))
                         (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                         (org-tags-match-list-sublevels t)
                         (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-sorting-strategy
                          '(todo-state-down effort-up category-keep))))
             (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                        ((org-agenda-overriding-header (concat "Project Subtasks"
                                                               (if bh/hide-scheduled-and-waiting-next-tasks
                                                                   ""
                                                                 " (including WAITING and SCHEDULED tasks)")))
                         (org-agenda-skip-function 'bh/skip-non-project-tasks)
                         (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-sorting-strategy
                          '(category-keep))))
             (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                        ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                               (if bh/hide-scheduled-and-waiting-next-tasks
                                                                   ""
                                                                 " (including WAITING and SCHEDULED tasks)")))
                         (org-agenda-skip-function 'bh/skip-project-tasks)
                         (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-sorting-strategy
                          '(category-keep))))
             (tags-todo "-CANCELLED+WAITING|HOLD/!"
                        ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                               (if bh/hide-scheduled-and-waiting-next-tasks
                                                                   ""
                                                                 " (including WAITING and SCHEDULED tasks)")))
                         (org-agenda-skip-function 'bh/skip-non-tasks)
                         (org-tags-match-list-sublevels nil)
                         (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
             (tags "-REFILE/"
                   ((org-agenda-overriding-header "Tasks to Archive")
                    (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                    (org-tags-match-list-sublevels nil))))
            nil)))))  ;; use-package org-agenda

;; ;;*** org-mode helper functions
;; ;; Agenda Jump to Dashboard
(defun my-jump-to-org-dashboard ()
  (interactive)
  (require 'org)
  (org-agenda nil "d"))

;; Agenda Refresh
;; automatically refresh the agenda after adding a task
(defun my-org-agenda-refresh ()
  (interactive)
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo)
      (message "[org agenda] refreshed!"))))
(add-hook 'org-capture-after-finalize-hook 'my-org-agenda-refresh)

;;  Org Capture
(defun my-org-capture-todo ()
  "Open capture window for a TODO item"
  (interactive)
  (org-capture nil "t"))

(defun my-capture-mail-gtd (msg)
  "Capture message as a TODO item"
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "m"))

(setq org-capture-templates
      ;; Note the ` and , to get concat to evaluate properly
      `(
        ("f" "Firefox Bookmark"
         entry (file+headline org-default-notes-file "Inbox")
         "* TODO %i %?\n:PROPERTIES:\n:Created: %U\n:END:\nReference: %(grab-mac-link 'firefox 'org)\n"
         :prepend t
         :empty-lines 1
         :created t)
        ("j" "Job Posting"
         entry (file+headline org-default-notes-file "Jobs")
         "* TODO %i %(grab-mac-link 'firefox 'org) \nDEADLINE: %^t\n:PROPERTIES:\n:Created: %U\n:END:\n %?"
         :prepend t
         :empty-lines 1
         :created t)
        ("t" "Todo"
         entry (file+headline org-default-notes-file "Inbox")
         "* TODO %i%? \n:PROPERTIES:\n:Created: %U\n:END:\n"
         :prepend t
         :empty-lines 1
         :created t)  ; template
        ("m" "Mail todo"
         entry (file+headline org-default-notes-file "Mail")
         "* TODO %i%? \n:PROPERTIES:\n:Created: %U\n:END:\nLink: %a\n"
         :prepend t
         :created t
         :empty-lines 1)
        ("W"
         "Weight"
         table-line
         (file+olp "/Users/ilya/Dropbox/org-roam/20210514211724-racing_weight.org" "Weight" "Data")
         "| %U | %? | "
         :unnarrowed t)))
;; enables meow insert mode when entering the org-capture interface. I've disabled modal editors for now.
(eval-after-load 'org-capture
  (progn
    (defun my-org-capture-turn-on-meow-insert-mode ()
      (meow-insert))
    (add-hook 'org-capture-mode-hook #'my-org-capture-turn-on-meow-insert-mode)))

;;** org-clock
(use-package org-clock
  :ensure nil
  :after org
  :custom
  (org-clock-history-length 23) ;; show more items in the org-clock dispatcher
  ;; Change tasks to NEXT when clocking in
  ;; (org-clock-in-switch-to-state 'bh/clock-in-to-next) ;TODO: fix underlying functions

  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (org-clock-persist t)
  ;; Include current clocking task in clock reports
  (org-clock-report-include-clocking-task t)
  :config

  ;; borrowed from:https://doc.norang.ca/org-mode.html#Clocking
  (defun bh/clock-in-to-next (kw)
    "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond
       ((and (member (org-get-todo-state) (list "TODO"))
             (bh/is-task-p))
        "NEXT")
       ((and (member (org-get-todo-state) (list "NEXT"))
             (bh/is-project-p))
        "TODO"))))  ) ;; use-package org-clock


;;* org-ql
;; search org files with a query language org-ql
(use-package org-ql
  :after (org) ;; for org agenda searches
  :demand t)

;;** org-ql associated functions
(defun my-org-agenda-next ()
  "Show the agenda block, followed by NEXT tasks in every project.
The NEXT tasks are sorted by priority."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (or (ts-active :on today)
              (deadline auto)
              (scheduled :to today))
          (not (done)))
    :title "My Agenda View"
    ;; The `org-super-agenda-groups' setting is used automatically when set, or it
    ;; may be overriden by specifying it here:
    :super-groups '((:name "bla"
                           :tag "writing")
                    (:todo ("NEXT" "TODO")
                           :order 7)
                    (:name "Personal"
                           :habit t
                           :tag "personal"
                           :order 3)
                    (:todo "WAITING"
                           :order 6)
                    (:priority "A" :order 1)
                    (:priority "B" :order 2)

                    (:priority "C" :order 2))))

;; inspiration for the functions below from here:
;; https://sachachua.com/blog/2024/01/using-consult-and-org-ql-to-search-my-org-mode-agenda-files-and-sort-the-results-to-prioritize-heading-matches/
(defun my-consult-org-ql-agenda-jump ()
  "Search agenda files with preview."
  (interactive)
  (require 'org-ql-search) ;; this will hopefully avoid loading errors

  (let* ((marker (consult--read
                  (consult--dynamic-collection
                   #'my-consult-org-ql-agenda-match)
                  :state (consult--jump-state)
                  :category 'consult-org-heading
                  :prompt "Heading: "
                  :sort nil
                  :lookup #'consult--lookup-candidate))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    ;; based on org-agenda-switch-to
    (unless buffer (user-error "Trying to switch to non-existent buffer"))
    (pop-to-buffer-same-window buffer)
    (goto-char pos)
    (when (derived-mode-p 'org-mode)
      (org-fold-show-context 'agenda)
      (run-hooks 'org-agenda-after-show-hook))))

(defun my-consult-org-ql-agenda-format (o)
  (propertize
   (org-ql-view--format-element o)
   'consult--candidate (org-element-property :org-hd-marker o)))

(defun my-consult-org-ql-agenda-match (string)
  "Return candidates that match STRING.
Sort heading matches first, followed by other matches.
Within those groups, sort by date and priority."
  (let* ((query (org-ql--query-string-to-sexp string))
         (sort '(date reverse priority))
         (heading-query (-tree-map (lambda (x) (if (eq x 'rifle) 'heading x)) query))
         (matched-heading
          (mapcar #'my-consult-org-ql-agenda-format
                  (org-ql-select 'org-agenda-files heading-query
                    :action 'element-with-markers
                    :sort sort)))
         (all-matches
          (mapcar #'my-consult-org-ql-agenda-format
                  (org-ql-select 'org-agenda-files query
                    :action 'element-with-markers
                    :sort sort))))
    (append
     matched-heading
     (seq-difference all-matches matched-heading))))
;;* Other org-mode addons
;;** svg-tag-mode
;; A minor mode to replace keywords or regular expression with SVG tags.
(use-package svg-tag-mode)
;;** Org-Appear (Show Markup/Pretty Entities)
;; show markup at point -- this should be part of org!
(use-package org-appear
  :after org
  :commands (org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t)
  :hook (org-mode . org-appear-mode))

;;** Org Autolist (Smart Lists)
;; Better list behavior
;; the fork fixes checkbox behavior
;; as of January 13, 2025, not merged into the main package
;; confirming that this fork works with - [ ] lists
(use-package org-autolist
  :vc (:url "https://github.com/ucizi-turintech/org-autolist" :rev :newest)
  :hook (org-mode . org-autolist-mode))


;;** Org Modern (Display properties, bullets, etc)
;; A nicer set of default display options
;; seems to be working after an update.
;; Need better org-mode variable pitch font
(use-package org-modern
  :vc (:url "https://github.com/minad/org-modern" :rev :newest)
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-hide-stars 'leading)
  (org-modern-todo nil)
  (org-modern-tag t)
  ;; Customize this per your font
  (org-modern-label-border .25)
  ;; Note that these stars allow differentiation of levels
  ;; "①" "②" "③" "④" "⑤" "⑥" "⑦"
  (org-modern-star 'replace)
  (org-modern-replace-stars ["⦶" "⦷" "⦹" "⊕" "⍟" "⊛" "⏣" "❂"])
  ;; 20241222: bug in org 9.7 and org-mode
  (org-modern-label-border nil) ;; a bug in org-modern prevents borders, set to nil for now
  :config
  ;; change dates to fixed pitch so they don't screw up tables
  (set-face-attribute 'org-modern-date-active nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-modern-date-inactive nil :inherit 'fixed-pitch))


;;** Org Contrib
;; (use-package org-contrib
;;   :after org
;;   :config
;;   ;; ignore export of headlines marked with :ignore: tag
;;   (require 'ox-extra)
;;   (ox-extras-activate '(ignore-headlines)))

;;** Org Export
;; Useful base export settings
(use-package ox
  :ensure nil
  :after org
  :custom
  ;; Don't use bad hyperref value
  ;; https://emacs.stackexchange.com/a/46226/11934
  (org-latex-hyperref-template nil)
  ;; Export settings
  (org-table-export-default-format "orgtbl-to-csv") ;; export for org-tables to csv
  (org-export-with-smart-quotes t)
  (org-export-with-broken-links t)

  (org-export-with-toc nil)
  (org-export-with-author nil)
  (org-export-with-title nil)


  (org-export-async-debug t)
  (org-html-postamble nil) ;; dont export postamble
  (org-export-async-init-file nil)
  (org-export-backends '(html latex pandoc md ascii)) ;;org-export-dispatcher menu
  ;; org v8 bundled with Emacs 24.4
  (org-odt-preferred-output-format "docx")
  :config
  ;; Only OSX need below setup
  (defun my-setup-odt-org-convert-process ()
    (interactive)
    (let ((cmd "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
      (when (and (eq system-type 'darwin) (file-exists-p cmd))
        ;; org v8
        (setq org-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i"))))))
  (my-setup-odt-org-convert-process))

;;** use pandoc with org export
(use-package ox-pandoc
  :if (executable-find "pandoc")
  :after (org ox)
  :custom
  (org-pandoc-command (executable-find "pandoc"))
  (org-pandoc-options '((standalone .  t)))
  (org-pandoc-options-for-docx '((standalone . nil)))
  ;; (org-pandoc-options-for-beamer-pdf '((pdf-engine . "lualatex")))
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "lualatex")))
  (org-pandoc-format-extensions '(org+smart)))
;;** Org Html Conversion

(use-package htmlize
  :commands (htmlize-buffer))

;;** Org ID
;; Use org ids for reference
(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-id-locations-file (concat my-cache-dir ".org-id-locations"))
  (org-id-method 'ts) ;; use timestamp for id
  (org-id-link-to-org-use-id 'create-if-interactive)) ;; create ids


;;** org-mac-link
(use-package org-mac-link
  :after (org)
  :if sys-mac
  :ensure t
  :config
  ;; Remove some org-mac-link options
  (setq org-mac-grab-Mail-app-p nil)
  (setq org-mac-grab-Outlook-app-p nil)
  (setq org-mac-grab-devonthink-app-p nil)
  (setq org-mac-grab-Together-app-p nil)
  (setq org-mac-grab-Skim-app-p nil)
  (setq org-mac-grab-qutebrowser-app-p nil)
  (setq org-mac-grab-Evernote-app-p nil))


;;** TODO Keywords and tags
;; tags, in alphabetical order
;; note: emoji tags not supported yet
(setq org-tag-alist '(("admin" . ?a)
                      ("class" . ?c)
                      ("email" . ?e)
                      ("errands" . ?r) 
                      ("service" . ?u) ;; university and other service
                      ("today" . ?t)
                      ("tomorrow" . ?T)
                      ("writing" . ?w)))
;; useful for adding multiple tags to an entry
(setq org-fast-tag-selection-single-key nil)
(customize-set-variable 'org-todo-keywords
                        '((sequence "TODO(t)" "WAITING(w)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))


;;** Org Inline Tasks
(use-package org-inlinetask :ensure nil
  :commands org-inlinetask-insert-task)

;;** Org Archive
;; Tell org where to archive completed tasks
(setq org-archive-location (concat org-directory "/org-archive/archived.org::datetree/"))
(setq org-archive-subtree-save-file-p t) ;; save archive file whenever something is archived
(setq org-archive-subtree-add-inherited-tags t) ;; save inherited tags when archiving a sub-Entry

;; Tell org how to archive all the done tasks (DONE or CANCELED) in a file.
;; From https://changelog.complete.org/archives/9877-emacs-3-more-on-org-mode
(defun my-org-archive-done-tasks ()
  "Archive all DONE or CANCELLED entries in the file"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

;;** Org Refile
;; Set refile settings.  I got a lot of help on this from Aaron Bieber's discussion.
;; See https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html.

;; With the below settings, you can trigger Refile with C-c C-w in any Org file
;; and get a completing read of all headings up to three levels deep in all
;; files in org-agenda-files. You can also refile to the top header in a
;; document and create new parents.
(use-package org-refile
  :ensure nil
  :after org
  :custom
  ;; first entry points to current buffer
  ;; tricky syntax help here:
  ;; https://www.reddit.com/r/orgmode/comments/u320fj/add_directory_instead_of_individual_files_to/
  (org-refile-targets `((nil :maxlevel . 9)
                        (,(directory-files org-directory 'full (rx ".org" eos)) :maxlevel . 8)))
  (org-refile-use-cache t)  ;; use cache for org refile
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  :config
  ;; Set a timer to regenerate the refile cache automatically every time Emacs is idled for 5 mins:
  ;; ref: https://yiming.dev/blog/2018/03/02/my-org-refile-workflow/
  (run-with-idle-timer 300 t (lambda ()
                               (org-refile-cache-clear)
                               (org-refile-get-targets))))

;;** Open Files in Default Application
;;Open files in their default applications (ms word being the prime example)
(customize-set-variable 'org-file-apps
                        '(("\\.docx\\'" . default)
                          ("\\.mm\\'" . default)
                          ("\\.x?html?\\'" . default)
                          ("\\.pdf\\'" . emacs)
                          (auto-mode . emacs)))

;;** Helper Functions
;; Copy kill text under heading
(defun my-org-kill-text-under-heading ()
  "Kill the text under the current org-mode heading. The function moves the
point to the next heading and kills the region between the current heading and
the next one, if any."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (forward-line)
    (unless (= (point) (point-max))
      (let ((b (point))
            (e (or (outline-next-heading) (point-max))))
        (kill-region b e)))))

(defun my-org-copy-text-under-heading ()
  "Copy the text under the current org-mode heading.
The function moves the point to the next heading and copies the
region between the current heading and the next one, if any."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (forward-line)
    (unless (= (point) (point-max))
      (let ((b (point))
            (e (or (outline-next-heading) (point-max))))
        (copy-region-as-kill b e)))))

;;** Org Emphasis Functions
;; Adapted from https://emacs.stackexchange.com/a/14586
;; See https://emacstil.com/til/2021/11/29/org-emphasize-dwim/
(defun org-emphasize-dwim (&optional char)
  (interactive)
  (unless (region-active-p)
    (my-maybe-mark-word))
  (org-emphasize char))

(defun org-emphasize-with-verbatim-dwim ()
  (interactive)
  (org-emphasize-dwim ?=))

(defun org-emphasize-with-code-dwim ()
  (interactive)
  (org-emphasize-dwim ?~))

(defun my--cursor-outside-of-any-word ()
  (not (bounds-of-thing-at-point 'word)))

(defun my--cursor-at-beginning-of-a-word ()
  (eq (point) (car (bounds-of-thing-at-point 'word))))

(defun my-maybe-mark-word ()
  "Mark the current word. If cursor is outside of a word bounds, mark the empty position."
  (interactive)
  (unless (or (my--cursor-outside-of-any-word) (my--cursor-at-beginning-of-a-word))
    (backward-word))
  (unless (my--cursor-outside-of-any-word)
    (mark-word)))

;;** Narrow & Advance/Retreat
;; Functions to advance forwards or backwards through narrowed tree
(defun my-org-advance ()
  (interactive)
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (org-forward-heading-same-level 1))
  (org-narrow-to-subtree))

(defun my-org-retreat ()
  (interactive)
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (org-backward-heading-same-level 1))
  (org-narrow-to-subtree))

;; Clone and Narrow
(defun my-clone-buffer-and-narrow ()
  "Clone buffer and narrow outline tree"
  (interactive)
  (let ((buf (clone-indirect-buffer-other-window nil nil)))
    (with-current-buffer buf
      (cond ((derived-mode-p 'org-mode)
             (org-narrow-to-element))
            ((derived-mode-p 'markdown-mode)
             (markdown-narrow-to-subtree))))
    (switch-to-buffer-other-window buf)))

;;  Goto Org Files
(defun my-goto-org-files ()
  "goto org-files directory"
  (interactive)
  (let ((default-directory org-directory))
    (call-interactively 'find-file)))
(defun my-goto-inbox.org ()
  "goto org-inbox"
  (interactive)
  (find-file (concat org-directory "inbox.org")))
(defun my-goto-todo.org ()
  "goto org-todo"
  (interactive)
  (find-file (concat org-directory "todo.org")))
(defun my-goto-conferences.org ()
  "goto org-conferences"
  (interactive)
  (find-file (concat org-directory "conferences.org")))
(defun my-goto-referee-reports.org ()
  "goto org referee reports"
  (interactive)
  (find-file (concat org-directory "referee-reports.org")))
(defun my-goto-reference.org ()
  "goto org reference notes"
  (interactive)
  (find-file (concat org-directory "reference.org")))
(defun my-goto-someday.org ()
  "goto org-someday"
  (interactive)
  (find-file (concat org-directory "someday.org")))
(defun my-goto-reading.org ()
  "goto reading list"
  (interactive)
  (find-file (concat org-directory "reading.org")))
(defun my-goto-writing.org ()
  "goto writing list"
  (interactive)
  (find-file (concat org-directory "writing.org")))
(defun my-goto-teaching.org ()
  "goto teaching file"
  (interactive)
  (find-file (concat org-directory "teaching.org")))

;; Org demote/promote region
(defun my-demote-everything (number beg end)
  "Add a NUMBER of * to all headlines between BEG and END.
    Interactively, NUMBER is the prefix argument and BEG and END are
    the region boundaries."
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (narrow-to-region beg end)
        (goto-char (point-min))
        (let ((string (make-string number ?*)))
          (while (search-forward-regexp "^\\*" nil t)
            (insert string)))))))

;;** Org Hide Property Drawers
;; From [[https://www.reddit.com/r/emacs/comments/9htd0r/how_to_completely_hide_the_properties_drawer_in/e6fehiw][Reddit]]

(defun my-org-toggle-properties ()
  "Toggle visibility of properties in current header if it exists."
  (save-excursion
    (when (not (org-at-heading-p))
      (org-previous-visible-heading 1))
    (when (org-header-property-p)
      (let* ((a (re-search-forward "\n\\:" nil t)))
        (if (outline-invisible-p (point))
            (outline-show-entry)
          (org-cycle-hide-drawers 'all))))))


;;** Org Create Check Box From List Item
;; A useful macro for converting list items to checkboxes
(fset 'my-org-checkbox-from-list
      [?a ?  ?\[ ?  ?\] escape ?\M-x return])

;;** Org Copy Link
;; see https://emacs.stackexchange.com/a/63038/11934
(defun my-org-link-copy-at-point ()
  (interactive)
  (save-excursion
    (let* ((ol-regex "\\[\\[.*?:.*?\\]\\(\\[.*?\\]\\)?\\]")
           (beg (re-search-backward "\\[\\["))
           (end (re-search-forward ol-regex))
           (link-string (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (kill-new link-string)
      (message "Org link %s is copied." link-string))))

;; Remove Org Links
;; https://emacs.stackexchange.com/a/10714/11934
(defun my-org-replace-link-by-link-description ()
  "Replace an org link by its description or, if empty, its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

;; Reschedule
(defun my-org-reschedule ()
  "Re-schedule an org item by first clering the scheduled field and then interactively prompting for a new date/time."
  (interactive)
  (org-schedule '(4)) ;; remove any scheduled items
  (org-schedule 0)) ;; schedule

(defun my-goto-gtd.org ()
  (interactive)
  (find-file (concat org-directory "gtd.org")))

(defun my-org-change-todo-region ()
  "Toggle TODO states in the current region or tree of an org file. This function
is interactive and allows users to switch TODO states for all entries in either
the current region, if a region is selected, or the current tree."
  (interactive)
  (let ((scope (if mark-active 'region 'tree))
        (state (org-fast-todo-selection))
        (org-enforce-todo-dependencies nil))
    (org-map-entries (lambda () (org-todo state)) nil scope)))

;;  Hooks
(defun my-org-mode-hooks ()
  "Functions to add to org-mode-hook."
  (visual-line-mode 1))
;; Make agenda more readable
(defun my-setup-org-agenda--set-line-spacing ()
  (setq-local default-text-properties '(line-spacing 0.20 line-height 1.20)))
(defun my-org--set-extra-faces ()
  "Make prop, etc., faces smaller."
  (mapc ;; This sets the fonts to a smaller size
   (lambda (face)
     (set-face-attribute face nil :height 0.8))
   (list
    'org-drawer
    'org-special-keyword
    'org-property-value)))
(add-hook 'org-agenda-mode-hook #'my-setup-org-agenda--set-line-spacing)
(add-hook 'org-agenda-mode-hook #'my-org--set-extra-faces)
(add-hook 'org-mode-hook #'my-org-mode-hooks)
;; * Third party org-mode mode
;;** Org-Web-Tools
;; Download snapshots of websites straight into an org file
(use-package org-web-tools)

;;** org-transclusion
;; include part of an org file in another file
(use-package org-transclusion
  :after (org org-roam)
  :config
  ;; The frige is likely overwritten by org-modern, per org-transclusion manual
  ;; (set-face-attribute
  ;;  'org-transclusion-fringe nil
  ;;  :foreground "green"
  ;;  :background "green")

  (set-face-attribute
   'org-transclusion nil
   :background "#f5f5dc")

  (set-face-attribute
   'org-transclusion-source nil
   :background "#f5f5dc"))


;;** TOC-Org
;; export TOCs in org and markdown files for GitHub
(use-package toc-org
  :hook ((org-mode . toc-org-mode)
         (markdown-mode . toc-org-mode)))
;;** org-bookmark-heading
;;Use the standard Emacs bookmark commands, C-x r m, etc, to mark org headings
(use-package org-bookmark-heading)

;;** org-sticky-header
(use-package org-sticky-header
  :disabled t
  :hook (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-full-path 'full))

;;** org appearance and ricing
;;*** Org-Appear (Show Markup/Pretty Entities)
;; show markup at point -- this should be part of org!
(use-package org-appear
  :disabled t
  :after org
  :commands (org-appear-mode)
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t))

;;* easier highlighting in org

;; This library provides two additional markers !!
;; and !@ over and above those in org-emphasis-alist.
;; Text enclosed in !! is highlighted in yellow, and exported likewise
;; Text enclosed in !@ is displayed in red, and exported likewise
;; (use-package org-extra-emphasis
;;   :vc (:url "https://github.com/QiangF/org-extra-emphasis" :rev :newest)) 

;;* org-sticky-header
(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-full-path 'full))


;; Better list behavior
(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

;;** org-super-agenda
(use-package org-super-agenda
  :after org
  :config
  ;; (setq org-super-agenda-groups nil)
  (org-super-agenda-mode))
;;*** settings for org-super-agenda
(setq org-agenda-custom-commands
      (quote (("f" "Focused"
               ((alltodo "" ((org-use-tag-inheritance t)
                             (org-agenda-dim-blocked-tasks nil) ;; speeds up agenda generation
                             (org-agenda-show-inherited-tags 'always) ;; makes sure org-super-agenda can search tags
                             (org-agenda-overriding-header "")
                             (org-super-agenda-groups
                              '((:name "Today"
                                       :and (:tag "today" :todo ("NEXT" "TODO"))                                       
                                       :order 10)
                                (:name "Next"
                                       :and (:tag "tomorrow" :todo ("NEXT" "TODO"))                                       
                                       :order 20)
                                (:discard (:anything t))))))))

              ("z" "Super View"
               ((agenda "" ((org-agenda-span 'day)
                            (org-super-agenda-groups
                             '((:name "Today"
                                      :time-grid t
                                      :date today
                                      :deadline today
                                      :deadline past
                                      :scheduled today
                                      :discard (:anything t)
                                      :order 1))) ))
                ;; use org-super-agenda to organize all agenda items in a bucket
                (alltodo "" ((org-use-tag-inheritance t)
                             (org-agenda-dim-blocked-tasks nil) ;; speeds up agenda generation
                             (org-agenda-show-inherited-tags 'always) ;; makes sure org-super-agenda can search tags
                             (org-agenda-overriding-header "")
                             (org-super-agenda-groups
                              '((:name "Refile"
                                       :tag "inbox"
                                       :order 10)
                                (:name "Lab and Administrative"
                                       :and (:category "Lab"
                                                       :todo "NEXT")
                                       :and (:category "Lab"
                                                       :todo "TODO")
                                       :and (:category "Lab"
                                                       :todo "WAITING")
                                       :order 20)

                                (:name "Grants"
                                       :and (:category "Grants"
                                                       :todo "NEXT")
                                       :and (:category "Grants"
                                                       :todo "TODO")
                                       :and (:category "Grants"
                                                       :todo "WAITING")
                                       :order 30)

                                (:name "Projects"
                                       :and (:category "Project"
                                                       :todo "NEXT")
                                       :and (:category "Project"
                                                       :todo "TODO")
                                       :and (:category "Project"
                                                       :todo "WAITING")
                                       :order 40)

                                ;; chores--only next or TODO
                                (:name "Chores"
                                       :and (:category "Chores"
                                                       :todo "NEXT")
                                       :and (:category "Chores"
                                                       :todo "TODO")
                                       :and (:category "Chores"
                                                       :todo "WAITING")
                                       :order 50)

                                (:name "Career"
                                       :and (:category "Career"
                                                       :todo "NEXT")
                                       :and (:category "Career"
                                                       :todo "TODO")
                                       :and (:category "Career"
                                                       :todo "WAITING")
                                       :order 60)

                                (:name "Family"
                                       :and (:category "Family"
                                                       :todo "NEXT")
                                       :and (:category "Family"
                                                       :todo "TODO")
                                       :and (:category "Family"
                                                       :todo "WAITING")
                                       :order 70)

                                (:name "Teaching"
                                       :and (:category "Teach"
                                                       :todo "NEXT")
                                       :and (:category "Teach"
                                                       :todo "TODO")
                                       :and (:category "Teach"
                                                       :todo "WAITING")
                                       :order 80)

                                (:name "Collaborations"
                                       :and (:category "collab"
                                                       :todo "NEXT")
                                       :and (:category "collab"
                                                       :todo "TODO")
                                       :and (:category "collab"
                                                       :todo "WAITING")
                                       :order 95)))))))
              ("a" "Agenda"
               ((agenda "" ((org-agenda-span 2)))
                (alltodo ""
                         ((org-agenda-overriding-header "")
                          (org-agenda-show-inherited-tags )
                          (org-super-agenda-groups
                           '((:name "Inbox, unscheduled"
                                    :and (:scheduled nil)
                                    :order 1)
                             (:name "Important, unscheduled"
                                    :and (:priority "A"
                                                    :scheduled nil)
                                    :order 2)

                             (:name "Project-related, unscheduled"
                                    :and (:tag "project" :date nil :todo ("STARTED" "WAITING" "TODO"))
                                    :order 3)
                             (:name "Waiting"
                                    :and (:todo "WAITING"
                                                :scheduled nil)
                                    :order 4)
                             (:discard (:todo "SOMEDAY"
                                              :category "cooking"
                                              :date t))
                             (:name "Unscheduled"
                                    :scheduled nil
                                    :order 5)
                             (:discard (:anything t))))))
                ;; (tags-todo "TODO=\"TODO\"-project-cooking-routine-errands-shopping-video-evilplans"
                ;;            ((org-agenda-skip-function 'my-org-agenda-skip-scheduled)
                ;;             (org-agenda-prefix-format "%-6e ")
                ;;             (org-agenda-overriding-header "Unscheduled TODO entries: ")
                ;;             (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))))
                ;; show all emacs tasks, with the top-most entry as NEXT, then TODO, then WAITING
                ("e" "Emacs" tags "emacs"
                 ((org-super-agenda-groups
                   '((:name "Next Action"
                            :todo "NEXT"
                            :order 1)
                     (:name "TODOs"
                            :todo "TODO"
                            :order 2)))))
                ("i" "Inbox" alltodo ""
                 ((org-agenda-files '("~/sync/orgzly/Inbox.org" "~/sync/orgzly/computer-inbox.org"))))

                ;; quick tasks, with NEXT first, but limited to five entries
                ;; then show all todo ENTRIES
                ;; finally, show everything else
                ("q" "Quick tasks" alltodo ""
                 ((org-agenda-overriding-header "")
                  (org-super-agenda-groups
                   '((:name "Next Action"
                            :take (5 (:and (:todo "NEXT"
                                                  :effort< "0:30")))
                            :order 1)
                     (:name "TODOs"
                            :and (:todo "TODO"
                                        :effort< "0:30")
                            :order 2)
                     (:name "Everything else"
                            :and (:not (:todo "TODO")
                                       :effort< "0:30")
                            :discard (:anything t)
                            :order 3)))))
                ("0" "Unestimated tasks" tags-todo "EFFORT=\"\"")
                ("d" "Timeline for today" ((agenda "" ))
                 ((org-agenda-ndays 1)
                  (org-agenda-show-log t)
                  (org-agenda-log-mode-items '(clock closed))
                  (org-agenda-clockreport-mode t)
                  (org-agenda-entry-types '())))
                ("." "Waiting for" todo "WAITING"))))))


;;** org-babel
(use-package ob-lisp
  :ensure nil
  :commands (org-babel-execute:lisp))

(use-package ob-latex
  :ensure nil
  :commands
  (org-babel-execute:latex))

(use-package ob-shell
  :ensure nil
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

;;** org-download
;; Drag and drop images to Emacs org-mode. Courtesy of abo-abo.
;; https://github.com/abo-abo/org-download.
(use-package org-download
  :after (org)
  :commands (org-download-delete org-download-clipboard org-download-rename-at-point org-download-yank org-download-screenshot org-download-image)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir (concat org-roam-directory "org-pictures/"))
  (org-download-image-latex-width 500)
  (org-download-screenshot-method "screencapture")
  (org-download-timestamp "%Y-%m-%d_%H-%M-%S_")
  :hook (dired-mode. org-download-enable))

;;** org export extensions
;; Export w/pandoc
(use-package ox-pandoc
  :if (executable-find "pandoc")
  :after ox
  :custom
  (org-pandoc-command (executable-find "pandoc"))
  (org-pandoc-options '((standalone .  t)))
  (org-pandoc-options-for-docx '((standalone . nil)))
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "lualatex")))
  (org-pandoc-format-extensions '(org+smart))
  :config
  (setq org-pandoc-menu-entry
        '(
          ;;(?0 "to jats." org-pandoc-export-to-jats)
          ;;(?0 "to jats and open." org-pandoc-export-to-jats-and-open)
          ;;(?  "as jats." org-pandoc-export-as-jats)
          ;;(?1 "to epub2 and open." org-pandoc-export-to-epub2-and-open)
          ;;(?! "to epub2." org-pandoc-export-to-epub2)
          ;;(?2 "to tei." org-pandoc-export-to-tei)
          ;;(?2 "to tei and open." org-pandoc-export-to-tei-and-open)
          ;;(?" "as tei." org-pandoc-export-as-tei)
          ;;(?3 "to markdown_mmd." org-pandoc-export-to-markdown_mmd)
          ;;(?3 "to markdown_mmd and open." org-pandoc-export-to-markdown_mmd-and-open)
          ;;(?# "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
          ;;(?4 "to html5." org-pandoc-export-to-html5)
          (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
          (?$ "as html5." org-pandoc-export-as-html5)
          (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
          (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
          ;;(?6 "to markdown_phpextra." org-pandoc-export-to-markdown_phpextra)
          ;;(?6 "to markdown_phpextra and open." org-pandoc-export-to-markdown_phpextra-and-open)
          ;;(?& "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
          ;;(?7 "to markdown_strict." org-pandoc-export-to-markdown_strict)
          ;;(?7 "to markdown_strict and open." org-pandoc-export-to-markdown_strict-and-open)
          ;;(?' "as markdown_strict." org-pandoc-export-as-markdown_strict)
          ;; (?8 "to opendocument." org-pandoc-export-to-opendocument)
          ;; (?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
          ;; (?( "as opendocument." org-pandoc-export-as-opendocument)
          ;; (?8 "to opml." org-pandoc-export-to-opml)
          ;; (?9 "to opml and open." org-pandoc-export-to-opml-and-open)
          ;; (?* "as opml." org-pandoc-export-as-opml)
          ;;(?: "to rst." org-pandoc-export-to-rst)
          ;;(?: "to rst and open." org-pandoc-export-to-rst-and-open)
          ;;(?* "as rst." org-pandoc-export-as-rst)
          ;;(?< "to slideous." org-pandoc-export-to-slideous)
          ;; (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
          ;; (?, "as slideous." org-pandoc-export-as-slideous)
          ;; (?= "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
          ;; (?- "to ms-pdf." org-pandoc-export-to-ms-pdf)
          ;;(?> "to textile." org-pandoc-export-to-textile)
          ;;(?> "to textile and open." org-pandoc-export-to-textile-and-open)
          ;;(?. "as textile." org-pandoc-export-as-textile)
          ;;(?a "to asciidoc." org-pandoc-export-to-asciidoc)
          ;;(?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
          ;;(?A "as asciidoc." org-pandoc-export-as-asciidoc)
          ;; (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
          ;; (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
          ;; (?c "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
          ;; (?C "to context-pdf." org-pandoc-export-to-context-pdf)
          ;;(?d "to docbook5." org-pandoc-export-to-docbook5)
          ;; (?d "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
          ;; (?D "as docbook5." org-pandoc-export-as-docbook5)
          ;; (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
          ;; (?E "to epub3." org-pandoc-export-to-epub3)
          ;;(?f "to fb2." org-pandoc-export-to-fb2)
          ;;(?f "to fb2 and open." org-pandoc-export-to-fb2-and-open)
          ;;(?F "as fb2." org-pandoc-export-as-fb2)
          ;;(?g "to gfm." org-pandoc-export-to-gfm)
          ;; (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
          ;; (?G "as gfm." org-pandoc-export-as-gfm)
          ;;(?h "to html4." org-pandoc-export-to-html4)
          (?h "to html4 and open." org-pandoc-export-to-html4-and-open)
          (?H "as html4." org-pandoc-export-as-html4)
          ;;(?i "to icml." org-pandoc-export-to-icml)
          ;; (?i "to icml and open." org-pandoc-export-to-icml-and-open)
          ;; (?I "as icml." org-pandoc-export-as-icml)
          ;;(?j "to json." org-pandoc-export-to-json)
          (?j "to json and open." org-pandoc-export-to-json-and-open)
          (?J "as json." org-pandoc-export-as-json)
          ;; (?k "to markdown." org-pandoc-export-to-markdown)
          (?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
          (?K "as markdown." org-pandoc-export-as-markdown)
          (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
          (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
          ;;(?m "to man." org-pandoc-export-to-man)
          ;; (?m "to man and open." org-pandoc-export-to-man-and-open)
          ;; (?M "as man." org-pandoc-export-as-man)
          ;;(?n "to native." org-pandoc-export-to-native)
          ;; (?n "to native and open." org-pandoc-export-to-native-and-open)
          ;; (?N "as native." org-pandoc-export-as-native)
          (?o "to odt and open." org-pandoc-export-to-odt-and-open)
          (?O "to odt." org-pandoc-export-to-odt)
          (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
          (?P "to pptx." org-pandoc-export-to-pptx)
          ;;(?q "to commonmark." org-pandoc-export-to-commonmark)
          ;;(?q "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
          ;;(?Q "as commonmark." org-pandoc-export-as-commonmark)
          ;;(?r "to rtf." org-pandoc-export-to-rtf)
          ;; (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
          ;; (?R "as rtf." org-pandoc-export-as-rtf)
          ;;(?s "to s5." org-pandoc-export-to-s5)
          ;;(?s "to s5 and open." org-pandoc-export-to-s5-and-open)
          ;;(?S "as s5." org-pandoc-export-as-s5)
          ;;(?t "to texinfo." org-pandoc-export-to-texinfo)
          ;;(?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
          ;;(?T "as texinfo." org-pandoc-export-as-texinfo)
          (?x "to docx and open." org-pandoc-export-to-docx-and-open)
          (?X "to docx." org-pandoc-export-to-docx)))) ;; ox-pandoc

;; Org Html Conversion
(use-package htmlize
  :commands (htmlize-buffer))

;;** Productivity
;; time tracking
(use-package org-pomodoro
  :commands (org-pomodoro ijf-org-clock-time-xbar)
  :after org
  :defer 5 ;; load after ~5 idle seconds.
  :config
  ;; integrate org-pomodoro with the xbar so I can see timer minutes remaining at all times
  ;; ref: https://colekillian.com/posts/org-pomodoro-and-polybar/
  (setq org-pomodoro-audio-player "/usr/bin/afplay")
  (setq org-pomodoro-start-sound "/Users/ilya/.config/custom-sounds/water-drip.mp3")
  (setq org-pomodoro-killed-sound "/Users/ilya/.config/custom-sounds/water-drip.mp3")
  (setq org-pomodoro-finished-sound "/Users/ilya/.config/custom-sounds/water-drip.mp3")
  (setq org-pomodoro-short-break-sound "/Users/ilya/.config/custom-sounds/water-drip.mp3")
  (setq org-pomodoro-long-break-sound "/Users/ilya/.config/custom-sounds/water-drip.mp3")
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (setq org-pomodoro-expiry-time 999999) ;; stop command from asking about resetting count
  (setq org-pomodoro-length 45)) ;; org-pomodoro

(defun my-org-clock-task ()
  (interactive)
  "If org-pomodoro is running, return the pomodoro state and time.
If org-clock is running without a pomodoro timer, return the
minutes on the current clock period. NOTE: I explicitly return
just the clock-in on the current period; not the entire
clock-time If nothing is clocked in, indicate that with a 'No
Clock' message.

Usage: I use this with xbar to display the current clock in the
menu bar on MacOS. So the message is truncated to fit on screen.
Also, server-mode must be enabled. If the server gets confused,
use 'server-force-delete' and 'server-mode' to restart."
  (cond ((org-pomodoro-active-p)
         (cl-case org-pomodoro-state
           (:pomodoro
            (format "🍅%d m: %s" (/ (org-pomodoro-remaining-seconds) 60) (string-limit org-clock-heading 20)))
           (:short-break
            (format "🍅Short: %d m" (/ (org-pomodoro-remaining-seconds) 60)))
           (:long-break
            (format "🍅Long: %d m" (/ (org-pomodoro-remaining-seconds) 60)))
           (:overtime
            (format "🍅Overtime! %d m" (/ (org-pomodoro-remaining-seconds) 60)))))
        ((and (org-clocking-p) (not (org-pomodoro-active-p)))
         (format "⏳%d m: %s" (/ (org-time-convert-to-integer (time-since org-clock-start-time)) 60) (string-limit org-clock-heading 20)))
        (t
         (format "No clock"))))

;; org-timeblock for scheduling todos
;; schedule the day
;; (use-package org-timeblock
;;   :after org)
;;** org-agenda improvements
;;*** origami -- fold agenda elements
;; useful for folding elements in org-agenda
;; I can use outline-mode for this, but this already exists and is ready to go
;; https://www.reddit.com/r/orgmode/comments/1070bi2/a_little_elisp_to_kill_lines_in_the_agenda_like/
(use-package origami
  :hook (org-agenda-mode . origami-mode)
  :bind (:map org-super-agenda-header-map
              ("<tab>" . origami-toggle-node)))



;;** org-mru-clock for better clocking into and out of tasks
(use-package org-mru-clock
  :after (org)
  ;; integrate with hammerspoon
  :custom
  (org-mru-clock-how-many 100)
  :config
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))

;;** quality of life improvements
;; make org-mode more cozy!
;; org-bookmark-heading
;;Use the standard Emacs bookmark commands, C-x r m, etc, to mark org headings
;; NOTE: will be upstreamed?
;; TODO: not working? still need?
;; https://github.com/alphapapa/org-bookmark-heading/issues/1
(use-package org-bookmark-heading)

;;* Provide org
(provide 'my-setup-org)
;;; my-setup-org.el ends here
