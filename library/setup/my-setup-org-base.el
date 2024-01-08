;;; my-setup-org-base.el --- Org-mode settings -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein
;;; Commentary:

;; Basic setup for org-mode

;;; Code:
(message "Setting up org base settings...")

;;;; Org
;; Use Org from source rather than built in
(use-package org
  :ensure nil
  :commands (org-mode)
  :mode (("\\.org$" . org-mode))
  :hook ((org-mode . variable-pitch-mode))
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

  ;;  org speed command
  (org-use-speed-commands t)

  ;; TODOS
  (org-use-fast-todo-selection 'expert) ;; don't use popup window for todos
  ;; don't set to DONE if children aren’t DONE
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)

  :config
  (add-hook 'org-mode-hook (lambda ()  ;; don't pair < symbols
                             (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  ;; Setup further org config
  )

;;;; Org Agenda
(use-package org-agenda
  :ensure nil
  :commands (org-agenda)
  :custom
  ;; Agenda logging
  (org-agenda-start-with-log-mode t)

  ;; Agenda styling
  (org-auto-align-tags nil) ;; Don't align tags
  (org-agenda-tags-column 0) ;; Put tags next to heading
  (org-agenda-breadcrumbs-separator "  ")
  (org-agenda-block-separator " ") ;; No default block seperator
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "–––––––––––––– Now")

  ;; Display properties
  (org-agenda-tags-column org-tags-column)
  (org-agenda-show-inherited-tags nil)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)

  ;; from stack overflow https://stackoverflow.com/a/22900459/6277148
  ;; note that the formatting is nicer that just using '%b'
  (org-agenda-prefix-format
   '((agenda . " %-18c%?-10t ")
     (timeline . "  % s")
     (todo . " ")
     (tags . " ")
     (search . " %i %-12:c")))

  ;; Scheduling
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-todo-ignore-deadlines 'far)
  (org-agenda-sorting-strategy
   '((agenda time-up) (todo time-up) (tags time-up) (search time-up)))
  (calendar-week-start-day 1) ;; Start week on Monday

  ;; Agenda Custom Commands
  ;; Configure custom agenda views
  ;; https://orgmode.org/manual/Storing-searches.html#Storing-searches
  ;; https://systemcrafters.cc/emacs-from-scratch/organize-your-life-with-org-mode/

  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-agenda-span 'day)))
       (tags-todo "DEADLINE=\"<today>\""
                  ((org-agenda-overriding-header "Due Today!")))
       (tags-todo "+DEADLINE<\"<+5d>\"+DEADLINE>\"<today>\""
                  ((org-agenda-overriding-header "Due Soon")))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "email" ((org-agenda-overriding-header "Email")))
       ))

     ("n" "Next Tasks"
      ((todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))))

     ("W" "Work Tasks" tags-todo "+work")

     ;; Low-effort next actions
     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
      ((org-agenda-overriding-header "Low Effort Tasks")
       (org-agenda-max-todos 20)
       (org-agenda-files org-agenda-files)))

     ("w" "Workflow Status"
      ((todo "WAIT"
             ((org-agenda-overriding-header "Waiting on External")
              (org-agenda-files org-agenda-files)))
       (todo "REVIEW"
             ((org-agenda-overriding-header "In Review")
              (org-agenda-files org-agenda-files)))
       (todo "PLAN"
             ((org-agenda-overriding-header "In Planning")
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-files org-agenda-files)))
       (todo "BACKLOG"
             ((org-agenda-overriding-header "Project Backlog")
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-files org-agenda-files)))
       (todo "READY"
             ((org-agenda-overriding-header "Ready for Work")
              (org-agenda-files org-agenda-files)))
       (todo "ACTIVE"
             ((org-agenda-overriding-header "Active Projects")
              (org-agenda-files org-agenda-files)))
       (todo "COMPLETED"
             ((org-agenda-overriding-header "Completed Projects")
              (org-agenda-files org-agenda-files)))
       (todo "CANCELED"
             ((org-agenda-overriding-header "Cancelled Projects")
              (org-agenda-files org-agenda-files))))))))

;;;;; Agenda Jump to Dashboard
(defun my-jump-to-org-dashboard ()
  (interactive)
  (require 'org)
  (org-agenda nil "d"))

;;;;; Agenda Refresh
;; automatically refresh the agenda after adding a task
(defun my-org-agenda-refresh ()
  (interactive)
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo)
      (message "[org agenda] refreshed!"))))
(add-hook 'org-capture-after-finalize-hook 'my-org-agenda-refresh)

;;;;; Org Capture
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
         "* TODO %i %?\n Reference: %(grab-mac-link 'firefox 'org)\n:PROPERTIES:\n:Created: %U\n:END:\n"
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
         "* TODO %i%? \nLink: %a\n:PROPERTIES:\n:Created: %U\n:END:\n"
         :prepend t
         :created t
         :empty-lines 1)
        ("W"
         "Weight"
         table-line
         (file+olp "/Users/ilya/Dropbox/org-roam/20210514211724-racing_weight.org" "Weight" "Data")
         "| %U | %? | "
         :unnarrowed t)
        )
      )
;; enables meow insert mode when entering the org-capture interface. I've disabled modal editors for now.
(eval-after-load 'org-capture
  (progn
    (defun my-org-capture-turn-on-meow-insert-mode ()
      (meow-insert))
    (add-hook 'org-capture-mode-hook #'my-org-capture-turn-on-meow-insert-mode)))

;;;;; Hydra for Agenda
;; Hydra for org agenda (graciously offered by Spacemacs)
(with-eval-after-load 'org-agenda
  (defhydra my-hydra-org-agenda (:color pink :hint none)
    "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
    ;; Entry
    ("hA" org-agenda-archive-default)
    ("hk" org-agenda-kill)
    ("hp" org-agenda-priority)
    ("hr" org-agenda-refile)
    ("h:" org-agenda-set-tags)
    ("ht" org-agenda-todo)
    ;; Visit entry
    ("o"   link-hint-open-link :exit t)
    ("<tab>" org-agenda-goto :exit t)
    ("TAB" org-agenda-goto :exit t)
    ("SPC" org-agenda-show-and-scroll-up)
    ("RET" org-agenda-switch-to :exit t)
    ;; Date
    ("dt" org-agenda-date-prompt)
    ("dd" org-agenda-deadline)
    ("+" org-agenda-do-date-later)
    ("-" org-agenda-do-date-earlier)
    ("ds" org-agenda-schedule)
    ;; View
    ("vd" org-agenda-day-view)
    ("vw" org-agenda-week-view)
    ("vt" org-agenda-fortnight-view)
    ("vm" org-agenda-month-view)
    ("vy" org-agenda-year-view)
    ("vn" org-agenda-later)
    ("vp" org-agenda-earlier)
    ("vr" org-agenda-reset-view)
    ;; Toggle mode
    ("ta" org-agenda-archives-mode)
    ("tA" (org-agenda-archives-mode 'files))
    ("tr" org-agenda-clockreport-mode)
    ("tf" org-agenda-follow-mode)
    ("tl" org-agenda-log-mode)
    ("td" org-agenda-toggle-diary)
    ;; Filter
    ("fc" org-agenda-filter-by-category)
    ("fx" org-agenda-filter-by-regexp)
    ("ft" org-agenda-filter-by-tag)
    ("fr" org-agenda-filter-by-tag-refine)
    ("fh" org-agenda-filter-by-top-headline)
    ("fd" org-agenda-filter-remove-all)
    ;; Clock
    ("cq" org-agenda-clock-cancel)
    ("cj" org-agenda-clock-goto :exit t)
    ("ci" org-agenda-clock-in :exit t)
    ("co" org-agenda-clock-out)
    ;; Other
    ("q" nil :exit t)
    ("gd" org-agenda-goto-date)
    ("." org-agenda-goto-today)
    ("gr" org-agenda-redo)))

;;;; Org Contrib
(use-package org-contrib
  :ensure t
  :after org
  :config
  ;; ignore export of headlines marked with :ignore: tag
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;;;; Org Export
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

;;;; Org ID
;; Use org ids for reference
(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-id-locations-file (concat my-cache-dir ".org-id-locations"))
  (org-id-method 'ts) ;; use timestamp for id
  (org-id-link-to-org-use-id 'create-if-interactive)) ;; create ids


;;;; Org Modules
(with-eval-after-load 'org
  ;; Load additional org modules
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-tempo t)
  (add-to-list 'org-modules 'org-protocol t)
  (when sys-mac
    (add-to-list 'org-modules 'org-mac-link t)
    ;;;; Remove some org-mac-link options
    ;; disable unused linking options
    (setq org-mac-grab-Mail-app-p nil)
    (setq org-mac-grab-Outlook-app-p nil)
    (setq org-mac-grab-devonthink-app-p nil)
    (setq org-mac-grab-Together-app-p nil)
    (setq org-mac-grab-Skim-app-p nil)
    (setq org-mac-grab-qutebrowser-app-p nil)
    (setq org-mac-grab-Evernote-app-p nil))

;;;; Template expansion
;; These are invoked with C-c C-,
(setq new-structure-template-alist
      '(("el" . "src emacs-lisp")
        ("b" . "src bash")))
(dolist (ele new-structure-template-alist)
  (add-to-list 'org-structure-template-alist ele)))

;;;; TODO Keywords and tags
;; tags
(setq org-tag-alist '(("class" . ?c)
                      ("admin" . ?a)
                      ("email" . ?e)
                      ("errands" . ?r)
                      ("service" . ?s)
                      ("travel" . ?t)
                      ("writing" . ?w)))
(setq org-fast-tag-selection-single-key nil)
(customize-set-variable 'org-todo-keywords
                        '((sequence "TODO(t)" "WAITING(w)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))


;;;; Org Inline Tasks
(use-package org-inlinetask :ensure nil
  :commands org-inlinetask-insert-task)

;;;; Org Archive
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

;;;; Org Refile
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
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 8)))
  (org-refile-use-cache t)  ;; use cache for org refile
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm))

;;;; Open Files in Default Application
;;Open files in their default applications (ms word being the prime example)
(customize-set-variable 'org-file-apps
                        '(("\\.docx\\'" . default)
                          ("\\.mm\\'" . default)
                          ("\\.x?html?\\'" . default)
                          ("\\.pdf\\'" . emacs)
                          (auto-mode . emacs)))

;;;; Org Functions
;;;;; Org Emphasis Functions
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

;;;;; Narrow & Advance/Retreat
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

;;;;; Clone and Narrow
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

;;;;; Goto Org Files
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

;;;;; Export Headings as Seperate Files
;; export headlines to separate files
;; http://pragmaticemacs.com/emacs/export-org-mode-headlines-to-separate-files/ ; see also:
;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files

;; FIXME: neither of these functions work right now for some reason.
(defun my-org-export-headlines-to-docx ()
  "Export all subtrees that are *not* tagged with :noexport: to
    separate docx files.

    Subtrees that do not have the :EXPORT_FILE_NAME: property set
    are exported to a filename derived from the headline text."
  (interactive)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
              "EXPORT_FILE_NAME"
              (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-pandoc-export-to-docx nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level)))
  (shell-command-to-string "open ~/Dropbox/Work/Comments/Referee-Reports/ref-report.docx"))

(defun my-org-export-headlines-to-pdf ()
  "Export all subtrees that are *not* tagged with :noexport: to
    separate pdf files.

    Subtrees that do not have the :EXPORT_FILE_NAME: property set
    are exported to a filename derived from the headline text."
  (interactive)
  ;; (require 'ox-pandoc)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
              "EXPORT_FILE_NAME"
              (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-latex-export-to-pdf nil t nil nil '(:latex-class "org-notes"))
           ;; (org-pandoc-export-to-latex-pdf nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level))))

;;;;; Export Top Level Trees to File
;; From a useful [[https://emacs.stackexchange.com/questions/27226/how-to-export-top-level-trees-in-an-org-file-to-corresponding-files][stack exchange]] post
(defun my-org-map-entries (org-file in-tags func)
  (let ((tags (if (stringp in-tags)
                  (list in-tags)
                in-tags)))

    (with-temp-buffer
      (org-mode)
      (insert-file-contents org-file-main)

      ;; Execute func at each heading that matches tags.
      (while (< (point) (point-max))

        ;; If find a heading...
        (and (search-forward-regexp "^\* " nil "end")

             ;; ...that matches the given tags...
             (seq-reduce
              (lambda(a b) (and a b))
              (mapcar
               (lambda (tag)
                 (beginning-of-line)
                 (search-forward-regexp
                  (concat ":" tag ":") (line-end-position) "end"))
               tags)
              t)

             ;; ... then execute given function with cursor at beginning of
             ;; heading.
             (progn
               (beginning-of-line)
               (save-excursion
                 (funcall func))
               (end-of-line)))))))

;;;;; Org demote/promote region
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

;;;;; Org Hide Property Drawers
;; From [[https://www.reddit.com/r/emacs/comments/9htd0r/how_to_completely_hide_the_properties_drawer_in/e6fehiw][Reddit]]

(defun org-toggle-properties ()
  "Toggle visibility of properties in current header if it exists."
  (save-excursion
    (when (not (org-at-heading-p))
      (org-previous-visible-heading 1))
    (when (org-header-property-p)
      (let* ((a (re-search-forward "\n\\:" nil t)))
        (if (outline-invisible-p (point))
            (outline-show-entry)
          (org-cycle-hide-drawers 'all))))))

;;;;; Org Return DWIM
;; Note that i've disabled this for now as it was causing issues
;; https://gist.github.com/alphapapa/61c1015f7d1f0d446bc7fd652b7ec4fe
(defun my-org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
    A double return on an empty element deletes it. Use a prefix arg
    to get regular RET. "
  ;; See https://gist.github.com/alphapapa/61c1015f7d1f0d446bc7fd652b7ec4fe and
  ;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (if ignore
      (org-return)
    (cond ((eq 'link (car (org-element-context)))
           ;; Open links like usual
           (org-open-at-point-global))
          ((and (fboundp 'org-inlinetask-in-task-p) (org-inlinetask-in-task-p))
           ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
           ;; Johansson!
           (org-return))
          ((org-at-item-checkbox-p)
           ;; Add checkboxes
           (org-insert-todo-heading nil))
          ((and (org-in-item-p) (not (bolp)))
           ;; Lists end with two blank lines, so we need to make sure we are also not
           ;; at the beginning of a line to avoid a loop where a new entry gets
           ;; created with only one blank line.
           (if (org-element-property :contents-begin (org-element-context))
               (org-insert-heading)
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))
             (org-return)))
          ((org-at-heading-p)
           (if (s-present? (org-element-property :title (org-element-context)))
               (progn
                 (org-end-of-meta-data)
                 (org-insert-heading))
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))))
          ((org-at-table-p)
           (if (--any? (string-empty-p it)
                       (nth (- (org-table-current-dline) 1) (org-table-to-lisp)))
               (org-return)
             ;; Empty row
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))
             (org-return)))
          (t
           (org-return)))))

;; (general-define-key :keymaps 'org-mode-map "RET" #'my-org-return)

;;;;; Org Create Check Box From List Item
;; A useful macro for converting list items to checkboxes
(fset 'my-org-checkbox-from-list
      [?a ?  ?\[ ?  ?\] escape ?\M-x return])

;;;;; Org link Syntax
(defun org-update-link-syntax (&optional no-query)
  "Update syntax for links in current buffer.
Query before replacing a link, unless optional argument NO-QUERY
is non-nil."
  (interactive "P")
  (org-with-point-at 1
    (let ((case-fold-search t))
      (while (re-search-forward "\\[\\[[^]]*?%\\(?:2[05]\\|5[BD]\\)" nil t)
        (let ((object (save-match-data (org-element-context))))
          (when (and (eq 'link (org-element-type object))
                     (= (match-beginning 0)
                        (org-element-property :begin object)))
            (goto-char (org-element-property :end object))
            (let* ((uri-start (+ 2 (match-beginning 0)))
                   (uri-end (save-excursion
                              (goto-char uri-start)
                              (re-search-forward "\\][][]" nil t)
                              (match-beginning 0)))
                   (uri (buffer-substring-no-properties uri-start uri-end)))
              (when (or no-query
                        (y-or-n-p
                         (format "Possibly obsolete URI syntax: %S.  Fix? "
                                 uri)))
                (setf (buffer-substring uri-start uri-end)
                      (org-link-escape (org-link-decode uri)))))))))))


;;;;; Org Table Wrap
;; see https://emacs.stackexchange.com/a/30871/11934
(defun org-table-wrap-to-width (width)
  "Wrap current column to WIDTH."
  (interactive (list (read-number "Enter column width: ")))
  (org-table-check-inside-data-field)
  (org-table-align)

  (let (cline (ccol (org-table-current-column)) new-row-count (more t))
    (org-table-goto-line 1)
    (org-table-goto-column ccol)

    (while more
      (setq cline (org-table-current-line))

      ;; Cut current field
      (org-table-copy-region (point) (point) 'cut)

      ;; Justify for width
      (setq org-table-clip
            (mapcar 'list (org-wrap (caar org-table-clip) width nil)))

      ;; Add new lines and fill
      (setq new-row-count (1- (length org-table-clip)))
      (if (> new-row-count 0)
          (org-table-insert-n-row-below new-row-count))
      (org-table-goto-line cline)
      (org-table-goto-column ccol)
      (org-table-paste-rectangle)
      (org-table-goto-line (+ cline new-row-count))

      ;; Move to next line
      (setq more (org-table-goto-line (+ cline new-row-count 1)))
      (org-table-goto-column ccol))

    (org-table-goto-line 1)
    (org-table-goto-column ccol)))

(defun org-table-insert-n-row-below (n)
  "Insert N new lines below the current."
  (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
         (new (org-table-clean-line line)))
    ;; Fix the first field if necessary
    (if (string-match "^[ \t]*| *[#$] *|" line)
        (setq new (replace-match (match-string 0 line) t t new)))
    (beginning-of-line 2)
    (setq new
          (apply 'concat (make-list n (concat new "\n"))))
    (let (org-table-may-need-update) (insert-before-markers new))  ;;; remove?
    (beginning-of-line 0)
    (re-search-forward "| ?" (point-at-eol) t)
    (and (or org-table-may-need-update org-table-overlay-coordinates) ;;; remove?
         (org-table-align))
    (org-table-fix-formulas "@" nil (1- (org-table-current-dline)) n)))

;;;;; Org Export Last Subtree
;; bind f5 to keyboard macro of export-last-subtree
(fset 'export-last-subtree
      "\C-u\C-c\C-e")

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "<f5>") 'export-last-subtree)))


;;;;; Org Tag Selection

(defun my-org-select-tags-completing-read ()
  "Select tags to add to headline."
  (interactive)
  (let* ((current (org-get-tags (point)))
         (selected (completing-read-multiple "Select org tag(s): " (org-get-buffer-tags))))
    (alet (-uniq (append (-difference current selected)
                         (-difference selected current)))
      (org-set-tags it))))

;;;;; Org Copy Link
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

;;;;; Remove Org Links
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

;;;;; Uncheck Org boxes
;;see https://www.reddit.com/r/emacs/comments/r107bg/comment/hlx54vf/?utm_source=share&utm_medium=web2x&context=3
(defun my-copy-and-uncheck (start end)
  "copy a region of regularly repeating checkbox items forward from
one week to the next, unchecking them at the same time"
  (interactive "r")
  (kill-new (replace-regexp-in-string (rx "[X]") "[ ]" (buffer-substring start end)))
  (setq deactivate-mark t))

;;;;; Org Tree/Heading to New File
(defun my-org-tree-to-new-file ()
  (interactive)
  "Move an org subtree to a new file"
  (org-copy-subtree nil t)
  (find-file-other-window
   (read-file-name "Move subtree to file:" ))
  (org-paste-subtree))

;;;;; Org Wrap in Block Template
;; A helpful function I found for wrapping text in a block template.
;; http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-block-wrap ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(
                      ("a" . "ascii")
                      ("c" . "comment")
                      ("C" . "center")
                      ("e" . "example")
                      ("E" . "src emacs-lisp")
                      ("h" . "html")
                      ("l" . "laTeX")
                      ("n" . "notes")
                      ("q" . "quote")
                      ("s" . "src")
                      ("v" . "verse")
                      ))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+end_" choice "\n")
                (goto-char start)
                (insert "#+begin_" choice "\n")))
             (t
              (insert "#+begin_" choice "\n")
              (save-excursion (insert "#+end_" choice))))))))))


;;;;; Org Export Body to HTML Buffer
(defun my-org-export-to-buffer-html-as-body (&optional async subtreep visible-only body-only ext-plist)
  "Export org buffer body to html"
  (interactive)
  (org-export-to-buffer 'html "*Org HTML Export*"
    async body-only ext-plist (lambda () (html-mode)))
  (my-copy-whole-buffer-to-clipboard)
  (delete-windows-on "*Org HTML Export*")
  (message "HTML copied!"))
;; (my-previous-user-buffer))

;;;;; Attaching files
(defun org-attach-save-file-list-to-property (dir)
  "Save list of attachments to ORG_ATTACH_FILES property."
  (when-let* ((files (org-attach-file-list dir)))
    (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", "))))

(add-hook 'org-attach-after-change-hook #'org-attach-save-file-list-to-property)

;;;;; Reschedule
(defun my-org-reschedule ()
  "Re-schedule an org item by first clering the scheduled field and then interactively prompting for a new date/time."
  (interactive)
  (org-schedule '(4)) ;; remove any scheduled items
  (org-schedule 0)) ;; schedule

;;;;; Links
;; https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
(defun my-org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
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

;;;;; Hooks
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

;;; Provide Org-Base
(provide 'my-setup-org-base)
;;; my-setup-org-base.el ends here
