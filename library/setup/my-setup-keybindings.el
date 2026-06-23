;;; my-setup-keybindings.el --- keybinding configuration -*- lexical-binding: t -*-
(message "Setting up keybindings...")

;; Needed at compile time so `transient-define-prefix' is recognized as a
;; macro; otherwise the byte-compiler treats it as a function call and the
;; prefix name is miscompiled into a variable reference (void-variable at load).
(require 'transient)

;;* Global keybindings
;; buffer and tab bar navigation
(keymap-global-set "s-[" 'my-previous-user-buffer)
(keymap-global-set "s-]" 'my-next-user-buffer)
(keymap-global-set "s-{" 'tab-bar-switch-to-prev-tab)
(keymap-global-set "s-}" 'tab-bar-switch-to-next-tab)

(keymap-global-unset "s-k")
(keymap-global-set "s-K" 'kill-current-buffer)


;;** Personal Leader Key
(defvar my-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> key, for use with modal keybindings.")


;;* Meow for Modal Editing

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)

  (meow-motion-overwrite-define-key
   '("n" . meow-next)
   '("e" . meow-prev))

  ;; Set INSERT state for a few major modes. The meow documentation appears to be outdated;
  ;; the variable meow-mode-state-list can also start in insert state
  ;;https://github.com/meow-edit/meow/issues/260
  (add-to-list 'meow-mode-state-list '(org-msg-edit-mode . insert))
  (add-to-list 'meow-mode-state-list '(org-capture-mode . insert))
  (add-to-list 'meow-mode-state-list '(mu4e-compose-mode . insert))
  (add-to-list 'meow-mode-state-list '(mu4e-main-mode . insert))
  (add-to-list 'meow-mode-state-list '(mu4e-view-mode . motion))
  (add-to-list 'meow-mode-state-list '(edebug-mode . insert))
  (add-to-list 'meow-mode-state-list '(ghostel-mode . insert))

  (setq meow-selection-command-fallback
        '((meow-change . meow-change-char)
          (meow-kill . meow-c-k)
          (meow-cancel-selection . ignore)
          (meow-pop-selection . meow-pop-grab)
          (meow-beacon-change . meow-beacon-change-char)))

  (add-to-list 'meow-keymap-alist (cons 'leader my-leader-map))
  ;; Keypad prefixes hijack personal keybinds so disable them
  ;; See https://github.com/meow-edit/meow/issues/206
  (setq meow-keypad-meta-prefix nil
        meow-keypad-ctrl-meta-prefix nil
        meow-keypad-literal-prefix nil
        meow-keypad-start-keys nil)

  (meow-leader-define-key
   ;; bindings for high frequency commands
   '("<SPC>" . meow-M-x)
   '(";" . comment-line)
   '(":" . gptel-menu)
   '("a" . my-open-agenda-in-workspace)
   '("A" . my-transient-AI)
   '("b" . my-transient-buffer)
   '("B" . my-bookmarks-tmenu)
   '("c" . org-capture)
   '("C" . my-transient-config)
   '("d" . dired-recent-open)
   '("D" . dired-jump-other-window)
   '("e" . my-transient-eval)
   '("f" . my-transient-files)
   '("g" . my-transient-vc)
   '("j" . avy-goto-word-1)
   '("J" . avy-goto-char-timer)
   '("k" . consult-yank-from-kill-ring)
   '("l" . vertico-repeat)
   '("L" . my-transient-latex)
   '("M" . my-transient-email)
   '("n" . my-transient-notes)
   '("o" . my-transient-org)
   '("q" . my-transient-quit)
   '("r" . consult-register)
   '("R" . point-to-register)
   '("s" . my-transient-search)
   '("S" . my-transient-spelling)
   '("t" . my-org-capture-todo)
   '("T" . my-transient-toggle)
   '("w" . my-transient-window)
   '("W" . my-transient-workspace)
   '("z" . avy-zap-up-to-char-dwim)
   ) ;; meow-leader-define


  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("(" . backward-sentence)
   '(")" . forward-sentence)
   '("{" . backward-paragraph)
   '("}" . forward-paragraph)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   ;; g is the quick action key
   '("gg" . meow-visit)
   '("gb" . meow-pop-to-mark)
   '("gf" . meow-unpop-to-mark)
   '("gB" . pop-global-mark)
   '("g:" . jump-to-register)
   '("gr" . xref-find-references)
   '("gR" . substitute-target-in-buffer)
   '("gd" . xref-find-definitions)
   '("gD" . xref-find-definitions-other-window)

   '("G" . meow-grab)
   '("h" . meow-mark-word)
   '("H" . meow-mark-symbol)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . avy-goto-char-timer)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-left)
   '("M" . meow-left-expand)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)

   ;; (Q)uote quick bindings
   '("Qa" . embrace-add)
   '("Qc" . embrace-change)
   '("Qd" . embrace-delete)
   '("Q<" . embrace-angle)
   '("Q\(" . embrace-parens)
   '("Q\"" . embrace-double-quotes)
   '("Q'" .  embrace-single-quotes)

   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("y" . meow-save)
   '("Y" . meow-save-append)
   '("z" . meow-pop-selection)
   '(":" . gptel-menu)
   '("'" . repeat)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("<escape>" . meow-cancel-selection)))

;;** Meow
;; Note load Meow before loading personal keybindings, otherwise some might get clobbered
(use-package meow
  :config
  (setq meow-use-clipboard t)

  (setq meow-use-dynamic-face-color nil)
  (setq meow-use-cursor-position-hack t)
  (setq meow-goto-line-function 'consult-goto-line)

  ;; add additional meow things
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?a . angle))


  (meow-setup)
  (meow-global-mode 1))


;;** Personal Keybindings by Group
;;*** AI helper Keys
;; TODO: make device selection a switch (-d)
(transient-define-prefix my-transient-AI ()
  "AI commands menu"
  [["LLM"
    ("A" "Chat with GPT" gptel)
    ("a" "GPT Menu" gptel-menu)
    ("r" "Re-write" gptel-rewrite)]
   ["Whisper"
    ("d" "Transcribe Audio" whisper-run)
    ("D" "Select Audio Device" rk/select-default-audio-device)
    ("f" "Transcribe from File" whisper-file)]
   ["Claude Code"
    ("c" "Start Claude" claude-code)
    ("C" "Claude in Dir" claude-code-start-in-directory)
    ("s" "Send Region" my-claude-code-send-region-with-prompt)
    ("t" "Toggle Window" claude-code-toggle)
    ("m" "Claude Menu" claude-code-transient)]
   ["Assistants"
    ("O" "Toggle gptel-mode" (lambda ()
                               "Toggle gptel-mode in Org and Markdown buffers."
                               (interactive)
                               (when (or (derived-mode-p 'org-mode)
                                         (derived-mode-p 'markdown-mode))
                                 (gptel-mode 'toggle))))
    ]])

(transient-define-prefix my-transient-buffer ()
  "Transient menu for buffer management commands."
  ["Buffer Commands"
   ["Open/Manage"
    ("A" "Ibuffer" ibuffer)
    ("a" "Bufler List" bufler-list)
    ("b" "Bufler Switch" bufler-switch-buffer)
    ("c" "Copy Buffer to Clipboard" my-copy-whole-buffer-to-clipboard)
    ("d" "Kill Buffer & Window" kill-buffer-and-window)
    ("E" "Erase Buffer" erase-buffer)
    ("f" "Reveal in Finder" reveal-in-osx-finder)
    ("i" "Imenu" consult-imenu)
    ("w" "Copy File Name (C-u: basename)" my-copy-buffer-file-name)]
   ["Navigate"
    ("j" "Jump in Buffer" my-jump-in-buffer)
    ("k" "Kill This Buffer" kill-current-buffer)
    ("K" "Kill Other Buffers" crux-kill-other-buffers)
    ("m" "Global Mark" consult-global-mark)
    ("n" "Create New Buffer" my-create-new-buffer)
    ("N" "New Buffer in New Frame" my-new-buffer-new-frame)
    ("o" "Clone Indirect Buffer" clone-indirect-buffer-other-window)]
   ["Projects"
    ("p" "Project Buffer" consult-project-buffer)
    ("r" "Revert Buffer" revert-buffer)
    ("R" "Rename Buffer/File" crux-rename-buffer-and-file)
    ("s" "Buffer Other Window" consult-buffer-other-window)]
   ["Tabs"
    ("t" "New Tab" tab-bar-new-tab)
    ("[" "Previous User Buffer" my-previous-user-buffer)
    ("]" "Next User Buffer" my-next-user-buffer)
    ("{" "Prev Tab" tab-bar-switch-to-prev-tab)
    ("}" "Next Tab" tab-bar-switch-to-next-tab)
    ("<backtab>" "Switch to Prev Buffer" crux-switch-to-previous-buffer)]])

;;*** Config Keybindings
;; FIXME: fix kill-and-archive so that it creates an archive file
(transient-define-prefix my-transient-config ()
  "My configuration commands."
  ["Config Commands"
   ("d" "Goto emacs-dir" my-goto-emacs-dir)
   ("f" "Find emacs file" my-find-emacs-file)
   ("i" "Goto init.el" my-goto-init.el)
   ("I" "Load init file" my-load-init-file)
   ("o" "Goto org files" my-goto-org-files)
   ("s" "Search emacs files" my-search-emacs-files)])


;;*** Eval Keybindings
(transient-define-prefix my-transient-eval ()
  "My transient menu for evaluation commands."
  ["Evaluation Commands"
   ["Eval Buffer/Form"
    ("b" "Eval Buffer" eval-buffer)
    ("c" "Eval Current Form" my-eval-current-form)
    ]
   ["Eval Expression/Region"
    ("e" "Eval Last Sexp" eval-last-sexp)
    ("f" "Eval Defun" eval-defun)]])

;;*** File Keybindings
(transient-define-prefix my-transient-files ()
  "File operations transient."
  ["File Operations"
   ["Common"
    ("f" "Find File"                  find-file)
    ("l" "Locate"                     consult-locate)
    ("s" "Save Buffer"                save-buffer)
    ]
   ["Dired"
    ("C" "Copy Marked Files"          my-dired-copy-marked-files-add-date)
    ("R" "Rename Marked Files"        my-dired-rename-marked-files-add-date)
    ("w" "Change to Wdired Mode"      wdired-change-to-wdired-mode)]])



;;*** LaTeX Keybindings
(transient-define-prefix my-transient-latex ()
  "My LaTeX transient menu."
  ["LaTeX Commands"
   ["Reftex"
    ("c" "Citation" reftex-citation)
    ("r" "Reference" consult-reftex-insert-reference)
    ("g" "Goto label" consult-reftex-goto-label)
    ("t" "TOC" reftex-toc)
    ("&" "View crossref" reftex-view-crossref)]
   ["Build/View"
    ("a" "Build all" TeX-command-run-all)
    ("v" "View PDF" TeX-view)
    ("j" "Next error" TeX-next-error)
    ("k" "Kill job" TeX-kill-job)
    ("l" "Output log" TeX-recenter-output-buffer)
    ("W" "Clean aux" TeX-clean)]
   ["LaTeX"
    ("e" "Environment" LaTeX-environment)
    ("E" "Delete Environment" my-LaTeX-delete-environment)
    ("R" "Change Environment" latex-change-env)
    ("m" "Insert Macro" TeX-insert-macro)
    ("M" "Delete Macro" my-LaTeX-delete-macro)
    ("i" "Insert Item" LaTeX-insert-item)
    ("I" "Wrap as List" my-LaTeX-wrap-list)]
   ["Mark/Fill"
    ("n" "Narrow to Environment" LaTeX-narrow-to-environment)
    ("b" "Mark Inside Env" my-LaTeX-mark-inside-environment)
    ("q" "Fill Environment" LaTeX-fill-environment)]
   ["Sections"
    ("s" "Insert Section" LaTeX-section)
    ("S" "Mark Section" LaTeX-mark-section)
    ("<" "Promote" outline-promote)
    (">" "Demote" outline-demote)
    ("p" "Move Up" outline-move-subtree-up)
    ("N" "Move Down" outline-move-subtree-down)
    ("h" "Mark Subtree" outline-mark-subtree)]
   ["Folding"
    ("z" "Fold dwim" TeX-fold-dwim)
    ("f" "Fold buffer" TeX-fold-buffer)
    ("F" "Unfold buffer" TeX-fold-clearout-buffer)
    ("o" "Cycle heading" outline-cycle)
    ("O" "Cycle buffer" outline-cycle-buffer)]])


;;*** Mail Keybindings
(transient-define-prefix my-transient-email ()
  "Mu4e main menu."
  [["Main"
    ("M" "Main screen"         (lambda ()
                                 (interactive)
                                 (my-open-email-in-workspace)))
    ("U" "Update mail"         mu4e-update-mail-and-index)
    ("M-q" "Quit"             mu4e-quit)]
   ["Search"
    ("b" "Bookmark"            mu4e-search-bookmark)
    ("j" "Maildir"             mu4e-search-maildir)
    ("c" "Choose query"        mu4e-search-query)
    ("s" "Search"              mu4e-search)]
   ["Composition"
    ("C" "New"        (lambda ()
                        (interactive)
                        (my-open-email-in-workspace)
                        (mu4e-compose-new)))
    ("R" "Reply"               mu4e-compose-reply)
    ("W" "Reply-to-all"        mu4e-compose-wide-reply)
    ("F" "Forward"             mu4e-compose-forward )
    ;; only draft messages can be edited
    ("E" "Edit draft"          mu4e-compose-edit)
    ("S" "Supersede"           mu4e-compose-supersede)]
   ["Message"
    ("e" "Email to Kill Ring" my-email-to-kill-ring)
    ("d" "Send draft"         org-heading-mail-send)]
   ])


;;*** Notes (with org-roam)
(transient-define-prefix my-transient-notes ()
  "Transient menu for my notes keybindings."
  ["My Notes"
   ["Org-Roam"
    ("a" "Alias Add" org-roam-alias-add)
    ("b" "Toggle Buffer" org-roam-buffer-toggle)
    ("c" "Capture" org-roam-capture)
    ("f" "Find Node" org-roam-node-find)
    ("g" "Capture Tomorrow's Daily" org-roam-dailies-capture-tomorrow)
    ("G" "Graph" org-roam-graph)
    ("i" "Insert Node" org-roam-node-insert)
    ("j" "Capture Today's Daily" org-roam-dailies-capture-today)
    ("o" "Get/Create ID" org-id-get-create)
    ("t" "Goto Today's Daily" org-roam-dailies-goto-today)
    ("y" "Capture Yesterday's Daily" org-roam-dailies-capture-yesterday)]
   ["Consult & Citar"
    ("A" "Consult Notes" consult-notes)
    ("s" "Search in All Notes" consult-notes-search-in-all-notes)]])

;;*** Org-mode keybindings
(transient-define-prefix my-transient-org ()
  "Org Transient Menu"
  ["Org Commands"
   ["Task"
    ("A" "Archive Done Tasks" my-org-archive-done-tasks)
    ("E" "Set Effort" org-set-effort)
    ("K" "Kill Text Under Heading" my-org-kill-text-under-heading)
    ("q" "Set Tags" org-set-tags-command)]
   ["Clock"
    ("C" "Cancel" org-clock-cancel)
    ("g" "Goto" org-clock-goto)
    ("i" "Clock In" org-mru-clock-in)
    ("N" "Show Narrowed" org-mru-clock-show-narrowed)
    ("o" "Out" org-clock-out)
    ("R" "Recent" org-mru-clock-select-recent-task)]
   ["Navigation"
    ("D" "drafts.org" my-goto-drafts.org)
    ("G" "gtd.org" my-goto-gtd.org)
    ("I" "inbox.org" my-goto-inbox.org)
    ("j" "Interactive" org-goto-interactive)
    ("n" "Narrow/Widen" my-narrow-or-widen-dwim)]
   ["Org Subtrees"
    ("a" "Attach" org-attach-attach)
    ("c" "Copy Subtree" org-copy-subtree)
    ("k" "Cut Subtree" org-cut-subtree)
    ("l" "Store Link" org-store-link)
    ("L" "Grab FF Link" (lambda () (interactive)
                          (grab-mac-link-dwim 'firefox)))
    ("m" "Send from header" my-org-heading-send-dwim)]
   ["Export"
    ("ep" "Export to PDF & Open" (lambda ()
                                   (interactive)
                                   (org-open-file (org-latex-export-to-pdf))))
    ("eP" "Export to PDF" org-latex-export-to-pdf)
    ("ex" "Export to DOCX & Open" (lambda ()
                                    (interactive)
                                    (org-pandoc-export 'docx nil nil nil nil nil 0)))
    ("eX" "Export to DOCX" org-pandoc-export-to-docx)]
   ["Miscellaneous"
    ("h" "Timeblock" org-timeblock)
    ("p" "Pomodoro" org-pomodoro)
    ("f" "Agenda search" my-consult-org-ql-agenda-jump)
    ("s" "Search with org-ql" org-ql-search)
    ("S" "Org reschedule" my-org-reschedule)
    ("w" "Refile" org-refile)]
   ])


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-o") 'my-transient-org))

;;*** Quit Keybindings
(transient-define-prefix my-transient-quit ()
  "Quit operations."
  ["Quit Operations"
   ("q" "Save Buffers and Quit" save-buffers-kill-emacs)
   ("r" "Restart Emacs" restart-emacs)])



;;*** Spelling Keybindings
(transient-define-prefix my-transient-spelling ()
  "Spelling and correction commands."
  [["Spelling"
    ("b" "Check buffer" jinx-correct-all)
    ("n" "Next error" jinx-next)
    ("p" "Previous error" jinx-previous)]])

;;*** Search Keybindings
(transient-define-prefix my-transient-search ()
  "Transient menu for search commands."
  ["Search Commands"
    ("a" "Agenda" consult-org-agenda)
    ("b" "Multi Occur" consult-line-multi)
    ("d" "Ripgrep (current directory)" consult-ripgrep)
    ("D" "Search in input dir" my-search-in-input-dir)
    ("f" "Line" consult-line)
    ("F" "Affe Find" affe-find)
    ("H" "Affe Find in Home" (lambda () (interactive) (affe-find "~")))
    ("h" "Org Heading" consult-org-heading)
    ("j" "Forward/Backward Sexp" my-forward-or-backward-sexp)
    ("k" "Yank Pop" consult-yank-pop)
    ("L" "Locate" consult-locate)
    ("n" "Notes Search All" consult-notes-search-in-all-notes)
    ("r" "Query Replace" vr/query-replace)
    ("R" "Substitute Target" substitute-target-in-buffer)
    ("s" "Line Search" consult-line)
    ("W" "Affe Find in Work" (lambda () (interactive) (affe-find "~/Work")))
    ("." "Line Symbol at Point" consult-line-symbol-at-point)])

;;*** Toggle Keybindings
(transient-define-prefix my-transient-toggle ()
  "Toggle modes and settings."
  [["Display"
    ("h" "Highlight Line" hl-line-mode)
    ("e" "Empty Lines" toggle-indicate-empty-lines)
    ("l" "Line Spacing" my-toggle-line-spacing)
    ("n" "Line Numbers" display-line-numbers-mode)
    ("m" "Display Markup" my-toggle-display-markup)
    ("t" "Load Theme" consult-theme)]
   ["Editing"
    ("p" "Puni Global" puni-global-mode)
    ("P" "Show Paren" show-paren-mode)
    ("s" "Jinx" jinx-mode)
    ("S" "Jinx Correct" jinx-correct)
    ("E" "Eldoc" eldoc-mode)
    ("F" "Flymake" flymake-mode)]
   ["UI"
    ("o" "Imenu List" imenu-list-smart-toggle)
    ("O" "Olivetti" olivetti-mode)
    ("r" "Rainbow Identifiers" rainbow-identifiers-mode)
    ("z" "Zone" zone)]])

;;*** Version Control (Git) Keybindings
(transient-define-prefix my-transient-vc ()
  "My version control commands."
  [["Version Control"
    ("b" "Magit Blame"                magit-blame)
    ("c" "Magit Commit"               magit-commit)
    ("d" "Magit Diff"                 magit-diff)
    ("l" "Magit Log"                  magit-log)
    ("L" "Magit Log Buffer File"      magit-log-buffer-file)
    ("P" "Magit Pull from Pushremote" magit-pull-from-pushremote)
    ("n" "VC Next Action"             vc-next-action)
    ("s" "Magit Status"               magit-status)]])

;;*** Window Keybindings

(transient-define-prefix my-transient-window ()
  "Window management transient."
  ["Window Management"
   ["Split/Rotate"
    ("b" "Balance Windows" balance-windows)
    ("B" "Cycle 1/2 → 2/3 → 1/3" my-cycle-window-split-ratio)
    ("o" "Other Window" other-window)
    ("h" "Split & Focus Below" my-split-window-below-and-focus)
    ("H" "Split Below" split-window-below)
    ("v" "Split & Focus Right" my-split-window-right-and-focus)
    ("V" "Split Right" split-window-right)
    ]
   ["Manage"
    ("a" "Ace Window" ace-window)
    ("c" "Close Window" delete-window)
    ("m" "Maximize Window" delete-other-windows)
    ("r" "Rotate Windows" rotate-windows)
    ("R" "Rotate Windows Back" rotate-windows-back)
    ("t" "Tear Off Window" tear-off-window)
    ]
   ["Config"
    ("f" "Toggle Split" window-layout-transpose)
    ("u" "Undo Config" winner-undo)
    ("U" "Redo Config" winner-redo)
    ("x" "Exchange Buffer in Window" my-window-exchange-buffer)
    ] ])


;;*** Workspace Keybindings
(transient-define-prefix my-transient-workspace ()
  "Transient menu for workspace operations."
  ["Workspace Operations"
   ["Activity"
    ("A" "Resume" activities-resume)
    ("B" "Switch buffer" activities-switch-buffer)
    ("D" "Define" activities-define)
    ("G" "Revert" activities-revert)
    ("K" "Kill" activities-kill)
    ("L" "List" activities-list)
    ("N" "New activities" activities-new)
    ("S" "Suspend current" (lambda ()
                             (interactive)
                             (activities-suspend (activities-current))))
    ]
   ["Tabs"
    ("c" "Close" tab-close)
    ("m" "Move" my-move-tab-to)
    ("s" "Switch or create workspace" tabspaces-switch-or-create-workspace)
    ("p" "Switch project and open file" tabspaces-project-switch-project-open-file)
    ]
   ["Manage Workspaces"
    ("C" "Clear buffers" tabspaces-clear-buffers)
    ("d" "Close workspace" tabspaces-close-workspace)
    ("k" "Kill buffers and close" tabspaces-kill-buffers-close-workspace)
    ("o" "Open or create project" tabspaces-open-or-create-project-and-workspace)
    ("r" "Remove current buffer" tabspaces-remove-current-buffer)]
   ["Workspaces"
    ("a" "Agenda+Notes+Email" my-open-agenda-notes-email-in-workspaces)
    ("1" "Agenda" my-open-agenda-in-workspace)
    ("2" "Email" my-open-email-in-workspace)
    ("3" "Notes" my-open-notes-in-workspace)
    ("4" "Slack" my-open-slack-in-workspace)
    ("5" "Terminal" my-open-terminal-in-workspace)
    ]])


;;* Which Key
(use-package which-key
  :ensure nil ; built-in since Emacs 30.1
  :defer 1
  :config
  ;; hide the mode-line lighter (diminish is not installed)
  (setq which-key-lighter "")
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; Set the time delay (in seconds) for the which-key popup to appear.
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay .75)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-height 0.5)
  (setq which-key-allow-imprecise-window-fit nil)
  (setq which-key-side-window-location 'top)
  (setq which-key-separator " → ")
  (which-key-mode))

(provide 'my-setup-keybindings)
;;; my-setup-keybindings.el ends here
