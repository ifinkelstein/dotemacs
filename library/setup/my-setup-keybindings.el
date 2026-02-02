;; my-setup-keybindings.el --- summary -*- lexical-binding: t -*-
(message "Setting up keybindings...")

;;* Global keybindings
;; buffer and tab bar navigation
(global-set-key (kbd "s-[") 'my-previous-user-buffer)
(global-set-key (kbd "s-]") 'my-next-user-buffer)
(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)

(keymap-global-unset "s-k")
(keymap-global-set "s-K" 'my-kill-this-buffer)


;;** Personal Keybindings Prefix
(defcustom my-prefix "C-c C-SPC"
  "Prefix for all personal keybinds."
  :type 'string
  :group 'my-emacs)

;;** Personal Leader Key
(defcustom my-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> key, for use with modal keybindings."
  :type 'string
  :group 'my-emacs)


;;* Meow for Modal Editing

;; Set leader This isn't the sanctioned way to do this, but it seems to be the
;; only way to get `leader' to properly display keys from
;; `meow-leader-define-key' and my personal keymap in `lem+leader-map' I think
;; the preferred way is via (setq meow-keypad-leader-dispatch "...") but
;; that doesn't work as i want it to
;; (add-to-list 'meow-keymap-alist (cons 'leader lem+leader-map))


(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh) ;;used to be: meow-cheatsheet-layout-qwerty
  ;; (setq meow-keypad-leader-dispatch "C-c <SPC>")

  (meow-motion-overwrite-define-key
   '("s-[" . my-previous-user-buffer)
   '("s-]" . my-next-user-buffer)
   '("s-{" . tab-bar-switch-to-prev-tab)
   '("s-}" . tab-bar-switch-to-next-tab)
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
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))

  (setq meow-selection-command-fallback
        '((meow-change . meow-change-char)
          (meow-kill . meow-c-k)
          ;; (meow-kill . meow-delete) ;; new val, see how I like it
          ;; (meow-cancel-selection . keyboard-quit)
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
   '("<SPC>" . meow-M-x) ;; does this make C-c spc spc the user leader key?
   ;; '("?" . consult-apropos)
   ;; high frequency keybindings
   ;; '(")" . "C-)")
   ;; '("}" . "C-}")
   ;; '("." . "M-.")
   ;; '("[" . cpm/previous-user-buffer)
   ;; '("]" . cpm/next-user-buffer)
   ;; '("{" . tab-bar-switch-to-prev-tab)
   ;; '("}" . tab-bar-switch-to-next-tab)
   ;; '("TAB" . cpm/tab-bar-select-tab-dwim)
   ;; '("SPC" . execute-extended-command)
   '(";" . comment-line)
   '(":" . gptel-menu)
   ;; '("/" . meow-keypad-describe-key)
   ;; '("=" . hl-line-mode)
   ;; '("'" . embark-act)
   ;; '("\"" . embark-dwim)
   '("a" . my-open-agenda-in-workspace)
   '("A" . my-transient-AI)
   '("b" . my-transient-buffer)
   '("c" . my-transient-comment)
   '("c" . org-capture)
   '("C" . my-transient-config)
   ;; ;; '("d" . dired-jump)
   '("d" . dired-recent-open)
   '("D" . dired-jump-other-window)
   '("e" . my-transient-eval)
   '("f" . my-transient-files)
   ;; '("F" . my+flycheck-keys)
   '("g" . my-transient-vc)
   '("j" . avy-goto-word-1)
   '("J" . avy-goto-char-timer)
   '("k" . consult-yank-from-kill-ring)
   '("l" . vertico-repeat)
   '("L" . my-transient-latex)
   '("M" . my-transient-email)
   '("n" . my-transient-notes)
   ;; '("N" . consult-notes-search-in-all-notes)
   '("o" . my-transient-org)
   '("q" . my-transient-quit)
   '("r" . consult-register)
   '("R" . point-to-register)
   '("s" . my-transient-search)
   '("S" . my-transient-slack)
   ;; '("S" . cpm/search-in-input-dir)
   '("t" . my-org-capture-todo)
   ;; '("T" . my+toggle-keys)
   ;; '("u" . my+user-keys)
   ;; '("v" . my-transient-vc)
   ;; '("V" . multi-vterm-dedicated-toggle)
   '("w" . my-transient-window)
   '("W" . my-transient-workspace)
   ;; '("W" . my+workspace-keys)
   ;; '("y" . yas-minor-mode-map)
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
   
   '("gl" . meow-goto-line)
   ;; '("gc" . avy-goto-char-timer)
   ;; '("gC" . avy-goto-char)
   '("g:" . jump-to-register)
   '("gr" . xref-find-references)
   '("gR" . substitute-target)
   '("gd" . xref-find-definitions)
   '("gD" . xref-find-definitions-other-window)
   '("gs". meow-search)

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

   ;; (Q)uote quick bindgs
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
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("Y" . meow-save-append)
   '("z" . meow-pop-selection)
   '(":" . gptel-menu)
   '("'" . repeat)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("=" . meow-grab)
   '("s-[" . my-previous-user-buffer)
   '("s-]" . my-next-user-buffer)
   '("s-{" . tab-bar-switch-to-prev-tab)
   '("s-}" . tab-bar-switch-to-next-tab)
   '("<escape>" . meow-cancel-selection)))

;;** Meow
;; Note load Meow before loading personal keybindings, otherwise some might get clobbered
(use-package meow
  :ensure t
  :config
  ;; set colors in bespoke theme
  ;; use system keyboard
  (setq meow-use-clipboard t)

  (setq meow-use-dynamic-face-color nil)
  (setq meow-use-cursor-position-hack t)
  ;; Make sure delete char means delete char
  ;; see https://github.com/meow-edit/meow/issues/112
  ;; (setq meow--kbd-delete-char "<deletechar>")

  (setq meow-goto-line-function 'consult-goto-line)

  ;; add additional meow things
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?a . angle))


  ;; TODO: write a function to mark org blocks of all kinds for meow
  ;; (meow-thing-register 'block '(regexp "#+begin_" "#+end_") '((regexp "#+begin_" "#+end_")))
  ;; (add-to-list 'meow-char-thing-table '(?B . block))

  ;; TODO: write a function to mark latex environments

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
    ;; ("-r" "Refine" :class 'transient-toggle
    ;;  :variable 'my-whisper-do-refine :format "Refine: %-3s")

    ;; Suffix commands for actions
    ("d" "Transcribe Audio" whisper-run)
    ("D" "Select Audio Device" rk/select-default-audio-device)
    ("f" "Transcribe from File" whisper-file)]
   
   ["Assistants"
    ("o" "gptel-aibo" gptel-aibo)
    ("O" "Toggle gptel-mode" (lambda ()
                               (interactive)
                               "Enable gptel-mode in Org and Markdown buffers."
                               (when (or (derived-mode-p 'org-mode)
                                         (derived-mode-p 'markdown-mode))
                                 (gptel-mode))))
    ]
   ["Process"
    ;; ("b" "Process Message" my-gptel-process-message)
    ;; ("i" "Create Calendar Event" my-gptel-ical-from-message)
    ;; ("R" "Reply to Message" my-gptel-reply-to-message)
    ]
   ])

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
    ("i" "Imenu" consult-imenu)]
   ["Navigate"
    ("j" "Jump in Buffer" my-jump-in-buffer)
    ("k" "Kill This Buffer" my-kill-this-buffer)
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

;;*** Comment Keybindings
(transient-define-prefix my-transient-comment ()
  "Comment and wrap commands."
  [["Comment"
    ("c" "Comment DWIM" comment-dwim)
    ("d" "Duplicate & Comment" crux-duplicate-and-comment-current-line-or-region)
    ("l" "Comment Line" comment-line)]
   ["Wrap"
    ("o" "Org Block Wrap" org-insert-structure-template)
    ;; ("y" "YAML Wrap" my-yaml-wrap)
    ]])

;;*** Config Keybindings
;; FIXME: fix kill-and-archive so that it creates an archive file
(transient-define-prefix my-transient-config ()
  "My configuration commands."
  ["Config Commands"
   ;; ("c" "Goto custom.el" my-goto-custom.el)
   ("d" "Goto emacs-dir" my-goto-emacs-dir)
   ;; ("e" "Goto early-init.el" my-goto-early-init.el)
   ("f" "Find emacs file" my-find-emacs-file)
   ;; ("K" "Delete byte compiled files" my-delete-byte-compiled-files)
   ;; ("l" "Load config" my-load-config)
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
    ("f" "Eval Defun" eval-defun)
    ;; ("r" "Eval Last Region" eval-last-region)
    ]])

;;*** File Keybindings
(transient-define-prefix my-transient-files ()
  "File operations transient."
  ["File Operations"
   ["Common"
    ;; ("b" "Bookmark"                   consult-bookmark)
    ("f" "Find File"                  find-file)
    ("l" "Locate"                     consult-locate)
    ;; ("o" "Open with"                  crux-open-with)
    ("s" "Save Buffer"                save-buffer)
    ;; ("r" "Recent File"                consult-recent-file)
    ;; ("y" "Show and Copy Filename"     my-show-and-copy-buffer-filename)
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
    ("r" "Reference" reftex-reference)
    ("t" "TOC" reftex-toc)]
   ["LaTeX"
    ("e" "Environment" LaTeX-environment)
    ("E" "Delete Environment" my-LaTeX-delete-environment)
    ("m" "Insert Macro" TeX-insert-macro)
    ("M" "Delete Macro" my-LaTeX-delete-macro)
    ("n" "Narrow to Environment" LaTeX-narrow-to-environment)]
   ["Folding"
    ("f" "Fold buffer" TeX-fold-buffer)
    ("F" "Unfold buffer" TeX-fold-clearout-buffer)
    ]])


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
    ("S" "Supersede"           mu4e-compose-supersede)
    ;; ("X" "Resend"          mu4e-compose-resend)
    ]
   ["Message"
    ("e" "Email to Kill Ring" my-email-to-kill-ring)]
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
    ;; ("F" "Roam RG Search" bms/org-roam-rg-search)
    ("g" "Capture Tomorrow's Daily" org-roam-dailies-capture-tomorrow)
    ("G" "Graph" org-roam-graph)
    ("i" "Insert Node" org-roam-node-insert)
    ("j" "Capture Today's Daily" org-roam-dailies-capture-today)
    ;; ("n" "New File Named" org-roam--new-file-named)
    ("o" "Get/Create ID" org-id-get-create)
    ("t" "Goto Today's Daily" org-roam-dailies-goto-today)
    ("y" "Capture Yesterday's Daily" org-roam-dailies-capture-yesterday)]
   ["Consult & Citar"
    ("A" "Consult Notes" consult-notes)
    ;; ("C" "Citar Open Notes" citar-open-notes)
    ;; ("r" "Citar Open Notes" citar-open-notes)
    ;; ("R" "Cited Roam Notes" consult-notes-org-roam-cited)
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
    ("m" "Email from header" org-heading-mail-send)]
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
;; TODO:add grammar and other writing support
(transient-define-prefix my-transient-writing ()
  "Spelling, Grammar, and other support"
  [["Spelling"
    ("b" "Check Buffer" ispell-buffer)
    ("B" "Consult Flyspell" consult-flyspell)
    ("n" "Correct Next" flyspell-correct-next)
    ("p" "Correct Previous" flyspell-correct-previous)]
   ["Grammar"
    ;; TODO: add transient for this
    ]])


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
    ;; ("S" "Flyspell Next Error" my-flyspell-ispell-goto-next-error)
    ("W" "Affe Find in Work" (lambda () (interactive) (affe-find "~/Work")))
    ("." "Line Symbol at Point" consult-line-symbol-at-point)])

;;*** Toggle Keybindings
(transient-define-prefix my-transient-toggle ()
  "Toggle modes and settings."
  [["Toggle Modes"
    ("b" "Buffer Line" buffer-line-mode)
    ("g" "Git Gutter" git-gutter-mode)
    ("h" "Highlight Line" hl-line-mode)
    ("H" "Hidden Mode Line" hidden-mode-line-mode)
    ("e" "Empty Lines" toggle-indicate-empty-lines)
    ("E" "Eldoc" eldoc-mode)
    ("F" "Flymake" flymake-mode)
    ("l" "Line Spacing" my-toggle-line-spacing)
    ("m" "Display Markup" my-toggle-display-markup)
    ("n" "Line Numbers" display-line-numbers-mode)
    ("N" "Org Numbers Overlay" org-numbers-overlay-mode)
    ("o" "Imenu List" imenu-list-smart-toggle)
    ("O" "Olivetti" olivetti-mode)
    ("p" "Puni Global" puni-global-mode)
    ("P" "Show Paren" show-paren-mode)
    ("r" "Rainbow Identifiers" rainbow-identifiers-mode)
    ("s" "Flyspell" flyspell-mode)
    ("S" "Flyspell Correct" flyspell-correct-wrapper)
    ("t" "Dark/Light Theme" toggle-dark-light-theme)
    ("T" "Load Theme" my-load-theme)
    ("z" "Zone" zone)
    ]])

;;*** Version Control (Git) Keybindings
(transient-define-prefix my-transient-vc ()
  "My version control commands."
  [["Version Control"
    ("b" "Magit Blame"                magit-blame)
    ("c" "Magit Commit"               magit-commit)
    ("d" "Magit Diff"                 magit-diff)
    ("l" "Magit Log"                  magit-log)
    ("L" "Magit Log Buffer File"      magit-log-buffer-file)
    ;; ("n" "Next Hunk"                  git-gutter:next-hunk)
    ;; ("p" "Previous Hunk"              git-gutter:previous-hunk)
    ("P" "Magit Pull from Pushremote" magit-pull-from-pushremote)
    ("n" "VC Next Action"             vc-next-action)
    ;; ("r" "Magit Reflog"               magit-reflog)
    ("s" "Magit Status"               magit-status)]])

;;*** Window Keybindings

(transient-define-prefix my-transient-window ()
  "Window management transient."
  ["Window Management"
   ["Split/Rotate"
    ("b" "Balance Windows" balance-windows)
    ("o" "Other Window" my-other-window)
    ("h" "Split & Focus Below" my-split-window-below-and-focus)
    ("H" "Split Below" split-window-below)
    ("v" "Split & Focus Right" my-split-window-right-and-focus)
    ("V" "Split Right" split-window-right)
    ]
   ["Manage"
    ("a" "Ace Window" ace-window)
    ("c" "Close Window" delete-window)
    ("m" "Maximize Window" delete-other-windows)
    ("r" "Rotate Windows" my-rotate-windows)
    ("R" "Rotate Windows Back" my-rotate-windows-backward)
    ("t" "Tear Off Window" tear-off-window)
    ]
   ["Config"
    ("f" "Toggle Split" my-toggle-window-split)
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
    ;; ("b" "Switch to buffer" tabspaces-switch-to-buffer)
    
    ]
   ["Tabs"
    ("c" "Close" tab-close)
    ("m" "Move" my-move-tab-to)
    ("s" "Switch or create workspace" tabspaces-switch-or-create-workspace)
    ("p" "Switch project and open file" tabspaces-project-switch-project-open-file)
    ]
   ["Manage Workspaces"
    ("c" "Clear buffers" tabspaces-clear-buffers)
    ("d" "Close workspace" tabspaces-close-workspace)
    ("k" "Kill buffers and close" tabspaces-kill-buffers-close-workspace)
    ("o" "Open or create project" tabspaces-open-or-create-project-and-workspace)
    ("r" "Remove current buffer" tabspaces-remove-current-buffer)]
   ["Workspaces"
    ("1" "Agenda" my-open-agenda-in-workspace)
    ("2" "Email" my-open-email-in-workspace)
    ("3" "Notes" my-open-notes-in-workspace)
    ("4" "Notes" my-open-terminal-in-workspace)
    ]])


;;* Which Key
(use-package which-key
  :defer 1
  :diminish ""
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; Set the time delay (in seconds) for the which-key popup to appear.
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay .75)
  (setq which-key-idle-secondary-delay 0.05)
  ;; use widow
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-height 0.5)
  (setq which-key-allow-imprecise-window-fit nil)
  (setq which-key-side-window-location 'top)
  ;; use minibuffer
  ;; (which-key-setup-minibuffer)
  ;; separator
  (setq which-key-separator " → ")
  (which-key-mode))

;;* End keybindings
(provide 'my-setup-keybindings)

