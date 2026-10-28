;;; my-setup-ui.el -*- lexical-binding: t -*-

(message "Setting up Emacs core and UI...")
;;* Core Emacs settings
;;** Interface settings
(use-package emacs
  :ensure nil
  :defer 1
  :config
  ;; (Don't) Blink the cursor
  (blink-cursor-mode 0)
  ;; Skip bidi reordering for LTR-only text (perf win in large files)
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)
  ;; Defer fontification while typing (smoother scrolling/input)
  (setq redisplay-skip-fontification-on-input t)
  ;; short answers for y-or-n questions
  (setq use-short-answers t))

;; Text file settings
(use-package files
  :ensure nil
  :defer 1
  :custom
  ;; Make sure your text files end in a newline
  (require-final-newline t)
  ;; Allow large(r) files
  (large-file-warning-threshold 10000000)
  (confirm-kill-processes nil) ;; don't object when quitting
  ;; Follow symlinks
  (find-file-visit-truename t)
  ;; backups
  (backup-by-copying t)               ; don't clobber symlinks
  (version-control t)                 ; version numbers for backup files
  (delete-old-versions t)             ; delete excess backup files silently
  (kept-old-versions 2)               ; oldest versions to keep when a new numbered backup is made
  (kept-new-versions 10)              ; newest versions to keep when a new numbered backup is made
  (vc-make-backup-files t)            ; backup versioned files, which Emacs does not do by default
  :init
  ;; backups
  (let ((backup-dir (concat my-cache-dir "backup")))
    ;; Move backup file to `~/.emacs.d/temp/cache/backup'
    (setq backup-directory-alist `(("." . ,backup-dir)))
    ;; Make sure backup directory exist
    (when (not (file-exists-p backup-dir))
      (make-directory backup-dir t)))
  ;; auto save
  (setq auto-save-list-file-prefix
        (concat my-cache-dir "auto-save-list/.saves-"))
  (let ((auto-save-files-dir (concat my-cache-dir "auto-save-files/")))
    (setq auto-save-file-name-transforms
          `((".*" ,auto-save-files-dir t)))
    (when (not (file-exists-p auto-save-files-dir))
      (make-directory auto-save-files-dir t)))
  (setq create-lockfiles nil)
  :config
  ;; Auto-chmod files with shebang lines on save
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

  ;; Save file-visiting buffers after `auto-save-visited-interval' (5 s) idle.
  ;; Skip files changed on disk behind our back: auto-revert picks those up,
  ;; and saving them would prompt.
  (setq auto-save-visited-predicate #'verify-visited-file-modtime)
  (auto-save-visited-mode 1))

;; how many spaces after a period?
(use-package emacs
  :ensure nil
  ;; open files in external app quickly
  :bind (:map global-map
              ("s-o" . xah-open-in-external-app))
  :custom
  ;; Single space between sentences
  (sentence-end-double-space nil))

(use-package subword
  :ensure nil
  ;; Iterate through CamelCase words
  :hook (after-init . global-subword-mode))

(use-package simple
  :ensure nil
  :hook (after-init . global-visual-line-mode)
  :custom
  ;; reduce mark ring
  (global-mark-ring-max 256)
  (mark-ring-max 256)
  ;; After first C-u C-SPC, keep popping with just C-SPC
  (set-mark-command-repeat-pop t)
  ;; Don't store duplicate kills
  (kill-do-not-save-duplicates t))

;; line Numbers
(use-package display-line-numbers
  :ensure nil
  :commands display-line-numbers-mode
  :init
  (setq-default display-line-numbers-type 'visual)
  (setq-default display-line-numbers-width-start t))

;;* Help Setup
(use-package help
  :ensure nil
  :custom
  ;; Always focus on help window/buffer
  (help-window-select 't))

;;** Help At Point
(use-package help-at-pt
  :ensure nil
  :custom
  (help-at-pt-timer-delay 0.1)
  (help-at-pt-display-when-idle '(flymake-diagnostic)))

;;** Better Help with Helpful
;; Much better lookup both in details and headings/aesthetics
(use-package helpful
  :bind (;; Remap standard commands.
         ([remap display-local-help] . helpful-at-point)
         ([remap describe-function]  . helpful-callable)
         ([remap describe-variable]  . helpful-variable)
         ([remap describe-symbol]    . helpful-symbol)
         ([remap describe-key]       . helpful-key)
         ([remap describe-command]   . helpful-command)
         ("C-h C-l" . find-library)
         ;; Display file commentary section
         ("C-h C-c" . finder-commentary)))

;;* Indentation & Tabs
(use-package emacs
  :ensure nil
  :defer 1
  :config
  (setq-default tab-width 4)
  (setq-default fill-column 80)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-always-indent 'complete))

;;** Save history and backups
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq-default savehist-file (concat my-cache-dir "savehist"))
  (setq history-length 100)

  (put 'kill-ring 'history-length 100)
  ;; save kill ring between sessions
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring)

  ;; Strip text properties from kill-ring before saving to prevent
  ;; bloated savehist files (fonts, overlays from org-mode, etc.)
  (add-hook 'savehist-save-hook
            (lambda ()
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring))))))

;;* Transient
(use-package transient
  :ensure nil
  :demand t
  ;; <ESC> to close the transient
  :bind
  (:map transient-map
        ("<escape>" . transient-quit-one)))

;;* Fonts
;; With the following snippet, we configure the three "faces" that are
;; used to specify font families. Emacs has the concept of a "face" for a bundle
;; of text properties that include typographic properties (font family, font
;; height, font weight, …) and colours (text/foreground colour, background
;; colour).
;; other good fonts:
;; variable-pitch: Iosevka Aile
;; variable-pitch: Iosevka Aile
(let ((mono-spaced-font "Hack")
      (proportionately-spaced-font "Source Sans 3"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 200)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.2 :weight 'normal))

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.
(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;* Theme
(use-package lambda-themes
  :vc (:url "https://github.com/Lambda-Emacs/lambda-themes" :branch "main" :rev :newest)
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t)
  :config
  ;; load preferred theme
  (load-theme 'lambda-light-faded))

;;* Modeline
;; highlight the active window at the bottom
;; NOTE: this hardcoded face may conflict with theme reloads; kept intentionally
(set-face-attribute 'mode-line-active nil
                    :foreground "black" :background "goldenrod" :box '(:line-width 1 :color
                                                                                   "black"))
;; Buffer position as a nyan cat.  lambda-line renders its position segment
;; with `format-mode-line', so the (:eval ...) construct goes straight in.
;; Only `nyan-create' is needed: `nyan-mode' just patches the stock mode
;; line, and `nyan-animate-nyancat' runs a 0.2 s redraw timer.
(use-package nyan-mode
  :custom
  (nyan-bar-length 16)
  (nyan-wavy-trail t))

;; minimal and cute
;; Official repo moved to Codeberg (2026-05); PR #25 fix is merged upstream
(use-package lambda-line
  :vc (:url "https://codeberg.org/Lambda-Emacs/lambda-line" :branch "main")
  :custom
  (lambda-line-position-format '("%l:%c " (:eval (nyan-create))))
  (lambda-line-abbrev t)
  (lambda-line-position 'top)
  (lambda-line-hspace "  ")
  (lambda-line-prefix t)
  (lambda-line-prefix-padding nil)
  (lambda-line-status-invert nil)
  (lambda-line-gui-ro-symbol  " ⨂")  ;; ⬤◯⨂
  (lambda-line-gui-mod-symbol " ⬤") ;; ⨀⬤
  (lambda-line-gui-rw-symbol  " ◯")  ;; ◉ ◎ ⬤◯
  (lambda-line-vc-symbol "")
  (lambda-line-space-top 0)
  (lambda-line-space-bottom 0)
  (lambda-line-symbol-position 0)
  :custom-face
  (lambda-line-visual-bell ((t (:background "red3"))))
  (org-agenda-clocking ((t (:foreground "maroon" :underline t))))
  :config
  (lambda-line-mode)
  (lambda-line-visual-bell-config)

  ;; Show the running org clock on the right of the status line.  org-clock
  ;; already builds and truncates `org-mode-line-string' (see
  ;; `org-clock-string-limit'), but lambda-line only reads it in its
  ;; org-clock-mode format, which loses to the prog-mode/org-mode formats.
  ;; Splicing it into SECONDARY shows it in every mode, and keeps
  ;; `lambda-line-compose' aware of its width when truncating the file name.
  ;; Drop the built-in org-clock format, or the clock is shown twice.
  (setf (alist-get 'org-clock-mode lambda-line-mode-formats nil t) nil)

  (defun my-lambda-line-add-clock (args)
    "Prepend the running org clock to the SECONDARY element of ARGS.
Filter-args advice for `lambda-line-compose'."
    (if (and (fboundp 'org-clocking-p) (org-clocking-p) (> (length args) 4))
        (let ((args (copy-sequence args)))
          (setf (nth 4 args) (concat org-mode-line-string lambda-line-hspace (nth 4 args)))
          args)
      args))
  (advice-add 'lambda-line-compose :filter-args #'my-lambda-line-add-clock)

  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

;;* Windows
;; Vertical window divider
(use-package frame
  :ensure nil
  :custom
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))

;; Quickly switch windows in Emacs
(use-package ace-window
  :commands (ace-window
             ace-swap-window
             aw-flip-window)
  :config
  (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

(use-package windmove
  :ensure nil
  :commands (windmove-up
             windmove-down
             windmove-left
             windmove-right)
  :config
  ;; Default bindings conflict with org-mode
  ;; documentation: https://orgmode.org/manual/Conflicts.html
  ;; solution: re-assign windmove keybindings to hyper-<arrow>
  (windmove-default-keybindings 'hyper))

;; Winner mode is a built-in package for restoring window configurations
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))

;; Make C-x 1 reversible: press once to delete other windows,
;; press again to restore the previous layout via winner-undo
(defun my-toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'my-toggle-delete-other-windows)

;;* Frames
;;;; Frame defaults
(use-package frame
  :ensure nil
  :config
  ;; Push entries individually to preserve early-init's menu-bar-lines/tool-bar-lines entries
  (add-to-list 'default-frame-alist '(internal-border-width . 18))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
  ;; Resize pixel-wise to avoid gaps
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t)

  ;; rename frames with their major mode. This helps yabai not whig out
  (setq-default frame-title-format '("%f [" mode-name "]"))
  ;; Don't show icon in frame
  (setq-default ns-use-proxy-icon nil))

;;* Tabs
(use-package tab-bar
  :ensure nil
  :commands (tab-bar-new-tab
             tab-bar-switch-to-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-show 1)
  (tab-bar-tab-hints t) ;; show numbers in tabs
  ;; Unless another file/buffer is designated, start from workspace scratch buffer
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-tab-name-format-function #'my--tab-bar-tab-name-format)
  (tab-bar-new-button nil)
  (tab-bar-close-button nil)
  (tab-bar-auto-width nil)
  ;; Tabs of the current group show in full; other groups collapse to their name.
  (tab-bar-format '(tab-bar-format-history
                    my--tab-bar-format-tabs-groups
                    my--tab-bar-suffix
                    tab-bar-format-add-tab))
  ;; `tab-bar-format-tabs-groups' draws tabs with this instead of
  ;; `tab-bar-tab-face-function'.  The default gives ungrouped tabs
  ;; `tab-bar-tab-ungrouped', which lambda themes make nearly invisible.
  (tab-bar-tab-group-face-function #'my--tab-bar-tab-group-face)
  (tab-bar-tab-group-format-function #'my--tab-bar-tab-group-format)
  :custom-face
  (tab-bar-tab-group-inactive ((t (:inherit tab-bar-tab-inactive))))
  (tab-bar-tab-group-current ((t (:inherit tab-bar-tab))))
  :config
  (defun my--tab-bar-tab-group-face (tab)
    "Face for TAB in a grouped tab bar: the same as without groups."
    (if (bound-and-true-p activities-tabs-mode)
        (activities-tabs--tab-bar-tab-face-function tab)
      (tab-bar-tab-face-default tab)))

  ;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  (defvar my-tab-bar--circle-numbers-alist
    '((0 . "⓪")
      (1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨")
      (10 . "⑩")
      (11 . "⑪")
      (12 . "⑫")
      (13 . "⑬")
      (14 . "⑭")
      (15 . "⑮"))
    "Alist of integers to strings of circled unicode numbers.")

  (defun my--tab-bar-slots (&optional tabs)
    "Group TABS into numbered slots, as a list of ((RAW-INDEX . TAB) ...) lists.
Consecutive tabs sharing a non-nil tab group share a single slot so
companion tabs (such as `<tab>-llm') do not consume extra `s-1'..`s-0' keys."
    (let ((tabs (or tabs (funcall tab-bar-tabs-function)))
          (i 0)
          prev-group
          slots)
      (dolist (tab tabs)
        (setq i (1+ i))
        (let ((group (funcall tab-bar-tab-group-function tab)))
          (if (and group (equal group prev-group))
              (setcdr (car slots) (append (cdar slots) (list (cons i tab))))
            (push (list (cons i tab)) slots))
          (setq prev-group group)))
      (nreverse slots)))

  (defun my--tab-bar-slot-number (tab)
    "Return the 1-based slot number for TAB if it heads a slot, else nil."
    (let ((slots (my--tab-bar-slots))
          (n 1)
          found)
      (while (and slots (not found))
        (when (eq (cdar (car slots)) tab)
          (setq found n))
        (setq slots (cdr slots)
              n (1+ n)))
      found))

  (defun my--tab-bar-tab-name-format (tab _i &optional override-name)
    "Format TAB with its slot number (if it heads a slot) and truncated name.
When OVERRIDE-NAME is non-nil, display it instead of TAB's own name."
    (let* ((current-p (eq (car tab) 'current-tab))
           (slot (my--tab-bar-slot-number tab))
           (tab-num (if (and tab-bar-tab-hints slot (< slot 16))
                        (alist-get slot my-tab-bar--circle-numbers-alist)
                      ""))
           ;; Agent tabs show a status mark and the agent's short name.
           (agent (and (not override-name)
                       (fboundp 'my-agent-tab-label)
                       (my-agent-tab-label tab))))
      (propertize
       (concat
        " "
        tab-num
        (if (string-empty-p tab-num) "" (propertize " " 'display '(space :width (4))))
        (car agent)
        (truncate-string-to-width (or override-name (cdr agent) (alist-get 'name tab)) 10 nil nil t)
        (or (and tab-bar-close-button-show
                 (not (eq tab-bar-close-button-show
                          (if current-p 'non-selected 'selected)))
                 tab-bar-close-button)
            "")
        (propertize " " 'display '(space :width (4))))
       'face (funcall tab-bar-tab-face-function tab))))

  (defun my--tab-bar-tab-group-format (tab i &optional _current-p)
    "Format collapsed group TAB at index I like a normal tab with its slot number."
    (let* ((tab-bar-tab-face-function tab-bar-tab-group-face-function)
           (agent (and (fboundp 'my-agent-tab-label) (my-agent-tab-label tab))))
      (my--tab-bar-tab-name-format
       tab i (and agent (funcall tab-bar-tab-group-function tab)))))

  (defun my--tab-bar-format-tabs-groups ()
    "Produce tabs grouped by tab group without a redundant active-group title."
    (let* ((tabs (funcall tab-bar-tabs-function))
           (current-group (funcall tab-bar-tab-group-function
                                   (tab-bar--current-tab-find tabs)))
           (previous-group nil)
           (i 0))
      (mapcan
       (lambda (tab)
         (let ((tab-group (funcall tab-bar-tab-group-function tab)))
           (setq i (1+ i))
           (prog1 (cond
                   ;; Current group's tabs and ungrouped tabs: show in full.
                   ((or (equal tab-group current-group) (not tab-group))
                    (let ((tab-bar-tab-face-function tab-bar-tab-group-face-function))
                      (tab-bar--format-tab tab i)))
                   ;; First tab of an inactive group: show collapsed group tab.
                   ((not (equal previous-group tab-group))
                    (tab-bar--format-tab-group tab i)))
             (setq previous-group tab-group))))
       tabs)))

  (defun my--tab-bar-select-tab-by-slot (orig-fn &optional tab-number)
    "Select tab by slot number on `s-1'..`s-0', cycling within the slot if active."
    (if tab-number
        (funcall orig-fn tab-number)
      (let* ((key (event-basic-type last-command-event))
             (slot-num (if (and (characterp key) (>= key ?0) (<= key ?9))
                           (let ((n (- key ?0)))
                             (if (zerop n) 10 n))
                         1))
             (slots (my--tab-bar-slots))
             (slot (or (nth (1- slot-num) slots) (car (last slots))))
             (pos (seq-position slot 'current-tab
                                (lambda (entry tag) (eq (cadr entry) tag)))))
        (when slot
          (funcall orig-fn
                   (car (if (and pos (cdr slot))
                            (nth (mod (1+ pos) (length slot)) slot)
                          (car slot))))))))

  (advice-add 'tab-bar-select-tab :around #'my--tab-bar-select-tab-by-slot)


  ;; See https://github.com/rougier/nano-modeline/issues/33
  (defun my--tab-bar-suffix ()
    "Add empty space.
This ensures that the last tab's face does not extend to the end
of the tab bar."
    " "))

;;* Highlight

;;** highlight TODOS
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :custom
  (hl-todo-keyword-faces '(("zzz"   . "#FF0000")
                           ("ZZZ"   . "#FF0000")
                           ("??"   . "#FF0000")
                           ("???"   . "#FF0000")
	                       ("FIXME"  . "#FF0000")
                           ("todo"  . "#A020F0")
	                       ("TODO"  . "#A020F0"))))


;;;;; Highlight Cursor Line with Pulse
;; From https://karthinks.com/software/batteries-included-with-emacs/
;; Replace external package with internal command

(use-package pulse
  :bind
  ("C-<return>" . my-pulse-line)
  :config
  (setq pulse-delay 0.08)
  (defun my-pulse-line (&rest _)
    "Pulse the current line."
    (interactive)
    (pulse-momentary-highlight-one-line))
  ;; pulse for commands
  (dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom))
    (advice-add command :after #'my-pulse-line))
  ;; pulse on window change (covers `other-window')
  (add-hook 'window-selection-change-functions #'my-pulse-line))

;;;; Goggles (Highlight Changes)
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;;* end my-setup-ui
(provide 'my-setup-ui)
