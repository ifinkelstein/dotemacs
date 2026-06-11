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
  (make-backup-files t)               ; backup of a file the first time it is saved.
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
  (defun my-full-auto-save ()
    "Save all file-visiting buffers silently.
Skip buffers whose file changed on disk (let auto-revert handle those)."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (buffer-modified-p)
                   ;; Only save if file on disk hasn't changed behind our back.
                   ;; When it has, auto-revert-mode will pick up the new version.
                   (verify-visited-file-modtime buf))
          (condition-case err
              (basic-save-buffer)
            (error
             (message "Auto-save failed for %s: %s"
                      (buffer-name buf) (error-message-string err))))))))

  ;; Auto-chmod files with shebang lines on save
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

  ;; Save all buffers after idle time
  (run-with-idle-timer 5 t #'my-full-auto-save))

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
  ;; move via visual lines
  (line-move-visual t)
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
  (setq-default tab-always-indent 'complete)
  ;; TAB cycle if there are only few candidates
  (setq-default completion-cycle-threshold 3))

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
  (add-to-list 'savehist-additional-variables 'mark-ring)
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

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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
;; minimal and cute
;; TODO: revert to upstream once https://github.com/Lambda-Emacs/lambda-line/pull/25 is merged
;; :vc (:url "https://github.com/Lambda-Emacs/lambda-line" :branch "main")
(use-package lambda-line
  :vc (:url "https://github.com/ifinkelstein/lambda-line" :rev "fix/quoted-face-refs")
  :custom
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
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-tab-name-format-function #'my--tab-bar-tab-name-format)
  (tab-bar-new-button nil)
  (tab-bar-close-button nil)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs
                    my--tab-bar-suffix
                    tab-bar-format-add-tab))
  :config
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
  (defun my--tab-bar-tab-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab))
          (tab-num (if (and tab-bar-tab-hints (< i 16))
                       (alist-get i my-tab-bar--circle-numbers-alist) "")))
      (propertize
       (concat
        " "
        tab-num
        (propertize " " 'display '(space :width (4)))
        (truncate-string-to-width (alist-get 'name tab) 10 nil nil t)
        (or (and tab-bar-close-button-show
                 (not (eq tab-bar-close-button-show
                          (if current-p 'non-selected 'selected)))
                 tab-bar-close-button)
            "")
        (propertize " " 'display '(space :width (4))))
       'face (funcall tab-bar-tab-face-function tab))))


  ;; See https://github.com/rougier/nano-modeline/issues/33
  (defun my--tab-bar-suffix ()
    "Add empty space.
This ensures that the last tab's face does not extend to the end
of the tab bar."
    " "))

;;* Colors
;; Colorize color names in buffers
(use-package rainbow-mode
  :commands rainbow-mode)

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

(use-package consult-todo
  :commands (consult-todo)
  :after (hl-todo consult))



;;;;; Highlight Cursor Line with Pulse
;; From https://karthinks.com/software/batteries-included-with-emacs/
;; Replace external package with internal command

(use-package pulse
  :bind
  ("C-<return>" . pulse-line)
  :commands (pulse-line pulse-momentary-highlight-one-line)
  :config
  (setq pulse-delay 0.08)
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (interactive)
    (pulse-momentary-highlight-one-line (point)))
  ;; pulse for commands
  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))
  ;; pulse on window change
  (push 'pulse-line window-selection-change-functions))

;;;; Goggles (Highlight Changes)
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;;* end my-setup-ui
(provide 'my-setup-ui)
