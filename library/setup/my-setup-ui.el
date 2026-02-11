;;; my-setup-ui.el -*- lexical-binding: t -*-

(message "Setting up Emacs core and UI...")
;;* Core Emacs settings
;;** Interface settings
(use-package emacs
  :ensure nil
  :defer 1
  :config
  ;; Hide mouse cursor while typing. Why?
  ;; .. it can overlap characters we want to see.
  (setq make-pointer-invisible t)
  ;; No audible bell/alert
  (setq-default visible-bell t)
  ;; (Don't) Blink the cursor
  (blink-cursor-mode 0)
  ;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
  ;; Using `advice' here to make it easy to reverse in custom
  ;; configurations with `(advice-remove 'yes-or-no-p #'y-or-n-p)'
  ;;
  ;; short answers for y-or-n questions
  (if (boundp 'use-short-answers)
      (setq use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p)))

;; Set custom settings in a separate file in the cache-dir
(use-package cus-edit
  :ensure nil
  :defer 1
  :custom
  (custom-file (expand-file-name "custom.el" my-cache-dir))
  :config
  (when (not (file-exists-p custom-file))
    (write-file custom-file))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; Text file settings
(use-package files
  :ensure nil
  :defer 1
  :custom
  ;; Make sure your text files end in a newline
  (require-final-newline t)
  ;; Allow large(r) files
  (large-file-warning-threshold 10000000)
  (setq confirm-kill-processes nil) ;; don't object when quitting
  ;; Follow symlinks
  (setq find-file-visit-truename t))

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
  (mark-ring-max 256))

;; line Numbers
(use-package display-line-numbers
  :ensure nil
  ;; :hook (markdown-mode prog-mode)
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
  ;; :straight (:type built-in)
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

;;** Help Transient

;; A little more useful for calling help than just C-h (less info density)
;; see https://luca.cambiaghi.me/vanilla-emacs/readme.html#h:14F8ECDE-9E15-46F7-B903-ECE383251C48
;; (with-eval-after-load 'transient
;;   (bind-key (concat my-prefix " h") 'my-help-transient)
;;   (transient-define-prefix my-help-transient ()
;;     ["Help Commands"
;;      ["Mode & Bindings"
;;       ("m" "Mode" describe-mode)
;;       ("b" "Major Bindings" which-key-show-full-major-mode)
;;       ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
;;       ("d" "Descbinds" describe-bindings)
;;       ]
;;      ["Describe"
;;       ("c" "Command" helpful-command)
;;       ("f" "Function" helpful-callable)
;;       ("o" "Symbol"  helpful-symbol)
;;       ("v" "Variable" helpful-variable)
;;       ("k" "Key" helpful-key)
;;       ]
;;      ["Info on"
;;       ("C-c" "Emacs Command" Info-goto-emacs-command-node)
;;       ("C-f" "Function" info-lookup-symbol)
;;       ("C-v" "Variable" info-lookup-symbol)
;;       ("C-k" "Emacs Key" Info-goto-emacs-key-command-node)
;;       ]
;;      ["Goto Source"
;;       ("L" "Library" find-library)
;;       ("F" "Function" find-function)
;;       ("V" "Variable" find-variable)
;;       ("K" "Key" find-function-on-key)
;;       ]
;;      ]
;;     [
;;      ["Internals"
;;       ("e" "Echo Messages" view-echo-area-messages)
;;       ("l" "Lossage" view-lossage)
;;       ]
;;      ["Describe"
;;       ("s" "Symbol" helpful-symbol)
;;       ("." "At Point   " helpful-at-point)
;;       ("C-d" "Face" describe-face)
;;       ("w" "Where Is" where-is)
;;       ("=" "Position" what-cursor-position)
;;       ]
;;      ["Info Manuals"
;;       ("C-i" "Info" info)
;;       ("C-4" "Other Window " info-other-window)
;;       ("C-e" "Emacs" completing-read-info-emacs-manual)
;;       ("C-l" "Elisp" completing-read-info-elisp-manual)
;;       ]
;;      ["Exit"
;;       ("q" "Quit" transient-quit-one)
;;       ("<escape>" "Quit" transient-quit-one)
;;       ]
;;      ]
;;     [
;;      ["External"
;;       ("W" "Dictionary" dictionary-lookup-definition)
;;       ]
;;      ]
;;     ))

;;* Indentation & Tabs
(use-package emacs
  :ensure nil
  :defer 1
  :config
  (setq tab-width 4)
  (setq-default fill-column 80)
  (setq fill-column 80)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default tab-always-indent 'complete)
  ;; TAB cycle if there are only few candidates
  (setq-default completion-cycle-threshold 3))

;; UTF 8
(use-package mule-cmds
  :ensure nil
  :defer t
  :config
  ;; UTF-8 for all the things!
  (prefer-coding-system 'utf-8))


;;** Private File
;; Where to store private or "secret" info
(let ((private (expand-file-name "private.el" my-user-dir)))
  (if (file-exists-p private)
      (load-file private)))

;;** Save history and backups
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq-default savehist-file (concat my-cache-dir "savehist"))
  (when (not (file-exists-p savehist-file))
    (write-file savehist-fil))
  (setq savehist-save-minibuffer-history t)
  (setq history-length 100)
  ;; (put 'minibuffer-history 'history-length 50)

  (put 'kill-ring 'history-length 100)
  ;; save kill ring between sessions
  (add-to-list 'savehist-additional-variables 'kill-ring )
  (add-to-list 'savehist-additional-variables 'mark-ring)
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring)
  (savehist-mode 1)) ;; Save History


;;* Backups / Auto-Save
(use-package files
  :ensure nil
  :defer 1
  :init
  ;; backups
  (let ((backup-dir (concat my-cache-dir "backup")))
    ;; Move backup file to `~/.emacs.d/temp/cache/backup'
    (setq backup-directory-alist `(("." . ,backup-dir)))
    ;; Makesure backup directory exist
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
  ;; auto-save every file visiting buffer
  ;; see https://emacs.stackexchange.com/q/7729/11934
  (setq-default auto-save-default t)
  (setq-default
   auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
   auto-save-interval 300            ; number of keystrokes between auto-saves (default: 300)
   auto-save-visited-mode t
   delete-auto-save-files t
   create-lockfiles nil)
  :config
  (setq  make-backup-files t               ; backup of a file the first time it is saved.
         backup-by-copying t               ; don't clobber symlinks
         version-control t                 ; version numbers for backup files
         delete-old-versions t             ; delete excess backup files silently
         kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made
         kept-new-versions 10              ; newest versions to keep when a new numbered backup is made
         vc-make-backup-files t            ; backup versioned files, which Emacs does not do by default
         )

  (defun my-full-auto-save ()
    (interactive)
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (buffer-file-name) (buffer-modified-p))
            (basic-save-buffer)))))

  (add-hook 'auto-save-hook 'my-full-auto-save)

  ;; Save all buffers after idle time
  (run-with-idle-timer 5 t (lambda () (my-full-auto-save)))) ;; use-package files
;; traverse through the backup file list
;; never used this package, but will keep around
(use-package backup-walker
  :commands backup-walker-start)

;;* Transient
(use-package transient
  :ensure nil
  :demand t
  ;; <ESC> to close the transient
  :bind
  (:map transient-map
        ("<escape>" . transient-quit-one)))

;;* Fonts
;; With the following snippet, we configure the three “faces” that are
;; used to specify font families. Emacs has the concept of a “face” for a bundle
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

;; Remember to do all-the-icons-install-fonts
(use-package all-the-icons)

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

;; nice theme from Prot, built in, use as fallback
(use-package modus-themes
  :disabled t
  :config
  (load-theme 'modus-operandi-tinted :no-confirm-loading))

;;* Modeline
;; highlight the active window at the bottom 
(set-face-attribute 'mode-line-active nil
                    :foreground "black" :background "goldenrod" :box '(:line-width 1 :color
                                                                                   "black"))
;; minimal and cute
;; NOTE: disabled for now, as I'm back to using lambda-line
(use-package moody
  :disabled t
  :after (minions)
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  ;; hide minor modes
  (minions-mode)
  
  ;; switch modeline to the top
  (setq-default header-line-format mode-line-format)
  (setq-default mode-line-format nil))

(use-package minions
  :disabled t)

;; I generally like 
(use-package lambda-line
  ;; :vc (:url "https://github.com/Lambda-Emacs/lambda-line" :branch "main")
  :load-path "/Users/ilya/projects/lambda-line/"
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
  (lambda-line-vc-symbol "")
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

;; Hide Modeline
(use-package hide-mode-line
  :commands hide-mode-line-mode)

;;* Windows
;; Vertical window divider
(use-package frame
  :ensure nil
  :custom
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))
;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

;; Quickly switch windows in Emacs
(use-package ace-window
  :commands (ace-window
             ace-swap-window
             aw-flip-window)
  :config
  (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

(defun my-other-window ()
  (interactive)
  (other-window 1))

(use-package windmove
  :ensure nil
  :commands (windmove-up
             windmove-down
             windmove-left
             windmove-right)
  ;; :bind (("C-c C-h" . #'windmove-left)
  ;;        ("C-c C-l" . #'windmove-right)
  ;;        ("C-c C-j" . #'windmove-down)
  ;;        ("C-c C-k" . #'windmove-up))
  :config
  ;; Default bindings conflict with org-mode
  ;; documentation: https://orgmode.org/manual/Conflicts.html
  ;; solution: re-assign windmove keybindings to hyper-<arrow>
  (windmove-default-keybindings 'hyper))

;; Winner mode is a built-in package for restoring window configurations
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))


;;* Frames
;;;; Frame defaults
(use-package frame
  :ensure nil
  :config
  ;; Make a clean & minimalist frame
  ;; To modify initial frame set `initial-frame-alist`
  (setq-default default-frame-alist
                (append (list
                         '(frame-title-format . nil)
                         '(internal-border-width . 18)
                         '(tool-bar-lines . 0)
                         '(vertical-scroll-bars . nil)
                         '(horizontal-scroll-bars . nil))))
  ;; Resize pixel-wise to avoid gaps
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t)

  ;; rename frames with their major mode. This helps yabai not whig out
  (setq-default frame-title-format '("%f [" mode-name "]"))
  ;; Don't show icon in frame
  (setq-default ns-use-proxy-icon nil))

;; (Re)Center Frames
(defun my-frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

;; un/comment this hook if you want frames recentered
;; (add-hook 'after-make-frame-functions #'my-frame-recenter)
;;* Tabs
(use-package tab-bar
  :ensure nil
  :after (project)
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
        (alist-get 'name tab)
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
    " ")

  ;; https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
  (defun my-tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Otherwise use completion to select the tab."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t)))))))
;;* Bookmarks
(use-package bookmark
  :ensure nil
  :defer 2
  :config
  (setq bookmark-default-file (concat my-cache-dir "bookmarks")))
;;* Colors
;; Colorize color names in buffers
(use-package rainbow-mode
  :commands rainbow-mode)

;; for macs
(setq-default ns-use-srgb-colorspace t)

;;* Highlight

;;** Highlight Numbers
;; check if I like/need this package
(use-package highlight-numbers
  :disabled t
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;;** highlight TODOS
(use-package hl-todo
  :commands hl-todo-mode
  :hook ((prog-mode . hl-todo-mode)
         (markdown-mode . hl-todo-mode)
         (LaTeX-mode . hl-todo-mode))
  :custom
  (hl-todo-keyword-faces '(("zzz"   . "#FF0000")
                           ("ZZZ"   . "#FF0000")
                           ("??"   . "#FF0000")
                           ("???"   . "#FF0000")
	                       ("FIXME"  . "#FF0000")
                           ("todo"  . "#A020F0")
	                       ("TODO"  . "#A020F0")))
  :config
  (global-hl-todo-mode))

(use-package consult-todo
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
